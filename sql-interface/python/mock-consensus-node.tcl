#!/usr/bin/tclsh

# mock of the consensus node.
# it is written in Tcl because it is safer than Python
# and has better TCP/IP support.
# 

package require Thread
package require sqlite3

set current_requests [list ]
set requests_performed [list ]

proc height {} {
	global requests_performed
	return [llength $requests_performed]
}

proc new_height {} {
	global current_requests requests_performed
	lappend requests_performed $current_requests
	database eval {COMMIT;}
	database eval {BEGIN TRANSACTION;}
	set current_requests [list ]
}

proc sql_str {str} {
	set str [string map {' ''} $str]
	return '$str'
}

proc try_eval_request {zz_request_sql zz_request_params} {
	foreach {param value} $zz_request_params {
		# good enough for mockup
		set $param $value
	}
	if {[catch {database eval $zz_request_sql} zz_reason]} {
puts "EVAL FAILED: $zz_reason"
		return 0
	}
	return 1
}

proc try_add_request {request_sql request_params} {
	global current_requests
	if {[string length $request_sql] < 1} {
puts "NO-OP REQUEST"
		return
	}
	if {[catch {set ids_list [database eval {SELECT request_id FROM allowed_requests WHERE request_text = :request_sql;}]} reason]} {
puts "WRONG SQL: $request_sql"
puts "   REASON: $reason"
		return
	}
	if {[llength $ids_list] != 1} {
puts "WRONG IDS LIST LENGTH: [list $ids_list]"
		return
	}
	puts "TODO: THE REAL THING NEEDS TO VALIDATE PARAMS!"
	set request_id [lindex $ids_list 0]
	# https://sqlite.org/lang_savepoint.html
	database eval {SAVEPOINT savepoint_to_check_request;}
	if {![try_eval_request $request_sql $request_params]} {
		database eval {ROLLBACK TO savepoint_to_check_request;}
	} else {
puts "REQUEST PASS"
		database eval {RELEASE savepoint_to_check_request;}
		set seq_index [llength $current_requests]
		set h [height]
		lappend current_requests $request_sql $request_params
		lappend current_requests "INSERT INTO serialized_requests (height, seq_index, request_id) VALUES ($h, $seq_index, [sql_str $request_id]);"
		lappend current_requests [list ]
		foreach {param_name param_value} $request_params {
			lappend current_requests "INSERT INTO serialized_requests_params (height, seq_index, request_id, request_param_name, request_param_value) VALUES ($h, $seq_index, [sql_str $request_id], [sql_str $param_name], [sql_str $param_value]);"
			lappend current_requests [list ]
		}
	}
}

proc ascend_timeout {} {
	return 10000
}

proc ascend_action {} {
	new_height
	after [ascend_timeout] ascend_action
}

proc request_response {pubkey client_height request parameters} {
	global current_requests requests_performed
	try_add_request $request $parameters
	set response [list [height]]
	if {$client_height < 0} {
		foreach genesis_request [database eval {SELECT request_sql FROM serialized_genesis_requests ORDER BY seq_index;}] {
			lappend response $genesis_request
			lappend response ""
		}
		set client_height 0
	}
	foreach reqs_list [lrange $requests_performed 0 end] {
		foreach {served_request served_request_params} $reqs_list {
			lappend response $served_request
			foreach {p v} $served_request_params {
				lappend response $p
				lappend response $v
			}
			lappend response ""
		}
	}
	return $response
}

proc _accept_connection {socket address port} {
	puts "[thread::id]: accepted connection from $address, port $port"
	set thread_script {
	package require Thread
	# the procedure that reads requests from specified socket
	# and calls (thread::send) main thread to take action.
	proc read_requests {socket main_id} {
		catch {
			while {![eof $socket]} {
				set pubkey [gets $socket]
				set height [gets $socket]
				set request [gets $socket]
				if {[string length $pubkey] < 1 || [string length $height] < 1} {
					break
				}
puts "[thread::id]: pubkey $pubkey, height $height, request '$request'"
				# clear parameters dictionary.
				array unset parameters
				array set parameters {}
				if {[string length $request]} {
					while 1 {
						set param_name [gets $socket]
						if {![string length $param_name]} {
							break
						}
						set param_value [gets $socket]
						set parameters($param_name) $param_value
					}
				}
				set parameters(user_id) $pubkey
				if {[thread::send $main_id [list request_response $pubkey $height $request [array get parameters]] response]} {
					puts "[thread::id] error executing request: '$response'"
					set response [list]
				}
				foreach r $response {
					#puts "[thread::id] sending: '$r'"
					puts $socket $r
				}
				puts $socket ""
				flush $socket
			}
		}
		catch {close $socket}
		thread::release
	}
}
	set other_id [thread::create]
	set our_id [thread::id]
	thread::transfer $other_id $socket
	thread::send -async $other_id "$thread_script\n\nread_requests $socket $our_id"
}

proc accept_connection {socket address port} {
	puts "current threads running: [thread::names]"
	after idle [list _accept_connection $socket $address $port]
}

proc main {argv0 argv} {
	if {[llength $argv] != 2} {
		puts "usage: $argv0 port-number \"command to populate tables\""
		exit 1
	}
	lassign $argv port mandatory_tables_command
	puts "getting mandatory tables content..."
	set mandatory_tables [eval exec $mandatory_tables_command]
	set databasefn "bububu.db"
	file delete -force $databasefn
	sqlite3 database $databasefn -create yes
	puts "database created"
	database eval {BEGIN TRANSACTION;}
	database eval $mandatory_tables
	database eval {COMMIT;}
	puts "tables added."
	database eval {BEGIN TRANSACTION;}
	after [ascend_timeout] ascend_action
	puts "accepting connections."
	socket -server accept_connection $port
}

main $argv0 $argv

thread::wait
