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
	set current_requests [list ]
}

proc ascend_action {} {
	new_height
	after 1000 ascend_action
}

proc _accept_connection {socket address port} {
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
puts "[thread::id]: got pubkey '$pubkey', height '$height' and request '$request'"
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
					puts "[thread::id] sending: '$r'"
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

proc request_response {pubkey client_height request parameters} {
	set response [list [height]]
	if {$client_height < 0} {
		foreach request [database eval {SELECT request_sql FROM serialized_genesis_requests ORDER BY seq_index;}] {
			lappend response $request
			lappend response ""
		}
	}
	return $response
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
	database eval $mandatory_tables
	puts "tables added."
	after 1000 ascend_action
	puts "accepting connections."
	socket -server accept_connection $port
}

main $argv0 $argv

thread::wait
