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

proc accept_connection {socket address port} {
	set thread_script {
	package require thread
	# the procedure that reads requests from specified socket
	# and calls (thread::send) main thread to take action.
	proc read_requests {socket main_id} {
		
	}
}
	thread::create "$thread_script\n\nread_requests $socket [thread::id]"
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
puts "Evaluating:
-------------------------
$mandatory_tables
-------------------------
"
	database eval $mandatory_tables
	puts "tables added."
	after 1000 ascend_action
	puts "accepting connections."
	socket -server accept_connection $port
}

main $argv0 $argv

thread::wait
