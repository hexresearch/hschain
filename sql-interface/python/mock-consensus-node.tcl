#!/usr/bin/tclsh

# mock of the consensus node.
# it is written in Tcl because it is safer than Python
# and has better TCP/IP support.
# 

package require sqlite3

proc main {argv0 args} {
	if {[llength $args] != 2} {
		puts "usage: $argv0 \"command to populate tables\""]
		exit 1
	}
	lassign {port mandatory_tables_command} $args
	puts "getting mandatory tables content..."
	set mandatory_tables [eval exec $mandatory_tables_command]
	set databasefn "bububu.db"
	file delete -force $databasefn
	sqlite3 
}

main $argv0 $args
