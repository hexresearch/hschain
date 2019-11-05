#!/usr/bin/tclsh

# mock of the consensus node.
# it is written in Tcl because it is safer than Python
# and has better TCP/IP support.
# 

package require sqlite3

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
}

main $argv0 $argv
