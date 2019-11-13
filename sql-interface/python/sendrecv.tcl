#!/usr/bin/tclsh8.5

if {[llength $argv] < 2} {
	error "usage: sendrecv.tcl addr port\n\nCopy stdin into socket and recv it until two consecutive newlines."
}
lassign $argv addr port

set h [socket $addr $port]

while {![eof stdin]} {
	set l [gets stdin]
	puts $h $l
}
flush $h

set nl_count 0
set first 1
while {$nl_count < 2} {
	set l [gets $h]
	if {[string length $l] > 0} {
		set nl_count $first
		set first 0
	} else {
		incr nl_count
	}
	puts $l
}

close $h
