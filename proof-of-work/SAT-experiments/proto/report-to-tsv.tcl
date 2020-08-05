#!/usr/bin/tclsh

while {![eof stdin]} {
	set l [gets stdin]
	if {[regexp {^complexity shift +([0-9]+), mantissa ([0-9a-fA-F]+): total time +([0-9.]+) sec, time per sample solution +([0-9.]+) sec, success percentage +([0-9.]+)$} $l _ shift mantissa total_time avg_time]} {
		puts [join [list $shift [format %d 0x$mantissa] $avg_time] \t]
	}
}
