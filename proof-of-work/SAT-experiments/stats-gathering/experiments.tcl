#!/usr/bin/tclsh

# Running experiments for SAT as PoW

proc k {} { return 3 }
proc nvars_low {} { return 256 }
proc nvars_high {} { return 1024 }
proc nvars_step {} { return 16 }
proc nclauses_low {nvars} { expr {int($nvars * 4.30)} }
proc nclauses_high {nvars} { expr {int($nvars * 4.40)} }
proc experiment_tries {} { return 100 }

proc parse_picosat_result {tracefile result} {
	set solve_time -1
	set num_solutions -1
	set seed {}
	set mb_allocated -1
	foreach line [split $result "\n"] {
		#puts "line: '$line'"
		if {[regexp {c ([0-9]+[.][0-9]+) seconds in library} $line _ time_seconds]} {
			set solve_time $time_seconds
		} elseif {[regexp {s SOLUTIONS ([0-9]+)} $line _ num_solutions]} {
			# nothing.
		} elseif {[regexp {generator seed ([0-9]+)} $line _ seed_reported]} {
			set seed $seed_reported
		} elseif {[regexp { (.*) MB maximally allocated} $line _ max_MB]} {
			# nothing.
		}
	}
	if {$num_solutions < 0 } {
		error "haven't found result line"
	}
	if {$num_solutions == 0} {
		set tracelines [lindex [exec wc -l $tracefile] 0]
		set tracewords [lindex [exec wc -w $tracefile] 0]
	} else {
		set tracelines -1
		set tracewords -1
	}
	if {$solve_time < 0} {
		error "haven't found solution time"
	}
	if {[string length $seed] < 1} {
		error "haven't seen a seed"
	}
	return [list picosat_num_solutions $num_solutions picosat_time $solve_time picosat_trace_words $tracewords picosat_trace_lines $tracelines picosat_max_MB $max_MB]
}

proc run_solver {solver cnf seed} {
	switch -- $solver {
		picosat {
			set tracefile picosat-trace-file
			return [parse_picosat_result $tracefile [exec bash -c "tee picosat-input.cnf | picosat -v -s $seed -n --all | tee picosat.log || true" << $cnf]] }
		minisat { error "don't know how to handle minisat run" }
		sadical { error "don't know how to handle sadical run" }
		default { error "unknown solver type to run: $solver" }
	}
}

proc generate_cnf {numvars numclauses} {
	set result [list "p cnf $numvars $numclauses"]
	array set clauses {}
	while {[llength $result] < $numclauses + 1} {
		set clause [list ]
		set variables [list ]
		# generate list of distinct variables.
		while {[llength $variables] < [k]} {
			set vi [expr {1 + int($numvars * rand())}]
			if {[lsearch $variables $vi] >= 0} continue
			lappend variables $vi
		}
		# add polarities.
		foreach v $variables {
			lappend clause [expr {$v * (rand() >= 0.5? 1 : -1)}]
		}
		# check whether we have that clause already.
		if {[info exists clauses($clause)]} continue
		lappend clause 0
		lappend result $clause
	}
	return [join $result "\n"]
}

proc run_for_nvars {nvars} {
	set high [nclauses_high $nvars]
	set low [nclauses_low $nvars]
	set total 100
	if {$total > $high - $low} {
		set total [expr {$high - $low}]
	}
	for {set nci 0} {$nci <= $total} {incr nci} {
		set nclauses [expr {($high-$low)*$nci/$total + $low}]
		set max_cnfi 20
		if {$nci >= 0} {
			set max_cnfi 4
		}
		for {set cnfi 0} {$cnfi < $max_cnfi} {incr cnfi} {
			puts "new cnf"
			set cnf [generate_cnf $nvars $nclauses]
			set num_random_runs 10
			if {$nci >= 0} {
				set num_random_runs 1
			}
			for {set seed 0} {$seed < $num_random_runs} {incr seed} {
				foreach solver {picosat} {
					set stats [run_solver $solver $cnf $seed]
					lappend stats nvars $nvars nclauses $nclauses
					puts "stats: $stats"
					flush stdout
				}
			}
		}
	}
}

proc main {} {
	for {set nv [nvars_low]} {$nv <= [nvars_high]} {incr nv [nvars_step]} {
		puts "at $nv ([expr {100*$nv/[nvars_high]}]%)"
		run_for_nvars $nv
	}
}

main
