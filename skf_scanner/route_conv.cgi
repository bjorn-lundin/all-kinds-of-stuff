#!/usr/bin/tclsh

source "cgi.tcl"
set ::env(temp) "/tmp" ; # This is only here since lock.tcl assumes env temp exists
source "lock.tcl"

# in production
set lockfile     [file join / var lock route_conveyor.lock]
set datafile     [file join / usr local scanner_handler route_conveyor.dat]
set hourlyLogDir [file join / usr local scanner_handler hourly_log]

set NOT_DEFINED not_defined

set Conveyor_List {}

set ROUTE_IDX_START 0
set ROUTE_IDX_STOP 9
set CONVEYOR_IDX_START 10
set CONVEYOR_IDX_STOP 12
set ROUTE_LENGTH [expr $ROUTE_IDX_STOP - $ROUTE_IDX_START +1]
set CONVEYOR_LENGTH [expr $CONVEYOR_IDX_STOP - $CONVEYOR_IDX_START +1]

global Conveyor(null) 

set Log_Enabled 0
###############################################################################

###############################################################################
proc Log {what} {
	if {$::Log_Enabled} {
		puts stderr $what
	}
}
###############################################################################
proc Parse_Data_File_To_Array {df} {
# Use an tcl-array, indexed by "conveyornumber,route" and "conveyornumber,valid"
#in tcl- index are strings, so an array can be index 1 2 3 or A B C or "1,route"  
# here we use that to simulate an array of a struct or record with fields 'route' and 'valid'
# read from list
	global Conveyor Conveyor_List
	set line {}
	set tmp_list {}
	set df_ptr [open $df {RDONLY}]

	while {[gets $df_ptr line] >= 0} {
		set Route [string trim [string range $line $::ROUTE_IDX_START $::ROUTE_IDX_STOP]]
		set Conv [string trim [string range $line $::CONVEYOR_IDX_START $::CONVEYOR_IDX_STOP]]
		Log "Route - '$Route' -> Conv - '$Conv'"
		set Conveyor($Conv,route) $Route
		set Conveyor($Conv,valid) 1
		lappend tmp_list $Conv 
	}
	set Conveyor_List [lsort -integer $tmp_list]
	close $df_ptr 
}
###############################################################################
proc Write_Array_To_Data_File {df} {
# write the contents of the global array to file, truncating it.
#only valid entries are written.
	global Conveyor Conveyor_List
	set df_ptr [open $df {WRONLY TRUNC}]
	foreach item $Conveyor_List {
		if { $Conveyor($item,valid)} {
			Log [format "%-*s%-*s" $::ROUTE_LENGTH $Conveyor($item,route) $::CONVEYOR_LENGTH $item]
			puts $df_ptr [format "%-*s%-*s" $::ROUTE_LENGTH $Conveyor($item,route) $::CONVEYOR_LENGTH $item]
		}
	}
	close $df_ptr
}
###############################################################################
proc Print_Array {} {
	global Conveyor Conveyor_List
	foreach item $Conveyor_List {
		Log "  $item : $Conveyor($item,route) valid : $Conveyor($item,valid)"
	}

}
###############################################################################
proc Do_Delete {idx} {
# Change the valid status for an element int the global array to not valid
#Write_Array_To_Data_File tests on the valid field
	global Conveyor Conveyor_List
	foreach item $Conveyor_List {
		if {[string equal $idx $item] } {
			set Conveyor($item,valid) 0
		}
	}
}
###############################################################################
proc Do_Update {idx val} {
# Change the value in the global array, element at index idx with value val
	global Conveyor Conveyor_List
	foreach item  $Conveyor_List {
		if {[string equal $idx $item] } {
			set Conveyor($item,route) $val
		}
	}
}
###############################################################################
proc Do_Add {idx val} {
# no add action is defines in the html page, but for future?
	global Conveyor Conveyor_List
	set Found 0
	foreach item $Conveyor_List {
		if {[string equal $idx $item] } {
			set Found 1
			Do_Update $idx $val
		}
	}
	if {! $Found} {
		set Conveyor($idx,route) $val
		set Conveyor($idx,valid) 1
		set tmp_list $Conveyor_List
		lappend tmp_list $idx
		set Conveyor_List [lsort -integer $tmp_list]

	}
}
###############################################################################
proc Do_Show {idx isLogFile} {
# either pass the content of a logfile to the caller (javascript page) or
# write the content of one specific conveyor (when button pushed to update the route
# in the html page) or write all conveyors back (onLoad for body of html page)
	global Conveyor Conveyor_List
	#CGI header
	puts "Content-type: text/plain\r\n"
	if {$isLogFile} {
		set logfile [file join $::hourlyLogDir $idx.log]
		set logfile_ptr [open $logfile {RDONLY}]
		while {[gets $logfile_ptr line] >= 0} {
			puts $line
		}
		close $logfile_ptr 
	} else {
		if {$idx} {
			puts "$Conveyor($idx,route)\t$idx"
		} else {
		# show all of them
			foreach item $Conveyor_List {
				puts "$Conveyor($item,route)\t$item"
			}
		}
	}
}

#Initialize read GET variables
cgi_input
#Get the CGI-variables (QUERY_STRING)
if { [ catch {cgi_import action} ]} {
	set action $NOT_DEFINED
}

if { [ catch {cgi_import index} ]} {
	set index  $NOT_DEFINED
}

if { [ catch {cgi_import value} ]} {
	set value  $NOT_DEFINED
}

Log "action - $action"
Log "index - $index"
Log "value - $value"

# Parse the file exclusively and take action thereafter
# That is, manipulate the global array
# write it to disk
# relase the lock
# show the result
# File locked for more that 10 sec (10000 ms) is really,really bad -> die
set Write_To_File 1
set isLogFile 0
lock::withLock {
	switch -exact -- $action {
		update  {
					Parse_Data_File_To_Array $datafile
					Do_Update $index $value
		}
		show    {
					Parse_Data_File_To_Array $datafile
					set Write_To_File 0
		}
		showLog {
					set Write_To_File 0
					set isLogFile 1
		}
		default {set Write_To_File 0}
	}
	if {$Write_To_File} {
		Write_Array_To_Data_File $datafile
	}
} 10000 $lockfile
Do_Show $index $isLogFile
#Print_Array
