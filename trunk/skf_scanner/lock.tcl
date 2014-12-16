 ###############################################################################
 # Package: lock.tcl
 # Author:  M.Hoffmann 2005-2006
 # Purpose: Guarantee atomic operations using a LOCKFILE, wrapper functions
 # Remarks: For intERprocess coordination, each process must specify THE SAME
 #          LOCKFILE. It's up to the programer to choose a name.-  We can let
 #          the module determine a name only if used within one program. Take
 #          care of the timeout: it depends on the type of the locked code and
 #          number of processes competing for the lock. The default is to wait
 #          only 1/4s for a lock to become available. Specifying a timeout of 0
 #          means to wait forever for a lock.
 # History:
 #  v0.1 2005/12/26
 #   - First release.
 #  v0.2 2006/02/25
 #   - New procs timeout and lockfile superseed the corresponding vars which
 #     are no longer availabe - they are used to query (and optionaly set new)
 #     defaults.
 #   - With v0.1 it wasn't possible to permanently overwrite the built-in
 #     defaults. [withLock] and [incrCounter] ALWAYS used these defaults because
 #     they where arg-less.
 #   - Additional proc defaults which restores timeout and lockfile to the
 #     original state.
 #   - Reworked withLock: errorState and results are given back to the caller.
 #   - withLock and incrCounter accepts optional args to pass timeout and lock-
 #     file on to acquireLock.
 #   - Added username to lockfile (perhaps useful in search for bugs).
 #   - Addes namespace export *.
 #   - New and redesigned testproc(s) - some of them never worked right...
 ###############################################################################

 package provide lock 0.2

 namespace eval lock {

    variable defto 250; # 250ms default time to acquire/use a lock; 0 = infinite
    variable curto $defto
    variable deflf [file join $::env(temp) [pid].lock]; # default lockfile
    variable curlf $deflf
    variable wait

    namespace export *

    # Query (and optionally set) the default timeout. Returns (new) value.
    #  Attention: no arg-checking!
    proc timeout args {
       variable curto
       return [expr {[llength $args]==1?[set curto $args]:$curto}]
    }

    # Query (and optionally set) the default lockfile. Returns (new) value.
    #  Attention: no arg-checking!
    proc lockfile args {
       variable curlf
       return [expr {[llength $args]==1?[set curlf $args]:$curlf}]
    }

    # Restore the original defaults
    proc defaults {} {
       variable defto
       variable deflf
       variable curto $defto
       variable curlf $deflf
    }

    # Problem: defaults through arglists are static. But user can change the
    # defaults through procs. So special code is required.
    proc acquireLock args {
       variable curto
       variable curlf
       # these (private) values are for now not accessible from outside
       set timeout  [expr {[lindex $args 0]==""?$curto:[lindex $args 0]}]
       set lockfile [expr {[lindex $args 1]==""?$curlf:[lindex $args 1]}]
       set trystart [clock clicks -milliseconds]
       while {([clock clicks -milliseconds] - $trystart) < $timeout || $timeout == 0} {
          if {![catch {open $lockfile {WRONLY CREAT EXCL}} rc]} {
             puts  $rc "locked by pid [pid] from $::tcl_platform(user)\
                        at [clock format [clock seconds]]"
             flush $rc
             return [list $rc $lockfile]
          }
          after 10 set lock::wait 1
          vwait lock::wait
       }
       return -code error "Error: timeout after $timeout ms"
    }

    proc releaseLock {info} {
       if {![catch {fconfigure [lindex $info 0]}]} {
          if {![catch {
             close [lindex $info 0]
             file delete -force [lindex $info 1]
             # another OPEN ... EXCL only will succeed if the file is gone
          } rc]} {
             return ""
          } else {
             return -code error "Error releasing lockfile: '$rc'"
          }
       } else {
          return "invalid lock, ignored"
       }
    }

    # Abstraction layer upon acquire/release to perform code which is protected
    # from running in parallel through a lockfile. Useful e.g. to update a file
    # which is in use by many systems. Optionally specify timeout and lockfile
    # which are passed through to acquireLock (eval is therefore required).
    # To handle errors in this routine, use catch {withLock {...}}
    proc withLock {code args} {
       if {![catch {eval acquireLock $args} rc]} {
          set urc [catch {uplevel 1 $code} ret]
          releaseLock $rc; # report errors the the caller, no CATCHing here
          return -code $urc $ret; # give original results back, including errors
       } else {
          return -code error $rc; # cannot get lock
       }
    }

    # Abstraction layer upon withLock, to maintain a file with a counter in it.
    # The counter is incremented with each access, to maintain a unique id.
    # Optionally specify timeout and lockfile which are passed over
    # wihtLock to acquireLock.
    # Let errors raise up to the main proc.
    proc incrCounter {ctrFile args} {
       set cmd {
          if {[file exists $ctrFile]} {
             set hCntr [open $ctrFile r]; # a+ not possible in VFS::MK4...
             set counter [gets $hCntr]
             incr counter
             close $hCntr
          } else {
             set counter 0
          }
          set hCntr [open $ctrFile w]; # a+ not possible in VFS::MK4...
          puts $hCntr $counter
          close $hCntr
       }
       eval withLock [list $cmd] $args; # notice eval and list are required!
       return $counter
    }
 }
