



set To_List_Ptr [open [file join svn all.dat] {RDONLY}]

while {[gets $To_List_Ptr md5] >= 0} {
  if { [catch {set t [exec tag --find $md5 "tc"]} ] } {
    puts stderr "no tag for '$f' "
  }
  set path [split $t "\n"]
  puts ""
  puts $md5
  set first 1
  foreach p $path {
    if {! $first } {
      if { [catch {set t [exec rm $p]} ] } {
        puts stderr "failed delete '$p' "
      } else {
        puts stderr "deleted '$p' "
      }
      
    }
    set first 0
  }
}
close $To_List_Ptr
