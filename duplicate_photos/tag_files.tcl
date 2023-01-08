



set To_List_Ptr [open [file join . files_to_tag.dat] {RDONLY}]
set line ""
while {[gets $To_List_Ptr line] >= 0} {
  set la [split $line, ";"]
  set md5tmp [lindex $la 0] 
  set fullname [lindex $la 1] 

  set md5 [string range $md5tmp 1 end-1]
  
#  if { [catch {set t [exec tag --add  "$md5,pt" $fullname]} ] } {
#    puts stderr "no tag for '$fullname' "
#  }

  puts $fullname

  set fullname2 [string range $fullname 2 end-2]
  puts $fullname2

   
  set fullname3 [file join / Users bnl tmp $fullname2]
  puts $fullname3

  
  puts "tag --add $md5,pt $fullname3 "



  if { [catch {set t [exec tag --add $md5,pt $fullname3]} ] } {
    puts stderr "no tag for '$f' "
  }
#  puts "$md5 , $fullname"
}
close $To_List_Ptr
