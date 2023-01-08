

set To_List_Ptr [open [file join . files_to_tag.dat] {RDONLY}]
set line ""
while {[gets $To_List_Ptr line] >= 0} {
  set la [split $line, ";"]
  set md5tmp [lindex $la 0] 
  set fullnametmp [lindex $la 1] 
  set md5 [string range $md5tmp 1 end-1]

  puts $fullnametmp
  set fullname [string range $fullnametmp 1 end-2]
  puts $fullname


    
  #if { [catch {set t [exec tag --add  "$md5,pt" $fullname]} ] } {
  #  puts stderr "no tag for '$fullname' "
  #}
  #puts "tag --add $md5,pt $fullname "


  
  set dir2 [file dirname $fullname]
  set dir [string range $dir2 1 end] ; # no "/ in beginning ..slash

  
  puts "dir $dir"
  set newdir [file join / Users bnl tmp $dir]
  if { ! [file isdirectory $newdir] } {
    puts "create dir $newdir"
    file mkdir $newdir  
  }

  #new filename
  set newfile [file join $newdir [file tail $fullname]]
  
  if { ! [file exists "$newfile"] } {
    puts "cp $fullname $newfile"
    file copy "$fullname" "$newfile"
  }
  





}
close $To_List_Ptr
