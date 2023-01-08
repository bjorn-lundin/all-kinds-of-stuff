

set ::cnt 0

proc Traverse_Md5 {f} {
    if {[file isdirectory $f]} {
        foreach g [glob -nocomplain [file join $f *]] {
	        Traverse_Md5 $g
	    }
    } elseif {[file isfile $f]} {
    
      incr ::cnt  
    
      set ext [string tolower [file extension $f]]
      set ext [string range $ext 1 end ]
      #puts stderr "file ext '$ext' '$f' "
      set cvt_pcx 1
      switch  $ext  {
        mov {set cvt_pcx 0}
        avi {set cvt_pcx 0}
        mp4 {set cvt_pcx 0}
        default {set cvt_pcx 1}
      }
      set OK 1
      set fname ""
      set createdate "1999-01-01 01:01:01"
      
      if {$cvt_pcx} { 
        # convert to pcx 
        set fname "$f\.pcx"
        if { [catch { exec convert "$f" "$fname" } ] } {
          puts stderr "file convert '$f' "
          set OK 0
        }
      } else {
          set fname "$f"
      } 

      #exec exiftool -T -d "%Y-%m-%d %H:%M:%S" -createdate "$f"
      if { [catch {set createdate [exec exiftool -p \$dateTimeOriginal -d "%Y-%m-%d %H:%M:%S" -q -f "$f"]} ] } {
          puts stderr "no createdate for '$f' "
      }
      
      if { $createdate == "-" } {
        set createdate null
      } else {
        set createdate "'$createdate'"      
      }
      
      if { $ext == "" } {
        set ext null
      } else {
        set ext "'$ext'"      
      }
      
      
      
      
      # get md5
      if {$OK} {
        #macos md5 -q file
        #linux md5sum file

        # to have it defined
        set md5list [list "error getting md5"]
        
        if { [catch {set md5list [exec md5sum "$fname"]} ] } {
          puts stderr "no md5 for '$f' "
          set OK 0
        }
        set md5 [lindex $md5list 0]
      }

      

      
      if {$cvt_pcx} {
        # delete pcx 
        if {$OK} {
          if { [catch { file delete "$f\.pcx" } ] } {
            puts stderr "no delete for '$f' "
            set OK 0
          }
        }
      }
     # print md5 to file

     # if { [catch {set t [exec tag --set $md5 "$f"]} ] } {
     #   puts stderr "no tag for '$f' "
     # }
      if {$OK} {
        set name [file tail $f]
        set path [file dirname $f]
        puts stdout "insert into md5s values ($::cnt,'$md5','$f','$name','$path', $createdate , $ext ); "
      }
     # puts stderr '$f'
    } else {
        puts stderr "'$f' is neither a file nor a directory"
    }
}
############### start main ########################
set Startpoint [lindex $argv 0]

Traverse_Md5 $Startpoint
