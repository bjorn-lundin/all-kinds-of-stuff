
set ::env(SATTMATE_DATABASE_TYPE) sqlserver

proc insert_store_statement {store name alg} {
     
    switch [string tolower $::env(SATTMATE_DATABASE_TYPE)] {
        oci -
        oracle_odbc {
                 puts "ora store=$store, name=$name, alg=$alg"
        }
      
        sqlserver {
                 puts "ss store=$store, name=$name, alg=$alg"
        }

        postgresql_odbc {
                 puts "pg store=$store, name=$name, alg=$alg"
        }
                
        default {
                puts "not supported db '$::env(SATTMATE_DATABASE_TYPE)'"
        }
    }
}


proc update_xloc_statement { name coordinate size} {

    puts "update xloc set xlocnam='$name', xlocsiz=$size where xlocid='$coordinate'"
     
    switch [string tolower $::env(SATTMATE_DATABASE_TYPE)] {
        oci -
        oracle_odbc -
        postgresql_odbc {
            puts ";"
        }
      
        sqlserver {
            puts "go"
        }
                
        default {
                puts "not supported db '$::env(SATTMATE_DATABASE_TYPE)'"
        }
    }
}


proc insert_missing_xloc_statement { name coordinate size} {
     
    switch [string tolower $::env(SATTMATE_DATABASE_TYPE)] {
        oci -
        oracle_odbc {
                 puts "ora name=$name, coordinate=$coordinate, size=$size"
        }
      
        sqlserver {
                 puts "ss name=$name, coordinate=$coordinate, size=$size"
        }

        postgresql_odbc {
                 puts "ss name=$name, coordinate=$coordinate, size=$size"
        }
        
        default {
                puts "not supported db '$::env(SATTMATE_DATABASE_TYPE)'"
        }
    }
}
    

set myfile "store_data.dat"
set f [open $myfile r]
while {[gets $f line]>=0} {
  if { ! [string match #* $line] } {
     set data [split $line '|']
     set store [lindex $data 0]
     set name [lindex $data 1]
     set alg [lindex $data 2]
     insert_store_statement $store $name $alg
  }
}
close $f
#####################################

set myfile "insert_missing_xloc.dat"
set f [open $myfile r]
while {[gets $f line]>=0} {
  if { ! [string match #* $line] } {
     set data [split $line '|']
     set name [lindex $data 0]
     set coordinate [lindex $data 1]
     set size [lindex $data 2]
     insert_missing_xloc_statement $name $coordinate $size
  }
}
close $f
#####################################


set myfile "update_xloc.dat"
set f [open $myfile r]
while {[gets $f line]>=0} {
  if { ! [string match #* $line] } {
     set data [split $line '|']
     set name [lindex $data 0]
     set coordinate [lindex $data 1]
     set size [lindex $data 2]
     update_xloc_statement $name $coordinate $size
  }
}
close $f
######################################
