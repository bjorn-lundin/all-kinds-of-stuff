GNATMAKE_FLAGS = -k -g -s
GCC_FLAGS = -gnat05 -O0 -funwind-tables -fstack-check -gnatwaK -gnatvoU -gnatwW
BIND_FLAGS= -E
LINKER_FLAGS = serial_port.o parallell_port.o daemon.o
all : scanner_handler

c : serial_port.o parallell_port.o daemon.o

scanner_handler : *.ad[bs] serial_port.o parallell_port.o daemon.o
	gnatmake $(GNATMAKE_FLAGS) scanner_handler -cargs $(GCC_FLAGS) -bargs $(BIND_FLAGS) -largs $(LINKER_FLAGS)

parallell_port.o : parallell_port.c
	gcc -c -O2 -Wall -pedantic parallell_port.c

serial_port.o: serial_port.c
	gcc -c -O2 -Wall -pedantic serial_port.c

daemon.o : daemon.c
	gcc -c -O2 -Wall -pedantic daemon.c

web : cgi.tcl lock.tcl route_conv.cgi route_conv.html 
	scp *.tcl *.html *.cgi route_conveyor.dat bnl@ibm:/home/bnl/cgi/

clean :
	rm -f *.o *.ali scanner_handler b~*.ad[bs] *.*~


install : scanner scanner_handler cgi.tcl lock.tcl route_conv.cgi route_conv.html
	sudo cp route_conv.cgi /usr/lib/cgi-bin/
	sudo cp cgi.tcl /usr/lib/cgi-bin/
	sudo cp lock.tcl /usr/lib/cgi-bin/
	sudo chown www-data:www-data /usr/lib/cgi-bin/route_conv.cgi
	sudo chown www-data:www-data /usr/lib/cgi-bin/cgi.tcl
	sudo chown www-data:www-data /usr/lib/cgi-bin/lock.tcl
	sudo chmod u+x /usr/lib/cgi-bin/route_conv.cgi
	sudo cp route_conv.html /var/www/
	sudo mkdir -p /usr/local/scanner_handler/log
	sudo mkdir -p /usr/local/scanner_handler/hourly_log
	sudo cp scanner_handler /usr/local/scanner_handler/
	sudo cp route_conveyor.dat /usr/local/scanner_handler/
	sudo cp scanner /etc/init.d/
	sudo chown root:root /etc/init.d/scanner
	sudo chmod u+x /etc/init.d/scanner
	sudo update-rc.d scanner defaults

