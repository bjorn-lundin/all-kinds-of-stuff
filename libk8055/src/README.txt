The k8055 require libusb-0.1.9 or newer and kernel 2.4.18 or never.

install:
	#make all
	#make install
	update ld.so.conf with "/usr/local/lib" if not there or run
	  env-update at gentoo systems	

uninstall:
	#make uninstall

Check out the Makefile for all different make rules...

use k8055 library:
	in the program file "#include <k8055.h>" and compile with -lk8055 and
	-lusb, e.g gcc -lusb -lk8055 main.c -o k8055_rocks
