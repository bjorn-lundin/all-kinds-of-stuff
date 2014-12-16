

all : usb.o usb_client 

usb.o :usb.ads
	gcc -c -g -gnat05 usb.ads

usb_client : usb_client.adb lib8055.o usb.o
#	gnatmake  -c -g -gnat05  usb_client -largs ./lib8055.o -largs /lib/libusb-0.1.so.4 
	gnatmake  -g -gnat05  usb_client -largs /lib/libusb-0.1.so.4 

c8055.o : c8055.c
	gcc -c -Wall c8055.c

c8055 : c8055.c
	gcc -Wall c8055.c -o c8055 -lusb

lib8055.o : lib8055.c
	gcc -c -g -Wall lib8055.c

clean :
	rm -f *.o *.ali c8055 usb_client


