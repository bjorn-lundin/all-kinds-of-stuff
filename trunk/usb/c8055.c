/***************************************************************************
 *   Copyright (C) 2004 by Nicolas Sutre                                   *
 *   nicolas.sutre@free.fr                                                 *
 *                                                                         *
 *   This program is free software; you can redistribute it and/or modify  *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This program is distributed in the hope that it will be useful,       *
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of        *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *
 *   GNU General Public License for more details.                          *
 *                                                                         *
 *   You should have received a copy of the GNU General Public License     *
 *   along with this program; if not, write to the                         *
 *   Free Software Foundation, Inc.,                                       *
 *   59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.             *
 ***************************************************************************/

 
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


#include <string.h>

#include </usr/include/usb.h>
#include <assert.h>

#define STR_BUFF 256

char* error_str_ = NULL;

usb_dev_handle *locate_xsv(void);
// string product_Id;

int ia1;
int ia2;
int id8;
int ipid;

static int takeover_device( usb_dev_handle* udev, int interface )
{

  usb_set_debug(2);

  char driver_name[STR_BUFF];
  memset( driver_name, 0, STR_BUFF );
  int ret = -1;
  assert( udev != NULL );
  ret = usb_get_driver_np( udev, interface, driver_name, sizeof(driver_name) );
    
  if ( ret <0 ){
     fprintf( stderr, "get driver name: %s\n", usb_strerror());
  }
   
  if ( 0 > usb_detach_kernel_driver_np( udev, interface ) )
  {
     fprintf( stderr, "Disconnect OS driver: %s\n", usb_strerror());
  }
  else
    fprintf( stderr, "Disconnected OS driver: %s\n", usb_strerror());

  /*claim interface */
  usb_set_altinterface(udev, interface);  
  if( usb_claim_interface(udev, interface) <0 )
  {
    fprintf( stderr,"%s\n", usb_strerror());
    error_str_ = usb_strerror();
    return -1;
  }
  else fprintf( stderr,"Find interface %d\n", interface);
      
  fprintf( stderr, "Took over the device\n");
  error_str_ = NULL;
  
  return 0;
}

usb_dev_handle *locate_xsv(void) {
	unsigned char located = 0;
	struct usb_bus *bus;
	struct usb_device *dev;
	usb_dev_handle *device_handle = 0;

	usb_find_busses();
	usb_find_devices();

 	for (bus = usb_busses; bus; bus = bus->next) {
		for (dev = bus->devices; dev; dev = dev->next)	 {
			if ( (dev->descriptor.idVendor == 0x10cf) && 
			     (dev->descriptor.idProduct == ipid ) ) {

				located++;
				device_handle = usb_open(dev);
				fprintf(stderr,
                                        "XSV Device Found @ Address %s Vendor 0x0%x Product ID 0x0%x\n",
                                        dev->filename, dev->descriptor.idVendor,dev->descriptor.idProduct);

				if ( 0<= takeover_device( device_handle, 0 ) ) {
					error_str_ = NULL;
					break;
				} else {
					error_str_ = "Can not take over the device from the OS driver\n";
					break;
				}
			} else { 
                          fprintf(stderr,"** Device Found @ Address %s Vendor 0x0%x Product ID 0x0%x\n", 
                           dev->filename, dev->descriptor.idVendor,dev->descriptor.idProduct);
                        }
		}
	}
	fprintf(stderr," ipid 0x0%x \n", ipid);		
	if (device_handle==0) { 
          return (0);
	} else {
          return (device_handle);
    }  
}     

int read_param(int argc,char *argv[]) { 
  int error = 0;
  ipid = 1;
  int c;
  opterr = 0;

  char *a = NULL;
  char *b = NULL;
  char *d = NULL;
  char *p = "1";
     
  while ((c = getopt (argc, argv, "p:a:b:d:")) != -1) {
    switch (c) {
      case 'a':
             a = optarg;
             break;
      case 'b':
             b = optarg;
             break;
      case 'd':
             d = optarg;
             break;
      case 'p':
             p = optarg;
             break;
      case '?':
             if (optopt == 'c') {
               fprintf (stderr, "Option -%c requires an argument.\n", optopt);
               exit(1);
             } else if (isprint (optopt)) {
               fprintf (stderr, "Unknown option `-%c'.\n", optopt);
               exit(1);
             } else {
               fprintf (stderr,
                        "Unknown option character `\\x%x'.\n",
                        optopt);
               exit(1);
             }
     default:
             exit(1);
           }
    }

    ipid = atoi(p);
    id8 = atoi(d);
    ia1 = atoi(a);
    ia2 = atoi(b);
 // Board address
    switch (ipid)	{
          case 1  : ipid = 0x5500; break;
          case 2  : ipid = 0x5501; break;
          case 3  : ipid = 0x5502; break;
          case 4  : ipid = 0x5503; break;
          default : error = 1;     break;
    }

    if (error) {
		printf("** error in params **\n");
		printf("** example : c8055 -p1 -d147 -a:25 -b:203 **\n");
		printf("**        p is board (1-4), defaults to 1\n");
		printf("**        d is digital out\n");
		printf("**        a is analog  out 1\n");
		printf("**        b is analog  out 2\n");
    }
    return ! error;
	
}



/*

int main (int argc,char *params[]) 
{
        int i;
	struct usb_dev_handle *xsv_handle;
	int open_status;
	char data[9];

	if (read_param(argc,params)) {
		usb_init();
		usb_set_debug(2);
		if ((xsv_handle = locate_xsv())==0) {
			printf("Could not open the XSV device\n");
			return (-1);
		} else {
		  // read and print the EndPoint 0x81
												
			open_status = usb_interrupt_read(xsv_handle,0x81,data,8,20);
			
			printf("K8055 0.1 : %d %d %d %d %d %d %d %d   // --> %d %d %d \n", 
			data[0] ,data[1] ,data[2] ,data[3] ,data[4] ,data[5] ,data[6] ,data[7] ,
			id8, ia1, ia2);
			
			// Write the EndPoint 0x01 
			for (i=0; i < 7; i++) data[i] = 0;
			  
			data[0] = 0x05;
			data[1] = id8;
			data[2] = ia1;
			data[3] = ia2;
			 
			//data[1] = 0xFF; 
                        open_status = usb_interrupt_write(xsv_handle,0x1,data,8,20);
                        //usleep(100000);
                        // It's better with 2 write !?... 
                        open_status = usb_interrupt_write(xsv_handle,0x1,data,8,20);
						
			usb_close(xsv_handle);
		}
	}
	return 0;
}
*/




