
with Interfaces.C;
with Interfaces.C.Strings;

package USB is
  package C renames Interfaces.C;



  type uint8_t is range 0 .. 2**8 -1;
  for uint8_t'size use 8;

  type uint16_t is range 0 .. 2**16 -1;
  for uint16_t'size use 16;

  type uint32_t is range 0 .. 2**32 -1;
  for uint32_t'size use 32;
   --  pragma Linker_Options("-lusb");
  PATH_MAX : constant := 4_096; -- <linux/limits.h>

  type Dirname_Array is array(1..PATH_MAX+1) of C.Char;


  ---------------------------------------------------------


  -----------------------------------------------------------------------
  type USB_Bus;
  type USB_Bus_Pointer is access all USB_Bus;

  -----------------------------------------------------------------------
  type USB_Device;
  type USB_Device_Pointer is access all USB_Device;
  type USB_Device_Pointer_Pointer is access all USB_Device_Pointer;

  -----------------------------------------------------------------------
  type USB_Device_Handle;
  type USB_Device_Handle_Pointer is access all USB_Device_Handle;

  -----------------------------------------------------------------------
  type USB_Device_Descriptor;
  type USB_Device_Descriptor_Pointer is access all USB_Device_Descriptor;

  -----------------------------------------------------------------------
  type USB_Config_Descriptor is null record;
  type USB_Config_Descriptor_Pointer is access all USB_Config_Descriptor;

  -----------------------------------------------------------------------
  type Whatever is null record;
  type Whatever_Pointer is access all Whatever;

  -----------------------------------------------------------------------


 -- from http://blog.ednchina.com/Upload/Blog/2007/10/7/b0d8378b-2df9-41dc-9f53-b6ef5c987f02.pdf
--struct usb_bus {
--  struct usb_bus *next, *prev;
--  char dirname[PATH_MAX + 1];
--  struct usb_device *devices;
--};
  type USB_Bus is record
    Next : USB_Bus_Pointer;
    Prev : USB_Bus_Pointer;
    Dirname : Dirname_Array;
    Devices : USB_Device_Pointer;
    Location : uint32_t;
    Root_Dev : USB_Device_Pointer;
  end record;
  pragma Convention(C, USB_Bus);


  ---------------------------------------------------------
  type USB_Device_Descriptor is record
    bLength            : uint8_t  ;
    bDescriptorType    : uint8_t  ;
    bcdUSB             : uint16_t ;
    bDeviceClass       : uint8_t  ;
    bDeviceSubClass    : uint8_t  ;
    bDeviceProtocol    : uint8_t  ;
    bMaxPacketSize0    : uint8_t  ;
    idVendor           : uint16_t ;
    idProduct          : uint16_t ;
    bcdDevice          : uint16_t ;
    iManufacturer      : uint8_t  ;
    iProduct           : uint8_t  ;
    iSerialNumber      : uint8_t  ;
    bNumConfigurations : uint8_t  ;
  end record;
  pragma Convention(C, USB_Device_Descriptor);



--  type USB_Device is null record;
-- from http://blog.ednchina.com/Upload/Blog/2007/10/7/b0d8378b-2df9-41dc-9f53-b6ef5c987f02.pdf
--struct usb_device {
--  struct usb_device *next, *prev;
--  char filename[PATH_MAX + 1];
--  struct usb_bus *bus;
--  struct usb_device_descriptor descriptor;
--  struct usb_config_descriptor *config;
--  void *dev; /* Darwin support */
--};



  type USB_Device_Handle is record
-- from http://blog.ednchina.com/Upload/Blog/2007/10/7/b0d8378b-2df9-41dc-9f53-b6ef5c987f02.pdf
--struct usb_dev_handle {
--  int fd;
--  struct usb_bus *bus;
--  struct usb_device *device;
--  int config;
--  int interface;
--  int altsetting;
--  void *impl_info;
--};
        Fd           : C.Int;
	Bus          : USB_Bus_Pointer;
	Device       : USB_Device_Pointer;
	Config       : C.Int;
	An_Interface : C.Int;
	Altsetting   : C.Int;
	Implinfo     : Whatever_Pointer;
  end record;
  pragma Convention(C, USB_Device_Handle);



  ---------------------------------------------------------
  type USB_Device is record
    Next         : USB_Device_Pointer;
    Prev         : USB_Device_Pointer;
    Filename     : Dirname_Array;
    Bus          : USB_Bus_Pointer;
    Descriptor   : USB_Device_Descriptor;
    Config       : USB_Config_Descriptor_Pointer;
    Darwinstuff  : Whatever_Pointer;
    Devnum       : uint8_t;
    Num_Childres : C.Unsigned_Char;
    Children     : USB_Device_Pointer_Pointer;
  end record;
  pragma Convention(C, USB_Device);




  ---------------------------------------------------
--from http://george.zjlotto.com/index.php/2008/04/04/detect-the-recovery-device-using-libusb/

--struct usb_device {
--  struct usb_device *next, *prev;
--  char filename[LIBUSB_PATH_MAX];
--  struct usb_bus *bus;
--  struct usb_device_descriptor descriptor;
--  struct usb_config_descriptor *config;
--  void *dev;		/* Darwin support */
--  unsigned char devnum;
--  unsigned char num_children;
--  struct usb_device **children;
--};


  ---------------------------------------------------
















  ---------------------------------------------------
  --Just like the name implies, usb_init sets up some
  --internal structures. usb_init must be
  --called before any other libusb functions.
  procedure USB_Init ;
  ---------------------------------------------------


  ---------------------------------------------------
  --usb_find_busses will find all of the busses on the
  --system. Returns the number of changes since
  --previous call to this function
  --(total of new busses and busses removed).
  procedure USB_Find_Busses;
  ---------------------------------------------------


  ---------------------------------------------------
  --usb_get_busses simply returns the value of the global
  --variable usb_busses. This was implemented for those
  --languages that support C calling convention and can
  -- use shared libraries, but don't support C global
  --variables (like Delphi).
  function USB_Get_Busses return USB_Bus_Pointer;
  ---------------------------------------------------



  ---------------------------------------------------
  --usb_find_devices will find all of the devices on
  --each bus. This should be called after usb_find_busses.
  --Returns the number of changes since the previous
  --call to this function (total of new device and devices removed).
  procedure USB_Find_Devices;
  ---------------------------------------------------


  type End_Point_Type is new C.Int;
  type Milli_Second_Type is new C.Int;
  ---------------------------------------------------

  --usb_interrupt_write performs an interrupt write request to the endpoint
  --specified by ep. Returns number of bytes written on success or < 0 on error.
  function USB_Interrupt_Write(Device_Handle : USB_Device_Handle_Pointer;
                               End_Point     : End_Point_Type;
                               Bytes         : C.Strings.Chars_Ptr;
                               Size          : C.Int;
                               Timeout       : Milli_Second_Type) return C.int;
  ---------------------------------------------------



  ---------------------------------------------------
  --usb_interrupt_read performs a interrupt read request to the endpoint
  --specified by ep. Returns number of bytes read on success or < 0 on error.
  function USB_Interrupt_Read(Device_Handle : USB_Device_Handle_Pointer;
                              End_Point     : End_Point_Type;
                              Bytes         : C.Strings.Chars_Ptr;
                              Size          : C.Int;
                              Timeout       : Milli_Second_Type) return C.int;
  ---------------------------------------------------


  ---------------------------------------------------
  --usb_dev_handle *usb_open(struct *usb_device dev);

  --usb_open is to be used to open up a device for use.
  --usb_open must be called before attempting to perform
  --any operations to the device. Returns a handle
  --used in future communication with the device.
  function USB_Open( Device : USB_Device_Pointer) return USB_Device_Handle_Pointer;
  ---------------------------------------------------

  ---------------------------------------------------
  --int usb_close(usb_dev_handle *dev);

  --usb_close closes a device opened with usb_open.
  --No further operations may be performed on the
  --handle after usb_close is called.
  --Returns 0 on success or < 0 on error.
  function USB_Close(Device_Handle : USB_Device_Handle_Pointer) return C.Int;
  ---------------------------------------------------

 ---------------------------------------------------
 --int usb_set_configuration(usb_dev_handle *dev, int configuration);

 --usb_set_configuration sets the active configuration of a device.
 --The configuration parameter is the value as specified in the
 --descriptor field bConfigurationValue.
 --Returns 0 on success or < 0 on error.
  function USB_Set_Configuration(Device_Handle : USB_Device_Handle_Pointer;
                                 Configuration : C.Int ) return C.Int;
  ---------------------------------------------------

  ---------------------------------------------------
  --int usb_set_altinterface(usb_dev_handle *dev, int alternate);
  --usb_set_altinterface sets the active alternate setting of the
  --current interface. The alternate parameter is the value as
  --specified in the descriptor field bAlternateSetting.
  --Returns 0 on success or < 0 on error.
  function USB_Set_Alternative_Interface(Device_Handle : USB_Device_Handle_Pointer;
                                         Alternative   : C.Int ) return C.Int;

  ---------------------------------------------------


  ---------------------------------------------------
  --int usb_claim_interface(usb_dev_handle *dev, int interface);

  --usb_claim_interface claims the interface with the Operating System.
  --The interface parameter is the value as specified in the
  --descriptor field bInterfaceNumber.
  --Returns 0 on success or < 0 on error.

  --Must be called!: usb_claim_interface must be called before
  --you perform any operations related to this interface
  --(like usb_set_altinterface, usb_bulk_write, etc).

  --  Table 1. Return Codes
  --  code	description
  -- -EBUSY	Interface is not available to be claimed
  -- -ENOMEM	Insufficient memory

  function USB_Claim_Interface(Device_Handle : USB_Device_Handle_Pointer;
                               An_Interface  : C.Int ) return C.Int;

  ---------------------------------------------------

  ---------------------------------------------------
  --int usb_release_interface(usb_dev_handle *dev, int interface);

  --usb_release_interface releases an interface previously
  --claimed with usb_claim_interface. The interface parameter
  --is the value as specified in the descriptor field bInterfaceNumber.
  --Returns 0 on success or < 0 on error.
  function USB_Release_Interface(Device_Handle : USB_Device_Handle_Pointer;
                                 An_Interface  : C.Int ) return C.Int;
  ---------------------------------------------------

  ---------------------------------------------------
  --int usb_get_driver_np(usb_dev_handle *dev, int interface, char *name, int namelen);

  --This function will obtain the name of the driver bound to the
  --interface specified by the parameter interface and place it into
  --the buffer named name limited to namelen characters.
  --Returns 0 on success or < 0 on error.
  --  Implemented on Linux only.

  function USB_Get_Driver_NP(Device_Handle : USB_Device_Handle_Pointer;
                             An_Interface  : C.Int;
                             Name          : C.Strings.Chars_Ptr;
                             Name_Len      : C.Int) return C.Int;

  function USB_Get_Driver(Device_Handle : USB_Device_Handle_Pointer;
                          An_Interface  : C.Int;
                          Name          : C.Strings.Chars_Ptr;
                          Name_Len      : C.Int) return C.Int
                          renames USB_Get_Driver_NP;
  ---------------------------------------------------


  ---------------------------------------------------
  --int usb_detach_kernel_driver_np(usb_dev_handle *dev, int interface);

  --This function will detach a kernel driver from the interface
  --specified by parameter interface. Applications using libusb can
  --then try claiming the interface.
  --Returns 0 on success or < 0 on error.
  --Implemented on Linux only.

  function USB_Detach_Kernel_Driver_NP(Device_Handle : USB_Device_Handle_Pointer;
                                       An_Interface  : C.Int) return C.Int;

  function USB_Detach_Kernel_Driver(Device_Handle : USB_Device_Handle_Pointer;
                                    An_Interface  : C.Int) return C.Int
                                    renames USB_Detach_Kernel_Driver_NP;
  ---------------------------------------------------




  ---------------------------------------------------
  -- from kernel..
  --void usb_set_debug( int )
  procedure USB_Set_Debug(Level : C.int);
  ---------------------------------------------------

  ---------------------------------------------------
  -- from kernel..
  --char *  usb_strerror()
  function USB_Strerror return C.Strings.Chars_Ptr;
  -- usb_strerror() returns as a char *, the result of the latest action.
  --Think of the good old #include <errno.h> perror() days :-)



  ---------------------------------------------------

  function C_Locate_Xsv return USB_Device_Handle_Pointer;
  ---------------------------------------------------

private
  pragma Import(C, USB_Init, "usb_init");
  pragma Import(C, USB_Find_Busses, "usb_find_busses");
  pragma Import(C, USB_Get_Busses, "usb_get_busses");
  pragma Import(C, USB_Find_Devices, "usb_find_devices");
  pragma Import(C, USB_Interrupt_Write, "usb_interrupt_write");
  pragma Import(C, USB_Interrupt_Read, "usb_interrupt_read");
  pragma Import(C, USB_Open, "usb_open");
  pragma Import(C, USB_Close, "usb_close");
  pragma Import(C, USB_Set_Configuration, "usb_set_configuration");
  pragma Import(C, USB_Set_Alternative_Interface, "usb_set_altinterface");
  pragma Import(C, USB_Claim_Interface, "usb_claim_interface");
  pragma Import(C, USB_Release_Interface, "usb_release_interface");
  pragma Import(C, USB_Get_Driver_NP, "usb_get_driver_np");
  pragma Import(C, USB_Detach_Kernel_Driver_NP, "usb_detach_kernel_driver_np");
  pragma Import(C, USB_Set_Debug, "usb_set_debug");
  pragma Import(C, USB_Strerror, "usb_strerror");
  pragma Import(C, C_Locate_Xsv, "locate_xsv");



end USB;

