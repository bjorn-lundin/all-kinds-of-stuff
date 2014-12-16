
with Text_IO;
with USB; use USB;
with Interfaces.C;
with Interfaces.C.Strings;

procedure USB_Client is
  package C renames Interfaces.C;

  Bus           : USB_Bus_Pointer            := null;
  Device_Handle : USB_Device_Handle_Pointer  := null;
  Device        : USB_Device_Pointer         := null;
  Takeover_Device_Failure : exception;
  --------------------------------------------
  procedure Debug(What : String) is
  begin
    Text_io.Put_Line("Client: " & What);
  end Debug;
  --------------------------------------------
   procedure Takeover_Device(Device_Handle : USB_Device_Handle_Pointer; An_Interface : C.Int) is
      use type C.Int;
      use C.Strings;
      Ret : C.Int := -1;
      Driver_Name : String(1..256) := (others => Ascii.Nul);
      C_Driver_Name : Chars_Ptr := New_String(Driver_Name);
   begin
      Ret := USB_Get_Driver(Device_Handle,An_Interface,C_Driver_Name,256);
      Free(C_Driver_Name);
      if Ret < 0 then
         Debug("USB_Get_Driver : "& Value(USB_Strerror));
      end if;

      if USB_Detach_Kernel_Driver(Device_Handle, An_Interface) < 0 then
         Debug("USB_Detach_Kernel_Driver : "& Value(USB_Strerror));
      else
         Debug("USB_Detach_Kernel_Driver -> OK");
      end if;

      Ret :=  USB_Set_Alternative_Interface(Device_Handle, An_Interface);
      if USB_Claim_Interface(Device_Handle, An_Interface) < 0 then
         Debug("USB_Claim_Interface : "& Value(USB_Strerror));
         raise Takeover_Device_Failure;
      end if;
      Debug("Took over the device");

   end Takeover_Device;
   --------------------------------------------------------------------------------------




   Device_Taken_Over : Boolean := False;
   Result            : C.Int   := 0;
begin
  Debug("Client - usb_init");
  usb_init;
  Debug("usb_set_debug(2)");
  usb_set_debug(2);


  Debug("usb_find_busses");
  usb_find_busses;
  Debug("usb_devices");
  usb_find_devices;

  Bus := USB_Get_Busses ;
  Debug("bus is null: " & boolean'image(bus = null));
  Bus_Loop: loop
    exit when Bus = null;
    Device := Bus.Devices;
    Debug("in bus loop, devices=null: " & boolean'image(bus.devices = null));
    for i in Bus.Dirname'range loop
      text_io.put(c.char'image(bus.dirname(i)));
      exit when c.char'image(bus.dirname(i)) = "NUL";
    end loop;
    text_io.new_line;
    Device_Loop: loop
      exit when Device = null;
      Debug("  in device loop");
      for i in Device.Filename'range loop
        text_io.put(c.char'image(Device.Filename(i)));
        exit when c.char'image(bus.dirname(i)) = "NUL";
      end loop;
      text_io.new_line;
      Debug("  idVendor, IdProduct:" &
--               uint8_t'image(Device.Descriptor.blength) & ",");-- &
               uint16_t'image(Device.Descriptor.IdVendor) & "," &
            uint16_t'image(Device.Descriptor.IdProduct)) ;
         if Device.Descriptor.IdVendor = 16#10cf# and then
           Device.Descriptor.IdProduct = 16#5500# then
             Device_Handle := Usb_Open(Device);
             begin
               Takeover_Device(Device_Handle, 0);
               Device_Taken_Over := True;
               exit Bus_Loop;
             exception
               when Takeover_Device_Failure =>
                  Debug("Takeover_Device_Failure");
             end;
         end if;
      Device := Device.Next;
    end loop Device_Loop;
    Bus := Bus.Next;
  end loop Bus_Loop;

--  Device_Handle := C_Locate_Xsv ;


--  Debug("usbclose");
  if Device_Taken_Over then

    Result := USB_Close(Device_Handle);
    Debug("Result USB_CLose :" & C.Int'image(Result));
  end if;
  Debug("done");

end USB_Client;

