with Text_Io; use Text_Io;
with k8055; use k8055;

procedure Temp_Reader is
  Device : Longint;
  Counter : Integer := 0;
  Temperature : Data_Type := 0;
begin
  Device := OpenDevice(0);
  Put_Line ("Device:" & Longint'Image(Device));

  WriteAllDigital(0);

  --give sensor power
  OutputAnalogChannel(1,255);

  loop
    exit when Counter > 100;
    Counter := Counter +1;
    Temperature := ReadAnalogChannel(1);  
    Put_Line ("Temp value:" & Data_Type'Image(Temperature));
    delay 1.0; 
  end loop; 

  Put_Line("Shutdown...");

  --remove sensor power
  OutputAnalogChannel(1,0);
  CloseDevice;

end Temp_Reader;
