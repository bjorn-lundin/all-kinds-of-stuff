--  
--  Sample program that reads the temperatures from all attached
--  1-Wire temperature sensors and prints the values to the serial
--  interface.
--

--  std Ada type definitions, mostly needed for Unsigned_8
with Interfaces;                   use Interfaces;

with AVR;                          use AVR;
with AVR.Wait;
--  address and bit name constants for the MCU
with AVR.MCU;
--  routines to send and receive data across the serial line
with AVR.UART;

--  Dallas 1-Wire definitions and routines for a bus master
with One_Wire;                     use One_Wire;
with One_Wire.Search;
with One_Wire.ROM;
with One_Wire.Temperature_Sensors; use One_Wire.Temperature_Sensors;
with Crc8;

--  temperature types using Ada's fixed point capabilities
with Temperatures;                 use Temperatures;

procedure Ada_DS1820 is

   Found : Boolean;

   procedure Wait_1ms is new
     AVR.Wait.Generic_Wait_Usecs (Crystal_Hertz => 16_000_000,
                                  Micro_Seconds => 1000);

   --  workaround until a real Ada delay statement can be used
   procedure Wait_Long is
   begin -- delay 1.0;
      for J in 1 .. 1_000 loop
         Wait_1ms;
      end loop;
   end Wait_Long;
   
   procedure Wait_Very_Long is
   begin -- delay 10;
      for J in 1 .. 10_000 loop
         Wait_1ms;
      end loop;
   end Wait_Very_Long;

   --  this demo can handle up to 5 devices
   Max_OW_Devices : constant := 10;
   subtype OW_Device_Range       is Unsigned_8      range 0 .. Max_OW_Devices;
   subtype OW_Valid_Device_Range is OW_Device_Range range 1 .. Max_OW_Devices;

   type OW_Device_A is array (OW_Valid_Device_Range)
     of One_Wire.ROM.Unique_Serial_Code;
   type OW_Temp_A is array (OW_Valid_Device_Range) of Temperature_12bit;


   OW_Devices : OW_Device_A;  -- array of sensor rom codes
   OW_Temps   : OW_Temp_A;    -- array of corresponding temperatures

   OW_Sensor_Index : OW_Device_Range;
   Last_Sensor     : OW_Device_Range;
   Crc             : Unsigned_8;
   Port            : constant Bit_Number := 2;
begin
   --  provide high voltage at DPort as power supply in non-parasite mode
   MCU.DDRD_Bits (Port) :=  DD_Output;
   MCU.PORTD_Bits (Port) := Low;

   --
--   AVR.UART.Init (51);          -- Baud rate = 9600bps, 8MHZ, u2x=0
   AVR.UART.Init (25);            -- Baud rate = 38400bps, 16MHZ, u2x=0, 8n1
   UART.Put_Line ("Init 1-Wire");  
   One_Wire.Init_Comm;
   UART.Put_Line ("starting RE's 1-Wire to serial output test program");
   loop
      UART.New_Line;
      UART.Put_Line ("--> Test 1-wire to serial output");

      OW_Sensor_Index := 1;
      Last_Sensor     := 0;

      MCU.PORTD_Bits (Port) := High;
      Wait_Long;
      --  first find all devices
      Found := One_Wire.Search.First;
      if Found then
         loop
            --  copy the rom code to our array
            OW_Devices (OW_Sensor_Index) := One_Wire.ROM.Identifier;
            --  increment the sensor index
            Last_Sensor := OW_Sensor_Index;
            OW_Sensor_Index := OW_Sensor_Index + 1;

            --  search the next device
            Found := One_Wire.Search.Next;
            exit when not Found;
         end loop;
      else
         Uart.Put_Line ("no device");
      end if;
      -- Power_Off;
      --  print list of found IDs
      --  <sensor index> :  <ROM code in hex>
      for Idx in 1 .. Last_Sensor loop
         UART.Put ("ID ");
         UART.Put (Nat8 (Idx));
         UART.Put (":  ");
         Crc := 0;
         for J in One_Wire.ROM.Serial_Code_Index loop
            Crc := Crc8 (Data => OW_Devices (Idx)(J),
                         Seed => Crc);
            Uart.Put (Nat8 (OW_Devices (Idx)(J)), Base => 16);
            Uart.Put (' ');
         end loop;
         Uart.Put ("CRC: ");
         Uart.Put (Nat8 (Crc), Base => 16);
         Uart.New_Line;
      end loop;
      Wait_Long;

      --  start conversion for all sensors
      One_Wire.ROM.Identifier (1) := 0;
      --  see comment in One_Wire.Temperature_Sensors.  If the first
      --  byte of the ROM code is zero, send the command to start
      --  temperature sampling to all temperature sensors.
      One_Wire.Temperature_Sensors.Init_T_Conversion;

      --  leave enough time for the temperature conversion. (750ms for
      --  12 bit!  see the DS18B20 data sheet.)
      Wait_Long;

      --  request temp reading
      for Idx in 1 .. Last_Sensor loop
         --  set the rom code
         One_Wire.ROM.Identifier := OW_Devices (Idx);
         --  and read the temperature
         if One_Wire.ROM.Identifier (1) = Family_Code (DS18S20) then
            OW_Temps (Idx) := To_Temperature_12bit (Read_Raw_Value * 8);
         else
            OW_Temps (Idx) := To_Temperature_12bit (Read_Raw_Value);
         end if;
      end loop;

      --  print list of temperature readings
      for Idx in 1 .. Last_Sensor loop
         Uart.Put ("ID ");
         Uart.Put (Nat8 (Idx));
         Uart.Put (": T = ");
         Uart.Put (Image (OW_Temps (Idx)));
         Uart.Put ('C');
         Uart.New_Line;
      end loop;
      Uart.Put_Line ("Test - off");
      MCU.PORTD_Bits (Port) := Low;
      Wait_Long;   
   end loop;

end Ada_DS1820;
