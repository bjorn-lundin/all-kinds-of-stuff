--  std Ada type definitions, mostly needed for Unsigned_8
with Interfaces;                   use Interfaces;

with AVR;                          use AVR;
with AVR.Wait;
--  address and bit name constants for the MCU
with AVR.MCU;
--  routines to send and receive data across the serial line
with AVR.UART;


procedure Ada_Distance is

   -- speed of sound in air
   -- c = 331.5 + 0.6 * [air temperature in degrees Celsius]
   -- use 20 C  and use cm/us -> 
   -- c = 343.5 * 10 * 100 / 1000000 = 0.3435 mm/us
   c : constant Float :=  0.3435;
   Trigg_Pin : constant Bit_Number := 4;
   Echo_Pin  : constant Bit_Number := 5;
   Count     : Unsigned_16 := 0;
   -- D in mm  --  D = (t/2) * c  where t is the time to go there and back.
   D : Unsigned_16 :=  0;
   
   procedure Wait_1_us is new
     AVR.Wait.Generic_Wait_Usecs (Crystal_Hertz => 16_000_000,
                                  Micro_Seconds => 1);

   --  workaround until a real Ada delay statement can be used
   procedure Wait(us : Unsigned_16) is
   begin -- delay 1.0;
      for J in 1 .. us loop
         Wait_1_us;
      end loop;
   end Wait;
   
   procedure Wait_1_Second is
   begin -- delay 1.0;
      for J in 1 .. 1_000 loop
         Wait(Us => 1_000);
      end loop;
   end Wait_1_Second;

begin

   AVR.UART.Init (25);            -- Baud rate = 38400bps, 16MHZ, u2x=0, 8n1

   UART.Put_Line ("Init the pins");  
   MCU.DDRD_Bits (Trigg_Pin)  := DD_Output;
   MCU.PortD_Bits (Trigg_Pin) := Low;

   MCU.DDRD_Bits (Echo_Pin) := DD_Input;
   MCU.PortD_Bits(Echo_Pin) := High; -- activate pullup ?
   
   The_Big_Loop : loop
   
     UART.Put_Line ("Send trig");  
   
     MCU.PORTD_Bits (Trigg_Pin) := High;
     Wait(us => 10);
     MCU.PORTD_Bits (Trigg_Pin) := Low;
     Count := 0;   
     
     UART.Put_Line ("Wait for high");  
     Wait_For_High : loop
       exit Wait_For_High when MCU.PORTD_Bits (Echo_Pin) = High or else Count = Unsigned_16'Last;
       Count := Count +1;     
     end loop Wait_For_High;
     
     if Count = Unsigned_16'Last then
       UART.Put_Line ("After Wait for High - Unsigned_16'Last"); 
     end if;
     
     Count := 0;   
     Wait_For_Low : loop
       exit Wait_For_Low when MCU.PORTD_Bits (Echo_Pin) = Low or else Count = Unsigned_16'Last;
       Wait(us => 1_000);
       Count := Count +1;     
     end loop Wait_For_Low;
     
     if Count = Unsigned_16'Last then 
       UART.Put_Line ("After Wait for Low - Unsigned_16'Last"); 
     end if;
     
     D := Unsigned_16 ( ( Float(Count) / 2.0 ) * c);
     UART.Put ("D= "); 
     UART.Put(D); 
     UART.Put_Line (" mm"); 
     
     Wait_1_Second;
   end loop The_Big_Loop;

end Ada_Distance;
