--  std Ada type definitions, mostly needed for Unsigned_8
with Interfaces;                   use Interfaces;
with AVR;                          use AVR;
with AVR.Wait;
with AVR.MCU; --  address and bit name constants for the MCU
with AVR.UART; --  routines to send and receive data across the serial line
--with AVR.Interrupts;
with Ada_Interrupts_Def;

procedure Ada_Interrupts is
   ------------------------------------------------
   Trigg_Pin      : constant Bit_Number := 4;
   Echo_Pin       : constant Bit_Number := 5;
   ------------------------------------------------
   procedure Wait_1_us is new
     AVR.Wait.Generic_Wait_Usecs (Crystal_Hertz => 16_000_000,
                                  Micro_Seconds => 1);
   ------------------------------------------------
   procedure Wait(us : Unsigned_16) is
   begin -- delay 1.0;
      for J in 1 .. us loop
         Wait_1_us;
      end loop;
   end Wait;
   ------------------------------------------------
   procedure Send_Trigg is
   begin
   UART.Put_Line ("Send_Trigg");  
     MCU.PORTD_Bits (Trigg_Pin) := High;
     Wait(us => 12);
     MCU.PORTD_Bits (Trigg_Pin) := Low;
   end Send_Trigg;
   ------------------------------------------------
   Cnt : Unsigned_8 := 0; 
   Echo_DD      : Boolean renames MCU.DDRD_Bits(Echo_Pin);
   Echo_Pull_Up : Boolean renames MCU.PortD_Bits(Echo_Pin);
   Echo         : Boolean renames MCU.PinD_Bits(Echo_Pin);
   Old_Echo     : Boolean;
begin
   AVR.UART.Init (25);            -- Baud rate = 38400bps, 16MHZ, u2x=0, 8n1

   UART.Put_Line ("Init the pins");  
   
   MCU.DDRD_Bits  := (others => DD_Output);
   MCU.PortD_Bits := (others => Low);
   
   Echo_DD      := DD_Input;
   Echo_Pull_Up := High; -- activate pullup ?
   Old_Echo := Echo;

   Ada_Interrupts_Def.Init; -- start timer
   loop
     Send_Trigg;
--   Trigg_up : loop
--     exit Trigg_up when Echo /= Old_Echo;
--   end loop Trigg_up;
--   Old_Echo := Echo;   
--   Trigg_Down : loop
--     exit Trigg_Down when Echo /= Old_Echo;
--   end loop Trigg_Down;
--   UART.Put (MCU.TCNT0);  --read timer value, ie proportinal to the time elapsed
--   UART.New_Line;
   
     for i in 1 .. 100 loop
       Wait(us => 50_000);
       Wait(us => 50_000);
     end loop;
     UART.Put_Line ("done");  
   end loop;
end Ada_Interrupts;
