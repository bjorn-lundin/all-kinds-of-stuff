
with Interfaces;                   use Interfaces;
with AVR;                          use AVR;
with AVR.Wait;
with AVR.UART; --  routines to send and receive data across the serial line

with Ada_Interrupts_Def;

procedure Ada_Interrupts is

   procedure Wait_1_us is new
     AVR.Wait.Generic_Wait_Usecs (Crystal_Hertz => 16_000_000,
                                  Micro_Seconds => 1);

   procedure Wait(us : Unsigned_16) is
   begin 
      for J in 1 .. us loop
         Wait_1_us;
      end loop;
   end Wait;
   
   procedure Wait_1_Second is
   begin 
      for J in 1 .. 10 loop
         Wait(50_000);
         Wait(50_000);
      end loop;
   end Wait_1_Second;
         
begin
   AVR.UART.Init (25);            -- Baud rate = 38400bps, 16MHZ, u2x=0, 8n1
   UART.Put_Line ("init and wait 1 s");  
   Ada_Interrupts_Def.Init;
   for i in 1 .. 10 loop -- wait 10 secs
     Wait_1_Second;
   end loop;
   Ada_Interrupts_Def.Finish;
   UART.Put_Line ("done waiting");  
end Ada_Interrupts;
------------------------------------------------------------------

with Interfaces;                   use Interfaces;
with AVR;                          use AVR;
with AVR.MCU;  --  address and bit name constants for the MCU
with AVR.UART; --  routines to send and receive data across the serial line
with AVR.Interrupts;

package body Ada_Interrupts_Def is
   Toggle_Pin : constant Bit_Number := 7;
   Toggle     : Boolean := True;
   Counter    : Unsigned_16 :=0;
   Overflow   : constant Unsigned_8 := 255;
   ---------------------------------

   procedure Timer_A_Tick is
   begin
     UART.Put_Line ("Timer_A_Tick");  
     --set up next timer event
     Timer_A.Init_CTC(Prescaler => Timer_A.Scale_By_1024, Overflow => Overflow);
     -- do the toggle
     MCU.PinD_Bits (Toggle_Pin) := Toggle;
     Toggle := not Toggle;
     -- count all events and print number at end
     Counter := Counter +1;
   end Timer_A_Tick;
   ---------------------------------

   procedure Init is
   begin
     MCU.DDRD_Bits (Toggle_Pin)  := DD_Output;
     MCU.PortD_Bits (Toggle_Pin) := High;
     Counter := 0;
     AVR.Interrupts.Enable_Interrupts;
     --set up first timer event
     Timer_A.Init_CTC(Prescaler => Timer_A.Scale_By_1024, Overflow => Overflow); 
   end Init;
   ---------------------------------
   procedure Finish is
   begin
     --disable pin change
     AVR.Interrupts.Disable_Interrupts;
     UART.Put ("Finish - Counter = ");  
     UART.Put (Counter);  
     UART.New_Line;  
     MCU.PinD_Bits (Toggle_Pin) := Low;
   end Finish;
   
end Ada_Interrupts_Def;

---------------------------------------------------------------

with AVR;     use AVR;
with AVR.Timer0;

package Ada_Interrupts_Def is

  package Timer_A renames AVR.Timer0;
   ---------------------------------
   procedure Timer_A_Tick;
   pragma Machine_Attribute (Entity         => Timer_A_Tick,
                             Attribute_Name => "signal");
   pragma Export (Convention    => C,
                  Entity        => Timer_A_Tick,
                  External_Name => Timer_A.Signal_Compare);
   ---------------------------------
   procedure Init ;
   procedure Finish ;
   
end Ada_Interrupts_Def;
