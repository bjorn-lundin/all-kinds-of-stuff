with Interfaces;                   use Interfaces;

with AVR;                          use AVR;
--  address and bit name constants for the MCU
with AVR.MCU;
--  routines to send and receive data across the serial line
with AVR.UART;
with AVR.Interrupts;
package body Ada_Interrupts_Def is
   Is_Running_Pin : constant Bit_Number := 3;
   Toggle_Pin     : constant Bit_Number := 7;
   Toggle : Boolean := True;
   Counter : Unsigned_16 :=0;
   ---------------------------------
   procedure Pin_Change_Fired is
   begin
     MCU.PortD_Bits (Toggle_Pin) := Toggle;
     Toggle := not Toggle;
     UART.Put_Line ("Pin_Change_Fired");  
--     for i in AVR.MCU.PCMSK0_Bits'range loop
--       UART.Put("bit ");  
--       UART.Put(unsigned_8(i));  
--       UART.Put(" value ");  
--       if AVR.MCU.PCMSK0_Bits(i) then 
--         UART.Put(" t ");  
--       else
--         UART.Put(" f ");  
--       end if;       
--       UART.New_Line; 
--     end loop;
   end Pin_Change_Fired;   
   ---------------------------------
   procedure Timer_A_Tick is
   begin
--     UART.Put_Line ("Timer_A_Tick");  
--     Timer_A.Init_CTC(Prescaler => Timer_A.Scale_By_1024, Overflow => 255);
--     
--     MCU.PinD_Bits (7) := Toggle;
--     Toggle := not Toggle;
--          
     Counter := Counter +1;
   end Timer_A_Tick;
   ---------------------------------

   procedure Init is
   begin
     Counter := 0;
     AVR.Interrupts.Enable_Interrupts;
     Timer_A.Init_CTC(Prescaler => Timer_A.Scale_By_1024, Overflow => 255); 
     --enable pin change
     
     MCU.PCMSK0_Bits (MCU.PCINT0_Bit) := True; -- enable external int0
     MCU.MCUCR_Bits (MCU.ISC01_Bit)   := True; -- falling egde: int0
     
--     MCU.PCICR_Bits(MCU.PCIE0_Bit) := High;
--     MCU.PCMSK0_Bits(MCU.PCINT5_Bit) := High;
--     MCU.PCMSK0_Bits(MCU.PCINT4_Bit) := High;
--     MCU.PCMSK0 := MCU.PCINT5_Mask;

     MCU.PinD_Bits (Is_Running_Pin) := High;
   end Init;
   ---------------------------------
   procedure Finish is  
   begin
     AVR.Interrupts.Disable_Interrupts;
     UART.Put ("Finish - Counter = ");  
     UART.Put (Counter);  
     UART.New_Line;  
   end Finish;
   
end Ada_Interrupts_Def;
