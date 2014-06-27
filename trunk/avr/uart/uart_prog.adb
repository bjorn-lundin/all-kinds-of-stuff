
with AVR;                          use AVR;
with AVR.UART;
with AVR.Strings;

with AVR.MCU;
with AVR.Real_Time; use  AVR.Real_Time;
with AVR.Real_Time.Delays; 

with AVR.Int_Img;

with Interfaces;             use Interfaces; 

procedure Uart_Prog is

   Cnt      : Nat32 := Nat32'First; 
   Cnt_Str  : Strings.AStr10;
   Cnt_Last : Unsigned_8;

   subtype Alfa_Type is Character range 'a' .. 'z' ;
   
   Stop : Strings.Avr_String := "Stop";
   use type Strings.Avr_String;
   
   Red_LED_Pin13 : Boolean renames MCU.PortB_Bits (5);

   Red_On    : Real_Time.Duration := 1.0/10; -- Delay_For () waits for 2 X argument value!
   Red_Off   : Real_Time.Duration := 1.0/10; -- Delay_For () waits for 2 X argument value!

   -- flip the Red led output pin
   procedure Flip_Red is    
   begin   
      Red_LED_Pin13 := not Red_LED_Pin13;
   end Flip_Red;
  
   -- flash the Red led
   procedure Flash_Red (On_Time  : in Real_Time.Duration;
                        Off_Time : in Real_Time.Duration) is       
   begin
      Flip_Red;
      Real_Time.Delays.Delay_For (On_Time);
      Flip_Red;
      Real_Time.Delays.Delay_For (Off_Time);   
   end Flash_Red;  
   
begin

   -- 103 -->  9600Bd @ 16MHz
   --  51 --> 19200Bd @ 16MHz
   --  25 --> 38400Bd @ 16MHz

   UART.Init(25);
   
   MCU.DDRB_Bits := (others => DD_Output);
   Red_LED_Pin13    := Low;  -- LED on

  loop
    if Cnt < Nat32'Last then
      Cnt := Cnt +1;
    else
      Cnt := Nat32'First;
    end if;    
  
    for i in Alfa_Type'range loop
      UART.Put(i);
--      Test(8) := i;
--      UART.Put_Line (Test);
    end loop;
    UART.New_Line;
    Int_Img.U32_Img(Data => Cnt, Target => Cnt_Str, Last => Cnt_Last);
    
    UART.Put_Line (Cnt_Str(1 .. Cnt_Last));
    UART.New_Line;
    Flash_Red (On_Time  => Red_On,
               Off_Time => Red_Off);
   end loop;

end Uart_Prog;

