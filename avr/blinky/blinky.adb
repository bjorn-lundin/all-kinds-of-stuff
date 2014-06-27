
with AVR.MCU;
with AVR.Real_Time.Delays; 
use AVR;
use AVR.Real_Time;

procedure Blinky is

    YLED_Pin13 : Boolean renames MCU.PortB_Bits (5);

    Yellow_On    : AVR.Real_Time.Duration := 1.0/2; -- Delay_For () waits for 2 X argument value!
    Yellow_Off   : AVR.Real_Time.Duration := 5.0/2; -- Delay_For () waits for 2 X argument value!

    -- flip the yellow led output pin
    procedure Flip_Yellow is    
    begin   
       YLED_Pin13 := not YLED_Pin13;
    end Flip_Yellow;
   
    -- flash the yellow led
    procedure Flash_Yellow (On_Time  : in AVR.Real_Time.Duration;
                            Off_Time : in AVR.Real_Time.Duration) is       
    begin
       Flip_Yellow;
       AVR.Real_Time.Delays.Delay_For (On_Time);
       Flip_Yellow;
       AVR.Real_Time.Delays.Delay_For (Off_Time);   
    end Flash_Yellow;

begin -- setup
    MCU.DDRB_Bits := (others => DD_Output);
    YLED_Pin13    := Low;  -- LED on
      
    -- start blink loop
    loop -- flash the yellow led
        Flash_Yellow (On_Time  => Yellow_On,
                      Off_Time => Yellow_Off);
    end loop;
end Blinky;
