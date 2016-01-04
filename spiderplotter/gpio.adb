
--with Gpio.Thin ;

package body Gpio is
  
  ---------------------------------------------------------
  procedure Setup is
     R : Int := 0;
     
    function Wiring_Pi_Setup_Gpio return Interfaces.C.Int ;
    pragma Import(C, Wiring_Pi_Setup_Gpio, "wiringPiSetupGpio");
     
  begin
    R := Wiring_Pi_Setup_Gpio;
    if R /= 0 then 
      raise Bad_GPIO_Call with "Wiring_Pi_Setup_Gpio" & R'Img;
    end if;
  end Setup;
  ---------------------------------------------------------
  procedure Digital_Write(Pin : Interfaces.C.Int; Value : Boolean) is
    procedure Digital_Write(Pin : Interfaces.C.Int; Value :Interfaces.C.Int);
    pragma Import(C, Digital_Write, "digitalWrite");
  
  begin
    if Value then
      Digital_Write(Pin, HIGH);
    else  
      Digital_Write(Pin, LOW);
    end if;  
  end Digital_Write;
  ------------------------------------------------------
end Gpio;
