with Ada.Environment_Variables;

package body Gpio is
  
  ---------------------------------------------------------
  procedure Setup is
     R : Int := 0;
     
    function Wiring_Pi_Setup_Gpio return Interfaces.C.Int ;
    pragma Import(C, Wiring_Pi_Setup_Gpio, "wiringPiSetupGpio");
     
  begin
    -- to actually get the errorcodes ... 
    if not Ada.Environment_Variables.Exists("WIRINGPI_CODES") then
      Ada.Environment_Variables.Set("WIRINGPI_CODES","1"); -- to get the error codes
    end if;    

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
      Digital_Write(Pin, 1);
    else
      Digital_Write(Pin, 0);
    end if;
  end Digital_Write;
  ------------------------------------------------------
  function Digital_Read(Pin : Interfaces.C.Int) return Boolean is
    function Digital_Read(Pin : Interfaces.C.Int) return Interfaces.C.Int;
    pragma Import(C, Digital_Read, "digitalRead");
  begin
     return Digital_Read(Pin) = 1 ;
  end Digital_Read;
  ------------------------------------------------------
  
  
  
end Gpio;
