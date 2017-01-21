
with Ada.Environment_Variables;

package body Gpio is

  ---------------------------------------------------------
  procedure Setup is
    R : Int := 0;

    function Wiring_Pi_Setup_Gpio return Interfaces.C.Int ;
    pragma Import(C, Wiring_Pi_Setup_Gpio, "wiringPiSetupGpio");

  begin --http://wiringpi.com/reference/setup/
    -- If you want to restore the v1 behaviour, then you need to set the environment variable: WIRINGPI_CODES
    -- to any value
    Ada.Environment_Variables.Set("WIRINGPI_CODES","1");
    R := Wiring_Pi_Setup_Gpio;
    if R /= 0 then
      raise Bad_Gpio_Call with "Wiring_Pi_Setup_Gpio" & R'Img;
    end if;
  end Setup;
  ---------------------------------------------------------
  procedure Digital_Write(Pin : Interfaces.C.Int; Value : Boolean) is
    procedure Digital_Write(Pin : Interfaces.C.Int; Value :Interfaces.C.Int);
    pragma Import(C, Digital_Write, "digitalWrite");

  begin
    if Value then
      Digital_Write(Pin, High);
    else
      Digital_Write(Pin, Low);
    end if;
  end Digital_Write;
  ------------------------------------------------------
end Gpio;
