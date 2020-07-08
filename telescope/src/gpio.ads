
with Interfaces.C; use Interfaces.C;

package Gpio is

  -- wiringPi modes

  Wpi_Mode_Pins           : constant Interfaces.C.Int :=  0;
  Wpi_Mode_Gpio           : constant Interfaces.C.Int :=  1;
  Wpi_Mode_Gpio_Sys       : constant Interfaces.C.Int :=  2;
  Wpi_Mode_Phys           : constant Interfaces.C.Int :=  3;
  Wpi_Mode_Piface         : constant Interfaces.C.Int :=  4;
  Wpi_Mode_Uninitialised  : constant Interfaces.C.Int := -1;

  -- Pin modes

  Input                   : constant Interfaces.C.Int := 0;
  Output                  : constant Interfaces.C.Int := 1;
  Pwm_Output              : constant Interfaces.C.Int := 2;
  Gpio_Clock              : constant Interfaces.C.Int := 3;
  Soft_Pwm_Output         : constant Interfaces.C.Int := 4;
  Soft_Tone_Output        : constant Interfaces.C.Int := 5;
  Pwm_Tone_Output         : constant Interfaces.C.Int := 6;

  Low                     : constant Interfaces.C.Int := 0;
  High                    : constant Interfaces.C.Int := 1;

  -- Pull up/down/none

  Pud_Off                 : constant Interfaces.C.Int := 0;
  Pud_Down                : constant Interfaces.C.Int := 1;
  Pud_Up                  : constant Interfaces.C.Int := 2;

  -- PWM

  Pwm_Mode_Ms             : constant Interfaces.C.Int := 0;
  Pwm_Mode_Bal            : constant Interfaces.C.Int := 1;

  -- Interrupt levels

  Int_Edge_Setup          : constant Interfaces.C.Int := 0;
  Int_Edge_Falling        : constant Interfaces.C.Int := 1;
  Int_Edge_Rising         : constant Interfaces.C.Int := 2;
  Int_Edge_Both           : constant Interfaces.C.Int := 3;

  Bad_Gpio_Call : exception;

  procedure Setup ;

  procedure Pin_Mode(Pin : Interfaces.C.Int ; Mode : Interfaces.C.Int) ;

  procedure Digital_Write(Pin : Interfaces.C.Int; Value : Boolean);
  function  Digital_Read(Pin :  Interfaces.C.Int) return Boolean;


private
  pragma Import(C, Pin_Mode, "pinMode");


end Gpio;
