
with Interfaces.C; use Interfaces.C;

package Gpio is

  -- wiringPi modes

  WPI_MODE_PINS           : constant Interfaces.C.Int :=  0;
  WPI_MODE_GPIO           : constant Interfaces.C.Int :=  1;
  WPI_MODE_GPIO_SYS       : constant Interfaces.C.Int :=  2;
  WPI_MODE_PHYS           : constant Interfaces.C.Int :=  3;
  WPI_MODE_PIFACE         : constant Interfaces.C.Int :=  4;
  WPI_MODE_UNINITIALISED  : constant Interfaces.C.Int := -1;

  -- Pin modes

  INPUT                   : constant Interfaces.C.Int := 0;
  OUTPUT                  : constant Interfaces.C.Int := 1;
  PWM_OUTPUT              : constant Interfaces.C.Int := 2;
  GPIO_CLOCK              : constant Interfaces.C.Int := 3;
  SOFT_PWM_OUTPUT         : constant Interfaces.C.Int := 4;
  SOFT_TONE_OUTPUT        : constant Interfaces.C.Int := 5;
  PWM_TONE_OUTPUT         : constant Interfaces.C.Int := 6;

--  LOW                     : constant Interfaces.C.Int := 0;
--  HIGH                    : constant Interfaces.C.Int := 1;
  LOW                     : constant Boolean := False;
  HIGH                    : constant Boolean := True;

  -- Pull up/down/none

  PUD_OFF                 : constant Interfaces.C.Int := 0;
  PUD_DOWN                : constant Interfaces.C.Int := 1;
  PUD_UP                  : constant Interfaces.C.Int := 2;

  -- PWM

  PWM_MODE_MS             : constant Interfaces.C.Int := 0;
  PWM_MODE_BAL            : constant Interfaces.C.Int := 1;

  -- Interrupt levels

  INT_EDGE_SETUP          : constant Interfaces.C.Int := 0;
  INT_EDGE_FALLING        : constant Interfaces.C.Int := 1;
  INT_EDGE_RISING         : constant Interfaces.C.Int := 2;
  INT_EDGE_BOTH           : constant Interfaces.C.Int := 3;

  Bad_GPIO_Call : exception;

  procedure Setup ;

  procedure Pin_Mode(Pin : Interfaces.C.Int ; Mode : Interfaces.C.Int) ;
  procedure Pull_Up_Dn_Control(Pin : Interfaces.C.Int ; Mode : Interfaces.C.Int) is null;

  procedure Digital_Write(Pin : Interfaces.C.Int; Value : Boolean);
  function Digital_Read(Pin : Interfaces.C.Int) return Boolean;




end Gpio;
