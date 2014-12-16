
package Gameport_Io is

  Port_Access_Failure : exception;

  type Byte is range   0..2**8 - 1;
  for Byte'Size use 8;

  type Joystick_Type is (Joystick_1,Joystick_2);

  type Bit_Type is (A, B, X, Y);

  type Direction_Type is (Up,      Up_Right,   Right,   Down_Right,
                          Down,    Down_Left,  Left,    Up_Left,
                          Straight);

  type Position_Type is record
    X : Integer := 0;
    Y : Integer := 0;
  end record;

  procedure Get_Gameport_Access;
  procedure Release_Gameport_Access;

  procedure Set_Gameport(Value : in Byte);
  function  Get_Gameport return Byte;

  function  Is_Pressed(Joystick : in Joystick_Type;
                       Bit      : in Bit_Type;
                       Value    : in Byte := 0) return Boolean;

  function  Is_Zero(Joystick : in Joystick_Type;
                    Bit      : in Bit_Type;
                    Value    : in Byte := 0) return Boolean ;

  function  Get_Position(Joystick : Joystick_Type;
                         Samples  : Integer := 1) return Position_Type ;

  function  Get_Bit_Number(Joystick : in Joystick_Type;
                           Bit      : in Bit_Type      ) return Integer;

  function  Get_Direction(Position : Position_Type) return Direction_Type;

-- C-FUNCTIONS
--void perror(const char *s);
  procedure Perror(Text : in String);

--  int ioperm(unsigned long from, unsigned long num, int turn_on);
  function Ioperm(From,Num : in Integer ; Turn_On : in Boolean) return Boolean;

-- port access functions
  procedure Outb(Value : in Byte ; Port : in Integer);
  function  Inb(Port : in Integer) return Byte;

end Gameport_Io;
