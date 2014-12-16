with Ada.Unchecked_Conversion;
with Ada.Text_io ; use Ada.Text_io;


package body Gameport_Io is

  GAMEPORT : constant Integer := 16#201# ;--  joystick address


  type Bit_Array is array (0..7) of Boolean;
  pragma pack(Bit_Array);

  function To_bit_array is new Ada.Unchecked_Conversion(Byte, Bit_Array);
  function To_byte      is new Ada.Unchecked_Conversion(Bit_Array, Byte);


-- c-related functions---------------------------
--  int ioperm(unsigned long from, unsigned long num, int turn_on);
  function C_Ioperm(From, Num, Turn_On : in Integer ) return Integer ;
  pragma   Import(C,C_Ioperm,"ioperm");

  function Ioperm(From, Num : in Integer ; Turn_On : in Boolean) return Boolean is
   i : Integer := 2;
  begin
    i := C_Ioperm(From,Num,Boolean'pos(Turn_On));
    return (i /= -1);
  end Ioperm;



  procedure C_Outb(Value : in Byte ; Port : in Integer);
  pragma    Import(C,C_outb,"c_outb");

  procedure Outb(Value : in Byte ; Port : in Integer) is
  begin
    c_outb(Value,Port);
  end Outb;


  function C_Inb(Port : in Integer) return Byte;
  pragma   Import(C,C_Inb,"c_inb");

  function Inb(Port : in Integer) return Byte is
  begin
    return C_Inb(Port);
  end Inb;



--void perror(const char *s);
  procedure C_Perror(Text : in String);
  pragma    Import(C,C_perror,"perror");

  procedure Perror(Text : in String) is
  begin
    C_Perror(Text & Ascii.Nul);
  end Perror;


-- Ada functions --------------------------------------------------------------------


  procedure Print_Bit_Array(Arr : in Bit_Array) is
  begin
    for i in Arr'range loop
      Put(i'img & "-" & Boolean'image(arr(i)) & "|");
    end loop;
    New_Line;
  end Print_Bit_Array;

------------------------------------------------------------------------------------

  function Is_Pressed(Joystick : in Joystick_Type;
                      Bit      : in Bit_Type;
                      Value    : in Byte := 0) return Boolean is
    Bit_Number  : Integer := 0;
    Local_Value : Byte := 0;
    Bit_Value   : Bit_Array;
  begin
    if Value = 0 then
      Local_Value := Get_Gameport;
    else
      Local_Value := Value;
    end if;

    Bit_number := Get_Bit_Number(Joystick, Bit);
    Bit_value := To_bit_array(Local_Value);
--    Put_line("Bit_value");
--    Print_bit_array(Bit_value);
    return not Bit_Value(Bit_Number);
  end Is_Pressed;

------------------------------------------------------------------------------------

  function Is_Zero(Joystick : in Joystick_Type;
                   Bit      : in Bit_Type;
                   Value    : in Byte := 0            ) return Boolean is
  begin
    return  Is_pressed(Joystick, Bit, Value);
  end Is_zero;

------------------------------------------------------------------------------------

  function Get_Bit_Number(Joystick : in Joystick_Type;
                          Bit      : in Bit_Type      ) return Integer is
  begin
    case Joystick is
      when Joystick_1 =>
        case Bit is
          when A => return 4;
          when B => return 5;
          when X => return 0;
          when Y => return 1;
        end case;
      when Joystick_2 =>
        case Bit is
          when A => return 6;
          when B => return 7;
          when X => return 2;
          when Y => return 3;
        end case;
    end case;
  end Get_Bit_Number;

------------------------------------------------------------------------------------

  function Get_Position(Joystick : in Joystick_Type;
                        Samples : Integer := 1) return Position_Type is
    Position   : Position_Type := (others => 0);
    Affected   : Boolean := False;
    Value      : Byte;
    SumX, SumY : Integer := 0;
    MinX, MinY : Integer := Integer'Last;
    MaxX, MaxY : Integer := Integer'First;
    Turns      : Integer;
  begin
  -- by WRITING to port, the capacitator starts to charge,
  -- and the x,y port bits are set to 1.
  -- When charged accordingly to resistance given by
  -- joystick pots, the bits are set to 0

    if Samples < 3 then --alway make at least 3 samples
      Turns := 3;
    else
      Turns := Samples;
    end if;

    for i in 1 .. Turns loop -- get some samples for greater accuracy
      Position := (others => 0);
      Set_Gameport(255);     -- start capacitor charge
      loop
        Value := Get_Gameport;
        Affected := False;
        if not Is_Zero(Joystick, X, Value) then
          Position.X := Position.X + 1;
          Affected := True;
        end if;

        if not Is_Zero(Joystick, Y, Value) then
          Position.Y := Position.Y + 1;
          Affected := True;
        end if;
        exit when not Affected; --Loop until both x and y bits are zero
      end loop;

--      put_line("X: " & position.X'img & " Y: " & position.Y'img);

      SumX := SumX + Position.X;
      SumY := SumY + Position.Y;

      if Position.X > MaxX then
        MaxX := Position.X;
      end if;

      if Position.Y > MaxY then
        MaxY := Position.Y;
      end if;

      if Position.X < MinX then
        MinX := Position.X;
      end if;

      if Position.Y < MinY then
        MinY := Position.Y;
      end if;
    end loop;
-- remove extreme values and return average
    Position.X := Integer((SumX - MaxX - MinX) / (Turns - 2));
    Position.Y := Integer((SumY - MaxY - MinY) / (Turns - 2));
--    put_line("Done -> X:  Max: " & MaxX'img & " Min: " & MinX'img & " Sum: " & SumX'img);
--    put_line("Done -> Y:  Max: " & MaxY'img & " Min: " & MinY'img & " Sum: " & SumY'img);
--    put_line("Done -> X: " & position.X'img & " Y: " & position.Y'img);
    return Position;
  end Get_Position;

------------------------------------------------------------------------------------

  function  Get_Direction(Position : Position_Type) return Direction_Type is
    Low_Low     : constant Integer := 0;
    High_Low    : constant Integer := 150;
    Low_Middle  : constant Integer := 151;
    High_Middle : constant Integer := 300;
    Low_High    : constant Integer := 301;
    High_High   : constant Integer := 500;
  begin
  --     (x,y) measured on a PII 350 128Mb ram
  --
  -- (007,007)   (205,007)   (400,007)
  --
  -- (007,205)   (205,205)   (400,205)
  --
  -- (007,400)   (205,400)   (400,400)
--    put_line("X: " & position.X'img & " Y: " & position.Y'img);

--major directions
    if    (Position.X > Low_Low    ) and (Position.X < High_Low    ) and
          (Position.Y > Low_Middle ) and (Position.Y < High_Middle ) then
       return Left;
    elsif (Position.X > Low_Middle ) and (Position.X < High_Middle ) and
          (Position.Y > Low_Low    ) and (Position.Y < High_Low    ) then
       return Up;
    elsif (Position.X > Low_Middle ) and (Position.X < High_Middle ) and
          (Position.Y > Low_High   ) and (Position.Y < High_High   ) then
       return Down;
    elsif (Position.X > Low_High   ) and (Position.X < High_High   ) and
          (Position.Y > Low_Middle ) and (Position.Y < High_Middle ) then
       return Right;
--Corners
    elsif (Position.X > Low_Low    ) and (Position.X < High_Low    ) and
          (Position.Y > Low_Low    ) and (Position.Y < High_Low    ) then
       return Up_left;
    elsif (Position.X > Low_High   ) and (Position.X < High_High   ) and
          (Position.Y > Low_Low    ) and (Position.Y < High_Low    ) then
       return Up_right;
    elsif (Position.X > Low_Low    ) and (Position.X < High_Low    ) and
          (Position.Y > Low_High   ) and (Position.Y < High_High   ) then
       return Down_left;
    elsif (Position.X > Low_High   ) and (Position.X < High_High   ) and
          (Position.Y > Low_High   ) and (Position.Y < High_High   ) then
       return Down_right;
    else
--the middle
       return Straight;
    end if;
  end Get_Direction;

------------------------------------------------------------------------------------

  procedure Get_Gameport_Access is
  begin
--   Get access to the ports
    if not Ioperm(GAMEPORT, 8, True) then
      Perror("Get_Gameport_Access");
      raise Port_Access_Failure;
    end if;
  end Get_Gameport_Access;

------------------------------------------------------------------------------------

  procedure Release_Gameport_Access is
  begin
    if not Ioperm(GAMEPORT, 8, False) then
      Perror("Release_Gameport_Access");
      raise Port_Access_Failure;
    end if;
  end Release_Gameport_Access;

------------------------------------------------------------------------------------

  function Get_Gameport return Byte is
  begin
    return C_Inb(GAMEPORT);
  end Get_Gameport;

------------------------------------------------------------------------------------

  procedure Set_Gameport(Value : in Byte) is
  begin
    C_Outb(Value,GAMEPORT);
  end Set_Gameport;

------------------------------------------------------------------------------------

end Gameport_Io;
