
with Interfaces.C;
with Ada.Exceptions;
with Text_Io; use Text_Io;
with Calendar2;
--with Types; use Types;
with Gpio;
pragma Elaborate_All(Gpio);

--with Ada.Numerics.Generic_Elementary_Functions;


package body Steppers is
  use Interfaces.C;

  type Id_Type is new Integer range 1 .. 3 ;
  type Direction_Type is (Clock_Wise, Counter_Clock_Wise, None, Stop);


  --package Float_Math is new Ada.Numerics.Generic_Elementary_Functions (Float);

  -- Epsilon : constant Float := 0.0001;
  subtype Pin_Range_Type      is Integer range 1 .. 4;
  subtype Sequence_Range_Type is Integer range 1 .. 8;

  type Stepper_Pins_Array_Type is array (Pin_Range_Type'Range) of Interfaces.C.Int;

  -- https://projects.drogon.net/raspberry-pi/wiringpi/special-pin-functions/
  Step_Pins : array (Id_Type'Range) of Stepper_Pins_Array_Type := ( 1 => (22,10, 9,11),
                                                                  --  2 => (23,18,17,27), Rev 2?
                                                                    2 => (23,18,17,21),  --Rev 1?
                                                                    3 => ( 7, 8,25,24));
  -- stepper sequence
  type Stepper_Sequence_Type is array (Sequence_Range_Type'Range, Pin_Range_Type'Range) of Interfaces.C.Int;

  Sequence : constant Stepper_Sequence_Type := ((1,0,0,1),
                                                (1,0,0,0),
                                                (1,1,0,0),
                                                (0,1,0,0),
                                                (0,1,1,0),
                                                (0,0,1,0),
                                                (0,0,1,1),
                                                (0,0,0,1));

  Tics_Per_Revolution        : constant Positive :=    64; -- 4_096;
  Koggs_Per_Revolution       : constant Positive :=   140;

  Delay_Time : array(Id_Type'range) of Duration  := (1 =>      1.0, -- not Used
                                                     2 => 43_200.0/(Duration(Koggs_Per_Revolution * Tics_Per_Revolution)),  -- 140 koggs 12 hrs (86400/2 s) --2.0/1000.0;
                                                     3 =>  3_600.0/(Duration(Koggs_Per_Revolution * Tics_Per_Revolution))); -- 140 koggs 60 min (60*60 s)
  --Tics_Per_Revolution        : constant Interfaces.C.Int := 4096; -- tics/rev

  Global_Is_Initiated        : Boolean := False;

  protected type Data_Type is
    procedure Set_Direction(Direction : Direction_Type);
    function Get_Direction return Direction_Type;
  private
    D : Direction_Type := None;
  end Data_Type;
  -----------------------------------------------

  task type Motor_Type is
    entry Init(Identity : Id_Type);
  end Motor_Type;

  -----------------------------------------------
  Motor : array(Id_Type'Range) of Motor_Type;
  -----------------------------------------------
  protected body Data_Type is
    ------------------------------
    procedure Set_Direction(Direction : Direction_Type) is
    begin
      D := Direction;
    end Set_Direction;
    ------------------------------
    function Get_Direction return Direction_Type is
    begin
      return D;
    end Get_Direction;
  end Data_Type;
  -----------------------------------------------

  Data  : array(Id_Type'Range) of Data_Type;


  procedure Log(Who,What : in String) is
  begin
    Put_Line(Calendar2.Clock.To_String & " " & Who & " " & What);
    --Put_Line( Who & " " & What);
  end Log;


  procedure Set_Speed(Speed: in Speed_Type) is
  begin
    Log("Handle_Events","Set_Speed " & Speed'Img );
  end Set_Speed;
  -----------------------------------------------

  --M1 Handles Up/down
  --M2 Handles Left/Right

  procedure Up is
  begin
    Data(1).Set_Direction(Direction => Clock_Wise);
  end Up;
  -----------------------------------------------
  procedure Down is
  begin
    null;
    Data(1).Set_Direction(Direction => Counter_Clock_Wise);
  end Down;
  -----------------------------------------------
  procedure Right is
  begin
    Data(2).Set_Direction(Direction => Clock_Wise);
  end Right;
  -----------------------------------------------
  procedure Left is
  begin
    Data(2).Set_Direction(Direction => Counter_Clock_Wise);
  end Left;
  -----------------------------------------------
  procedure No_Direction is
  begin
    Data(1).Set_Direction(Direction => None);
    Data(2).Set_Direction(Direction => None);
  end No_Direction;
  -----------------------------------------------
  procedure Stop is
  begin
    for I in Id_Type'Range loop
      Data(I).Set_Direction(Direction => Stop);
    end loop;
  end Stop;

  -----------------------------------------------
  procedure Init is
  begin
    if not Global_Is_Initiated then
      Gpio.Setup;
      for I in Id_Type'Range loop
        Motor(I).Init(I);
      end loop;
      Global_Is_Initiated := True;
    end if;
  end Init;
  -----------------------------------------------


  task body Motor_Type is
    Pins           : Stepper_Pins_Array_Type;
    Id             : Id_Type;
    Sequence_Index : Sequence_Range_Type := 1;
  begin
    ----------------------------------------------------------
    accept Init(Identity : Id_Type) do
      Pins := Step_Pins(Identity);
      Id   := Identity;
      -- set pins output, and turn them off
      for Pin in Pin_Range_Type'Range loop
        Gpio.Pin_Mode(Pins(Pin), Gpio.Output);
        Gpio.Digital_Write(Pins(Pin), False);
      end loop;
    end Init;
    ----------------------------------------------------------

    Motor_Loop : loop
      case Data(Id).Get_Direction is
        when Stop => exit Motor_Loop;
        when None => null;
        when Clock_Wise =>
          for Pin in Pin_Range_Type'Range loop
            if Sequence(Sequence_Index, Pin) /= 0 then
              Gpio.Digital_Write(Pins(Pin), True);
            else
              Gpio.Digital_Write(Pins(Pin), False);
            end if;
          end loop;
          if Sequence_Index = Sequence_Range_Type'Last then
            Sequence_Index := Sequence_Range_Type'First;
          else
            Sequence_Index := Sequence_Index + 1;
          end if;

        when Counter_Clock_Wise =>
          for Pin in Pin_Range_Type'Range loop
            if Sequence(Sequence_Index, Pin) /= 0 then
              Gpio.Digital_Write(Pins(Pin), True);
            else
              Gpio.Digital_Write(Pins(Pin), False);
            end if;
          end loop;
          if Sequence_Index = Sequence_Range_Type'First then
            Sequence_Index := Sequence_Range_Type'Last;
          else
            Sequence_Index := Sequence_Index - 1;
          end if;
      end case;

      delay Delay_Time(Id);
    end loop Motor_Loop;

    -- turn the pins off at exit
    for Pin in Pin_Range_Type'Range loop
      Gpio.Digital_Write(Pins(Pin), False);
    end loop;

  exception
    when E: others =>
      declare
        Last_Exception_Name     : constant String := Ada.Exceptions.Exception_Name(E);
        Last_Exception_Messsage : constant String := Ada.Exceptions.Exception_Message(E);
        Last_Exception_Info     : constant String := Ada.Exceptions.Exception_Information(E);
      begin
        Put_Line(Last_Exception_Name);
        Put_Line("Message : " & Last_Exception_Messsage);
        Put_Line(Last_Exception_Info);
        -- Put_Line("addr2line" & " --functions --basenames --exe=" &
        --      Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
      end ;
  end Motor_Type;
  --------------------------------------------------------

  procedure Test is
  begin
    Log("Steppers.Test", "Running Test");
    delay 2.0;
    Gpio.Setup;
    delay 2.0;
    for I in Id_Type'Range loop
     --  Log("Steppers.Test", "start Init Motor" & i'img);
     --  Motor(I).Init(I);

      Log("Steppers.Test", "direction Running Motor" & I'Img & " Clock_Wise");
      Data(I).Set_Direction(Clock_Wise);
      delay 2.0;
      Data(I).Set_Direction(None);
      Log("Steppers.Test", "stop Running Motor" & I'Img);
      delay 2.0;

      Log("Steppers.Test", "direction Running Motor" & I'Img & " Counter_Clock_Wise");
      Data(I).Set_Direction(Counter_Clock_Wise);
      delay 2.0;
      Data(I).Set_Direction(None);
      Log("Steppers.Test", "stop Running Motor" & I'Img);
      delay 2.0;

    end loop;
    Log("Steppers.Test", "Test done");

  exception
    when E: others =>
      declare
        Last_Exception_Name     : constant String := Ada.Exceptions.Exception_Name(E);
        Last_Exception_Messsage : constant String := Ada.Exceptions.Exception_Message(E);
        Last_Exception_Info     : constant String := Ada.Exceptions.Exception_Information(E);
      begin
        Put_Line(Last_Exception_Name);
        Put_Line("Message : " & Last_Exception_Messsage);
        Put_Line(Last_Exception_Info);
        -- Put_Line("addr2line" & " --functions --basenames --exe=" &
        --      Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
      end ;
  end Test;
  ----------------------------
  procedure Do_Clock is
    Service  : constant String := "Steppers.Do_Clock";
    Not_Used : Id_Type := 1;
    Hour     : Id_Type := 2;
    Minute   : Id_Type := 3;
  begin
    Log(Service, "Running Test");
    Gpio.Setup;

    Data(Not_Used).Set_Direction(Stop);
    Data(Hour).Set_Direction(Clock_Wise);
    Data(Minute).Set_Direction(Clock_Wise);
    -- the rest is in the tasks

    Log(Service, "delay_time(hour): " & delay_time(hour)'img);
    Log(Service, "delay_time(minute): " & delay_time(minute)'img);

    loop
      Log(Service, "is running");
      delay 30.0;
    end loop;

  exception
    when E: others =>
      declare
        Last_Exception_Name     : constant String := Ada.Exceptions.Exception_Name(E);
        Last_Exception_Messsage : constant String := Ada.Exceptions.Exception_Message(E);
        Last_Exception_Info     : constant String := Ada.Exceptions.Exception_Information(E);
      begin
        Put_Line(Last_Exception_Name);
        Put_Line("Message : " & Last_Exception_Messsage);
        Put_Line(Last_Exception_Info);
        -- Put_Line("addr2line" & " --functions --basenames --exe=" &
        --      Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
      end ;
  end Do_Clock;

end Steppers;
