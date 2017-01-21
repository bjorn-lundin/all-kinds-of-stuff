
with Interfaces.C;
with Ada.Exceptions;
with Text_Io; use Text_Io;

with Gpio;
pragma Elaborate_All(Gpio);

--with Ada.Numerics.Generic_Elementary_Functions;


package body Steppers is
  use Interfaces.C;

  type Id_Type is new Integer range 2 .. 3 ;
  type Direction_Type is (Clock_Wise, Counter_Clock_Wise, None);


  --package Float_Math is new Ada.Numerics.Generic_Elementary_Functions (Float);

  -- Epsilon : constant Float := 0.0001;
  subtype Pin_Range_Type      is Integer range 1 .. 4;
  subtype Sequence_Range_Type is Integer range 1 .. 8;

  type Stepper_Pins_Array_Type is array (Pin_Range_Type'Range) of Interfaces.C.Int;

--  Step_Pins : array (Id_Type'Range) of Stepper_Pins_Array_Type := ( 1 => (22,10, 9,11),
--                                                                    2 => (23,18,17,27),
--                                                                    3 => ( 7, 8,25,24));

  Step_Pins : array (Id_Type'Range) of Stepper_Pins_Array_Type := ( 2 => (23,18,17,27),
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

  Delay_Time                 : Duration                  := 2.0/1000.0;
  --Tics_Per_Revolution        : constant Interfaces.C.Int := 4096; -- tics/rev

  protected type Data_Type is
    procedure Set_Direction(Direction : Direction_Type);
    function Get_Direction return Direction_Type;
    procedure Set_Is_Running(Is_Running : Boolean);
    function Get_Is_Running return Boolean;

  private
    Running: Boolean := False;
    D : Direction_Type := None;
  end Data_Type;
  -----------------------------------------------

  task type Motor_Type is
    entry Init(Id : Id_Type);
    entry Run;
    entry Stop;
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
    procedure Set_Is_Running(Is_Running : Boolean) is
    begin
      Running := Is_Running;
    end Set_Is_Running;
    ------------------------------
    function Get_Is_Running return Boolean is
    begin
      return Running;
    end Get_Is_Running;
  end Data_Type;
  -----------------------------------------------

  Data : array(Id_Type'Range) of Data_Type;


  procedure Log(Who,What : in String) is
  begin
    --Put_Line(Calendar2.Clock.To_String & " " & Who & " " & What);
    Put_Line( Who & " " & What);
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
    null;
    --Data(1).Set_Direction(Direction => Clock_Wise);
    --if not Data(1).Get_Is_Running then
    --  Motor(1).Run;
    --end if;
  end Up;
  -----------------------------------------------
  procedure Down is
  begin
    null;
    --Data(1).Set_Direction(Direction => Counter_Clock_Wise);
    --if not Data(1).Get_Is_Running then
    --  Motor(1).Run;
    --end if;
  end Down;
  -----------------------------------------------
  procedure Right is
  begin
    Data(2).Set_Direction(Direction => Clock_Wise);
    if not Data(2).Get_Is_Running then
      Motor(2).Run;
    end if;
  end Right;
  -----------------------------------------------
  procedure Left is
  begin
    Data(2).Set_Direction(Direction => Counter_Clock_Wise);
    if not Data(2).Get_Is_Running then
      Motor(2).Run;
    end if;
  end Left;
  -----------------------------------------------
  procedure Stop is
  begin
   -- Data(1).Set_Direction(Direction => None);
    Data(2).Set_Direction(Direction => None);

    for I in Id_Type'Range loop
      Motor(I).Stop;
    end loop;

  end Stop;

  -----------------------------------------------
  procedure Init is
  begin
    Gpio.Setup;
    for I in Id_Type'Range loop
      Motor(I).Init(I);
    end loop;
  end Init;
  -----------------------------------------------


  task body Motor_Type is
    Local_Pins         : Stepper_Pins_Array_Type;
    Local_Id           : Id_Type;    -- 1 revolution is 4096 tics
    Local_Step_Counter : Sequence_Range_Type := 1;
  begin
    ----------------------------------------------------------
    accept Init(Id : Id_Type) do
      Local_Pins := Step_Pins(Id);
      Local_Id   := Id;
      -- set pins output, and turn them off
      for Pin in Pin_Range_Type'Range loop
        Gpio.Pin_Mode(Local_Pins(Pin), Gpio.Output);
        Gpio.Digital_Write(Local_Pins(Pin), False);
      end loop;
    end Init;
    ----------------------------------------------------------

    Task_Loop : loop
      select
        ----------------------------------------------------------
        accept Stop do
          -- turn the pins off
          for Pin in Pin_Range_Type'Range loop
            Gpio.Digital_Write(Local_Pins(Pin), False);
          end loop;
        end Stop;
      or
          ----------------------------------------------------------
        accept Run do
           Data(Local_Id).Set_Is_Running(True);
        end Run;

        Motor_Loop_Start : loop
          Log("Steppers.Run" & Local_Id'Img,"start loop" );

          case Data(Local_Id).Get_Direction is
            when None =>
              exit Motor_Loop_Start;

            when Clock_Wise =>
              for Pin in Pin_Range_Type'Range loop
                if Sequence(Local_Step_Counter, Pin) /= 0 then
                  Gpio.Digital_Write(Local_Pins(Pin), True);
                else
                  Gpio.Digital_Write(Local_Pins(Pin), False);
                end if;
              end loop;
              if Local_Step_Counter = Sequence_Range_Type'Last then
                Local_Step_Counter := Sequence_Range_Type'First;
              else
                Local_Step_Counter := Local_Step_Counter + 1;
              end if;
              delay Delay_Time;

           when Counter_Clock_Wise =>
              for Pin in Pin_Range_Type'Range loop
                if Sequence(Local_Step_Counter, Pin) /= 0 then
                  Gpio.Digital_Write(Local_Pins(Pin), True);
                else
                  Gpio.Digital_Write(Local_Pins(Pin), False);
                end if;
              end loop;
              if Local_Step_Counter = Sequence_Range_Type'First then
                Local_Step_Counter := Sequence_Range_Type'Last;
              else
                Local_Step_Counter := Local_Step_Counter - 1;
              end if;
              delay Delay_Time;

          end case;
        end loop Motor_Loop_Start;
        Data(Local_Id).Set_Is_Running(False);
        Log("Steppers.Run" & Local_Id'Img,"stop  loop" );

        ----------------------------------------------------------
      or
        terminate;

        ----------------------------------------------------------
      end select;

    end loop Task_Loop;
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
      Log("Steppers.Test", "start Init Motor" & i'img);
      delay 2.0;
      Motor(I).Init(I);
      Log("Steppers.Test", "start Running Motor" & i'img);
      Data(I).Set_Direction(Clock_Wise);
      delay 2.0;
      Motor(I).Run;
      delay 10.0;
      Data(I).Set_Direction(None);
      Log("Steppers.Test", "stop Running Motor" & i'img);
      delay 2.0;
    end loop;
    Log("Steppers.Test", "Test done");

  end Test;

end Steppers;
