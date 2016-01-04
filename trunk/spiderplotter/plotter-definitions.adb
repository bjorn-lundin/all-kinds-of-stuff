
with Interfaces.C;
with Ada.Exceptions;
with Text_IO; use Text_IO;

with Gpio;
pragma Elaborate_All(Gpio);

with Ada.Numerics.Generic_Elementary_Functions;


package body Plotter.Definitions is
  use Interfaces.C;

  package Float_Math is new Ada.Numerics.Generic_Elementary_Functions (Float);

 -- Epsilon : constant Float := 0.0001;
  subtype Pin_Range_Type      is Integer range 1 .. 4;
  subtype Sequence_Range_Type is Integer range 1 .. 8;

  type Stepper_Pins_Array_Type is array (Pin_Range_Type'range) of Interfaces.C.Int;

  Step_Pins : array (Id_Type'range) of Stepper_Pins_Array_Type := ( 1 => (22,10, 9,11),
                                                                    2 => (23,18,17,27),
                                                                    3 => ( 7, 8,25,24));

  -- stepper sequence
  type Stepper_Sequence_Type is array (Sequence_Range_Type'range, Pin_Range_Type'range) of Interfaces.C.Int;

  Sequence : constant Stepper_Sequence_Type := ((1,0,0,1),
                                                (1,0,0,0),
                                                (1,1,0,0),
                                                (0,1,0,0),
                                                (0,1,1,0),
                                                (0,0,1,0),
                                                (0,0,1,1),
                                                (0,0,0,1));

  Delay_Time                 : Duration                  := 10.0/1000.0;
  Tics_Per_Revolution        : constant Interfaces.C.Int := 4096; -- tics/rev
  Millimeters_Per_Revolution : constant Float            := 85.666667; -- mm/rev
  Tics_Per_Millimeter        : constant Float := Float(Tics_Per_Revolution)/Millimeters_Per_Revolution; -- ~47,8 tic/mm
  --Millimeters_Per_Tic        : constant Float := Millimeters_Per_Revolution/Float(Tics_Per_Revolution); -- ~0,020915 mm/tic
  Global_Distance            : Float := 500.0; --mm
  Global_L1_Start            : Float := 200.0; --mm,
  Global_L2_Start            : Float := 200.0; --mm,

    -- 1 revolution is 4096 tics
    -- 1 revolution is 85,666667 mm
    -- 1 tic => 85,666667/4096 mm

  type Direction_Type is (Clock_Wise, Counter_Clock_Wise, None);

  protected type Data_Type is
    procedure Increase_Tics;
    procedure Decrease_Tics;
    procedure Set_Tics(Value : Interfaces.C.Int);
    function Get_Tics return Interfaces.C.Int;
    procedure Set_Stop(Value : Boolean);
    function Get_Stop return Boolean;
  private
    Tics : Interfaces.C.Int := 0;
    Stop : Boolean := False;
  end Data_Type;
  -----------------------------------------------

  task type Motor_Type is
    entry Init(Id : Id_Type);
    entry Turn_To(Tics : Interfaces.C.Int) ;
    entry Stop;
  end Motor_Type;

  -----------------------------------------------

  M1,M2,M3 : Motor_Type;
  -----------------------------------------------
  protected body Data_Type is
    ------------------------------
    procedure Increase_Tics is
    begin
      Tics := Tics + 1;
    end Increase_Tics;
    ------------------------------
    procedure Decrease_Tics is
    begin
      Tics := Tics - 1;
    end Decrease_Tics;
    ------------------------------
    procedure Set_Tics(Value : Interfaces.C.Int) is
    begin
      Tics := Value;
    end Set_Tics;
    ------------------------------
    function Get_Tics return Interfaces.C.Int is
    begin
      return Tics;
    end Get_Tics;
    ------------------------------
    procedure Set_Stop(Value : Boolean) is
    begin
      Stop := Value;
    end Set_Stop;
    ------------------------------
    function Get_Stop return Boolean is
    begin
      return Stop;
    end Get_Stop;
  end Data_Type;
  -----------------------------------------------

  Data : array(Id_Type'range) of Data_Type;

  task body Motor_Type is
    Local_Pins : Stepper_Pins_Array_Type;
    Local_Id   : Id_Type;
    Wanted_Tics,
    Local_Tics : Interfaces.C.Int := 0;  -- start in Origo
    -- 1 revolution is 4096 tics
    -- 1 revolution is 85,666667 mm
    -- 1 tic => 85,666667/4096 mm
    Local_Step_Counter : Sequence_Range_Type := 1;
    Local_Direction    : Direction_Type      := None;
  begin
    ----------------------------------------------------------
    accept Init(Id : Id_Type) do
      Local_Pins := Step_Pins(Id);
      Local_Id   := Id;

      case Id is
        when 1 => Local_Tics := Interfaces.C.Int(Global_L1_Start * Tics_Per_Millimeter);
        when 2 => Local_Tics := Interfaces.C.Int(Global_L2_Start * Tics_Per_Millimeter);
        when 3 => Local_Tics := 0;
      end case;
      Data(Local_Id).Set_Tics(Local_Tics);
      -- set pins output
      for Pin in Pin_Range_Type'range loop
        Gpio.Pin_Mode(Local_Pins(Pin), Gpio.OUTPUT);
      end loop;
    end Init;
    ----------------------------------------------------------

    Task_Loop : loop
      select
        ----------------------------------------------------------
        accept Stop do
          -- turn the pins off
          for Pin in Pin_Range_Type'range loop
            Gpio.Digital_Write(Local_Pins(Pin), False);
          end loop;
        end Stop;
      or
        ----------------------------------------------------------
        accept Turn_To(Tics : Interfaces.C.Int) do
          Wanted_Tics := Tics;
          if Tics = Local_Tics then
            Local_Direction := None;
          elsif Tics > Local_Tics then
            Local_Direction := Clock_Wise;
          elsif Tics < Local_Tics then
            Local_Direction := Counter_Clock_Wise;
          else
            Put_Line(Local_Id'Img & "tics=" & tics'Img & " Local_Tics" & Local_Tics'Img);
          end if;
          Put_Line(Local_Id'Img &
                   " Local_Tics=" & Local_Tics'Img &
                   " Wanted_Tics=" & Wanted_Tics'Img &
                   " " & Local_Direction'Img);
        end Turn_To;

        Put_Line("start " & Local_Id'Img &
                 " Local_Tics=" & Local_Tics'Img &
                 " Wanted_Tics=" & Wanted_Tics'Img &
                 " " & Local_Direction'Img & " in loop");
        Motor_Loop_Start : loop
          case Local_Direction is
            when None =>
              exit Motor_Loop_Start;

            when Clock_Wise | Counter_Clock_Wise =>
              for Pin in Pin_Range_Type'range loop
                if Sequence(Local_Step_Counter, Pin) /= 0 then
                  Gpio.Digital_Write(Local_Pins(Pin), True);
                else
                  Gpio.Digital_Write(Local_Pins(Pin), False);
                end if;
              end loop;

              if Local_Direction = Clock_Wise then
                if Local_Step_Counter = Sequence_Range_Type'Last then
                  Local_Step_Counter := Sequence_Range_Type'First;
                else
                  Local_Step_Counter := Local_Step_Counter + 1;
                end if;
                Local_Tics := Local_Tics +1;
                Data(Local_Id).Increase_Tics;
                exit Motor_Loop_Start when Local_Tics >= Wanted_Tics;
              else
                if Local_Step_Counter = Sequence_Range_Type'First then
                  Local_Step_Counter := Sequence_Range_Type'Last;
                else
                  Local_Step_Counter := Local_Step_Counter - 1;
                end if;
                Local_Tics := Local_Tics -1;
                Data(Local_Id).Decrease_Tics;
                exit Motor_Loop_Start when Local_Tics <= Wanted_Tics;
              end if;

              exit Motor_Loop_Start when Data(Local_Id).Get_Stop;

          end case;
          delay Delay_Time;
        end loop Motor_Loop_Start;
        Put_Line("stop  " & Local_Id'Img &
                 " Local_Tics=" & Local_Tics'Img &
                 " Wanted_Tics=" & Wanted_Tics'Img &
                 " " & Local_Direction'Img & " in loop");

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
  procedure Delta_L(X  : in     X_Coordinate_Type;
                    Y  : in     Y_Coordinate_Type;
                    d  : in     Float;
                    L1 : in out Float;
                    L2 : in out Float) is
  begin
    L2 := Float_Math.Sqrt(d*d -2.0*d*Float(x) + Float(x*x) + Float(y*y));
    L1 := Float_Math.Sqrt(Float(x*x) + Float(y*y));
  end Delta_L;
  --------------------------------------------------------


  task body Motor_Controller is
    L12,L22 : Float := 0.0;
    T1,T2   : Interfaces.C.Int := 0;
  begin
    accept Init do
      M1.Init(1);
      M2.Init(2);
    end Init;

    Motor_Controller_Task_Loop : loop
      select
        -----------------------------
        accept Goto_XY(X : X_Coordinate_Type; Y : Y_Coordinate_Type ) do
          Delta_L(X,Y,Global_Distance,L12,L22);
          T1 := Interfaces.C.Int(L12 * Tics_Per_Millimeter);
          T2 := Interfaces.C.Int(L22 * Tics_Per_Millimeter);

          Put_Line("Goto_XY Data(1).Get_Tics=" & Data(1).Get_Tics'Img & " T1=" & T1'Img );
          Put_Line("Goto_XY Data(2).Get_Tics=" & Data(2).Get_Tics'Img & " T2=" & T2'Img );

          M1.Turn_To(T1);
          M2.Turn_To(T2);

          --busy loop until done both
          if Data(1).Get_Tics > T1 then
            loop
              exit when Data(1).Get_Tics <= T1;
              delay Delay_Time/2.0;
            end loop;
          elsif Data(1).Get_Tics < T1 then
            loop
              exit when Data(1).Get_Tics >= T1;
              delay Delay_Time/2.0;
            end loop;
          end if;
          -- and now other engine, if not already done
          if Data(2).Get_Tics > T2 then
            loop
              exit when Data(2).Get_Tics <= T2;
              delay Delay_Time/2.0;
            end loop;
          elsif Data(2).Get_Tics < T2 then
            loop
              exit when Data(2).Get_Tics >= T2;
              delay Delay_Time/2.0;
            end loop;
          end if;
        end Goto_XY;

      or -----------------------------
        accept Stop do
          M1.Stop;
          M2.Stop;
        end Stop;
      or -----------------------------
        terminate;
      end select;
    end loop Motor_Controller_Task_Loop;

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
  end Motor_Controller;

  -----------------------------------------------------------

  task body Pen_Controller is
    Tics_Pen_Up   : Interfaces.C.Int := 500;
    Tics_Pen_Down : Interfaces.C.Int :=   0;
  begin
    ----------------------------------------------------------
    accept Init do
      M3.Init(3);
    end Init;
    ----------------------------------------------------------

    Pen_Controller_Task_Loop : loop
      select
        -----------------------------
        accept Pen_Up do
          if Data(3).Get_Tics <= Tics_Pen_Down then
            M3.Turn_To(Tics_Pen_Up);
            -- loop to wait for pen
            loop
              exit when Data(3).Get_Tics >= Tics_Pen_Up;
              delay Delay_Time;
            end loop;
          else
            Put_Line("Pen_Controller.Pen_Up bad tics" & Data(3).Get_Tics'Img);
          end if;
        end Pen_Up;
      or -----------------------------
        accept Pen_Down do
          if Data(3).Get_Tics >= Tics_Pen_Up then
            M3.Turn_To(Tics_Pen_Down);
            -- loop to wait for pen
            loop
              exit when Data(3).Get_Tics <= Tics_Pen_Down;
              delay Delay_Time;
            end loop;
          else
            Put_Line("Pen_Controller.Pen_Down bad tics" & Data(3).Get_Tics'Img);
          end if;
        end Pen_Down;
      or -----------------------------
        accept Stop do
          M3.Stop;
        end Stop;
      or -----------------------------
        terminate;
      end select;
    end loop Pen_Controller_Task_Loop;
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
  end Pen_Controller;
  --------------------------------------------------------


end Plotter.Definitions;

