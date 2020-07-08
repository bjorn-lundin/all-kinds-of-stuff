with Text_IO; use Text_IO;
with Interfaces.C;

--with Stacktrace;
with Gpio;
--with Binary_Semaphores;



package body Motors is

 -- Sem : Binary_Semaphores.Semaphore_Type;

  procedure Write(Pin: Pin_Type ; Value : Boolean) is
  begin
--    Sem.Seize;
    Gpio.Digital_Write(Interfaces.C.Int(Pin), Value);
--    Sem.Release;
  end Write;

  function Read(Pin: Pin_Type) return Boolean is
    R : Boolean := False;
  begin
 --   Sem.Seize;
    R := Gpio.Digital_Read(Interfaces.C.Int(Pin));
 --   Sem.Release;
    return R;
  end Read;




  protected type Step_Holder_Type is
    procedure Reset;
    function Get return Step_Type;
    procedure Increase;
    procedure Decrease;
  private
    Step : Step_Type := 0.0;
  end Step_Holder_Type;

  protected body Step_Holder_Type is
    procedure Reset is
    begin
      Step := 0.0;
    end Reset;
    -----------------------------
    function Get return Step_Type is
    begin
      return Step;
    end Get;
    -----------------------------
    procedure Increase is
    begin
      Step := Step + 1.0;
    end Increase;
    -----------------------------
    procedure Decrease is
    begin
      Step := Step - 1.0;
    end Decrease;



  end Step_Holder_Type;

  Steps : array (Motor_Index_Type'range) of Step_Holder_Type;


  protected type Busy_Type is
    function Is_Busy return Boolean;
    procedure Set(Val : Boolean);
  private
    B : Boolean := False;
  end Busy_Type;

  protected body Busy_Type is
    function Is_Busy return Boolean is
    begin
      return B;
    end Is_Busy;

    procedure Set(Val : Boolean) is
    begin
      B := Val;
      end Set;
  end Busy_Type;

  Busy : array (Motor_Index_Type'range) of Busy_Type;


  task body Motor_Task is
    Wanted_Step                               : Step_Type;
    State                                     : Motor_State_Type := Starting;
    Emg_Stop                                  : Boolean := False;
    Old_Stop                                  : Boolean := True;
    Direction                                 : Direction_Type;
    --  Current_Direction                         : Direction_Type;
    Pin                                       : Pin_Array_Type;
    Local_Name                                : Motor_Index_Type ;
    -- Busy                                      : Boolean := False;
    Diff                                      : Step_Type := 0.0;
  begin
    accept Config(Configuration_Pin : Pin_Array_Type; Direction_Towards_Emergency_Stop : Direction_Type; Name : Motor_Index_Type) do
      Pin := Configuration_Pin;
      Local_Direction_Towards_Emergency_Stop := Direction_Towards_Emergency_Stop;
      Local_Name := Name;
      Gpio.Pin_Mode( Interfaces.C.Int(Pin(Emergency_Stop)) , Gpio.INPUT);
      Gpio.Pull_Up_Dn_Control(Interfaces.C.Int(Pin(Emergency_Stop)), Gpio. PUD_DOWN);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Direction)), Gpio.OUTPUT);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Step)), Gpio.OUTPUT);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Enable)), Gpio.OUTPUT);
      Write(Pin(Step), Gpio.LOW);
      Write(Pin(Enable), Gpio.LOW); -- Low is to enable
      --Handler.Set(P => Pin(Enable)); -- so we can shut it down on ctrl^C
      Text_Io.Put_Line("Config done" & Local_Name'Img);
    end Config;

    loop
      Text_Io.Put_Line("select" & Steps(Local_Name).Get'Img & " " & Current_Direction'Img & Local_Name'Img);
      select

          accept Goto_Step(S : Step_Type) do
            Busy(Local_Name).Set(True);
            Wanted_Step := S;
            Text_Io.Put_Line(Local_Name'Img & " -> Goto_step Accepted" & " w/C" & S'Img & "/" & Steps(Local_Name).Get'Img );
            Write(Pin(Enable), Gpio.LOW); --Turn on stepper
           -- delay 1.0;
          end Goto_Step;



        case Direction is
          when Ccw => null;
--              if Current_Direction = Ccw then
--                -- change direction
--                Current_Direction := Cw;
--                Write(Pin(Direction), Current_Direction);
--              end if;

          when Cw  => null;
--              if Current_Direction = Cw then
--                -- change direction
--                Current_Direction := Ccw;
--                Write(Pin(Direction), Current_Direction);
--              end if;

          when Up =>
            if Current_Direction /= Up then
              -- change direction
              Current_Direction := Up;
              Write(Pin(Direction), 1);
            end if;

          when Down  =>
            if Current_Direction /= Down then
              -- change direction
              Current_Direction := Down;
              Write(Pin(Direction), 0);
            end if;
        end case;




          Move_Loop : loop
           -- Emg_Stop := Read(Pin(Emergency_Stop));
           -- if Emg_Stop then
           --   Text_Io.Put_Line(Local_Name'Img & " -> EMGSTOP" & Local_Name'Img & " kill motor and Exit task");
           --   Write(Pin(Enable), Gpio.Low); -- shut down power to stepper
           --   exit; -- error msg?
           -- end if;

          --  Text_Io.Put_Line(Local_Name'Img & " -> Goto_step " & " W/C" & Wanted_Step'Img & "/" & Steps(Local_Name).Get'Img );


              Write(Pin(Step), Gpio.HIGH);
              delay Delay_Time(Normal);
              Write(Pin(Step), Gpio.LOW);
              delay Delay_Time(Normal);

          end loop Move_Loop;
          Text_Io.Put_Line(Local_Name'Img & " -> exit move_Step " & " w/C" & Wanted_Step'Img & "/" & Steps(Local_Name).Get'Img & " dir=" & Current_Direction'Img);
          --Write(Pin(Enable), Gpio.HIGH); -- Low is to enable
      or
        terminate;
      end select;
    end loop;
--  exception
 --   when E: others =>
  --    Stacktrace.Tracebackinfo(E);
  end Motor_Task;


  procedure Safe_Home is
    --  Motor_Z   : Motors.Motor_Task renames Motors.M(1);
    --  Motor_Fi1 : Motors.Motor_Task renames Motors.M(2);
    --  Motor_Fi2 : Motors.Motor_Task renames Motors.M(3);
  begin
    Put_Line("Safe_Home Motorfi1.Home start");
    M(2).Home;
    M(2).Goto_Step(400.0);

    Put_Line("Safe_Home Motorfi2.Home start");
    M(3).Home;

    Put_Line("Safe_Home MotorZ.Home start");
    M(1).Home;

  end Safe_Home;


end Motors;
