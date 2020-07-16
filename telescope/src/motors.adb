with Text_Io; use Text_Io;
with Interfaces.C;

--with Stacktrace;
with Gpio;
--with Binary_Semaphores;



package body Motors is

-- Sem : Binary_Semaphores.Semaphore_Type;


  Delay_Time : array (Speed_Type'Range) of Duration := (Slow => 0.005, Normal => 0.001);
  -- 1 us is absolutly min


  procedure Write(Pin: Pin_Number_Type ; Value : Boolean) is
  begin
    --    Sem.Seize;
    Gpio.Digital_Write(Interfaces.C.Int(Pin), Value);
    --    Sem.Release;
  end Write;

  function Read(Pin: Pin_Number_Type) return Boolean is
    R : Boolean := False;
  begin
    --   Sem.Seize;
    R := Gpio.Digital_Read(Interfaces.C.Int(Pin));
    --   Sem.Release;
    return R;
  end Read;
  pragma Unreferenced(Read);

  protected Direction_Keeper is
    procedure Set(Val : Direction_Type);
    function Get return Direction_Type ;
  private
    D : Direction_Type := None;
  end Direction_Keeper;

  protected body Direction_Keeper is
    procedure Set(Val : Direction_Type) is
    begin
      D := Val;
    end Set;

    function Get return Direction_Type is
    begin
      return D;
    end Get;
  end Direction_Keeper;
  ----------------------------------

  procedure Set_Direction(Direction : Direction_Type) is
  begin
    Direction_Keeper.Set(Direction);
  end Set_Direction;
  ----------------------------------


  protected Speed_Keeper is
    procedure Set(Val : Speed_Type);
    function Get return Speed_Type ;
  private
    S : Speed_Type := Normal;
  end Speed_Keeper;

  protected body Speed_Keeper is
    procedure Set(Val : Speed_Type) is
    begin
      S := Val;
    end Set;

    function Get return Speed_Type is
    begin
      return S;
    end Get;
  end Speed_Keeper;
  ----------------------------------

  procedure Set_Speed(Speed: Speed_Type) is
  begin
    Speed_Keeper.Set(Speed);
  end Set_Speed;
  ----------------------------------



  task body Motor_Task is
    Dir        : Direction_Type;
    Pin        : Pin_Array_Type;
    Local_Name : Motor_Index_Type;
    Speed      : Speed_Type := Normal;
  begin
    accept Config(Configuration_Pin : Pin_Array_Type; Name : Motor_Index_Type) do
      Pin := Configuration_Pin;
      Local_Name := Name;
    --  Gpio.Pin_Mode( Interfaces.C.Int(Pin(Emergency_Stop)) , Gpio.Input);
    --  Gpio.Pull_Up_Dn_Control(Interfaces.C.Int(Pin(Emergency_Stop)), Gpio. Pud_Down);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Direction)), Gpio.Output);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Step)), Gpio.Output);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Enable)), Gpio.Output);
      Write(Pin(Step), False);
      Put_Line("Config done" & Local_Name'Img);
    end Config;

    Select_Loop : loop
      select
        accept Go do
          Write(Pin(Enable), False); --Turn on stepper
          -- delay 1.0;
        end Go;

        Dir := Direction_Keeper.Get;

        case Dir is
          when Ccw  => null;
          when Cw   => null;
          when Up   => Write(Pin(Direction), True);
          when Down => Write(Pin(Direction), False);
          when None => null;
        end case;

        Move_Loop : loop
          exit Move_Loop when Direction_Keeper.Get = None;
          Speed := Speed_Keeper.Get;
          Write(Pin(Step), True);
          delay Delay_Time(Speed);
          Write(Pin(Step), False);
          delay Delay_Time(Speed);
          Put_Line("in loop " & Local_Name'Img);
        end loop Move_Loop;
        Write(Pin(Enable), True); -- Low is to enable - turn off
        Put_Line("out of loop " & Local_Name'Img);
      or
        accept Stop do
          null;
        end Stop;
        exit Select_Loop;
      or
        terminate;
      end select;
      Put_Line("task exited MOVE loop ok " & Local_Name'Img);
    end loop Select_Loop;
    Put_Line("task died ok " & Local_Name'Img);
  exception
    when others =>
      Put_Line("task died bad " & Local_Name'Img);

    --    Stacktrace.Tracebackinfo(E);
  end Motor_Task;

begin
  Gpio.Setup;
end Motors;
