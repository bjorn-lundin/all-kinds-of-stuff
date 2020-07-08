with Text_Io; use Text_Io;
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

  protected Direction_Keeper is
    procedure Set(Val : Direction_Type);
    function Get return Direction_Type ;
  private
    Db : Direction_Type := None;
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

  private
    Db : Direction_Type := None;
  end Direction_Keeper;
  ----------------------------------

  procedure Set_Direction(Direction : Direction_Type) is
  begin
    Direction_Keeper.Set(Direction);
  end Set_Direction;
  ----------------------------------


  task body Motor_Task is
    Wanted_Step                               : Step_Type;
    State                                     : Motor_State_Type := Starting;
    Direction                                 : Direction_Type;
    Pin                                       : Pin_Array_Type;
    Local_Name                                : Motor_Index_Type ;
  begin
    accept Config(Configuration_Pin : Pin_Array_Type; Name : Motor_Index_Type) do
      Pin := Configuration_Pin;
      Local_Name := Name;
      Gpio.Pin_Mode( Interfaces.C.Int(Pin(Emergency_Stop)) , Gpio.Input);
      Gpio.Pull_Up_Dn_Control(Interfaces.C.Int(Pin(Emergency_Stop)), Gpio. Pud_Down);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Direction)), Gpio.Output);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Step)), Gpio.Output);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Enable)), Gpio.Output);
      Write(Pin(Step), Gpio.Low);
      Write(Pin(Enable), Gpio.Low); -- Low is to enable
      Text_Io.Put_Line("Config done" & Local_Name'Img);
    end Config;

    loop
      select

        accept Go do
          Write(Pin(Enable), Gpio.Low); --Turn on stepper
          -- delay 1.0;
        end Gop;

        Direction := Direction_Keeper.Get;

        case Direction is
          when Ccw  => null;
          when Cw   => null;
          when Up   => Write(Pin(Direction), 1);
          when Down => Write(Pin(Direction), 0);
        end case;

        Move_Loop : loop
          exit Move_Loop when Direction_Keeper.Get = None;
          Write(Pin(Step), Gpio.High);
          delay Delay_Time(Normal);
          Write(Pin(Step), Gpio.Low);
          delay Delay_Time(Normal);
        end loop Move_Loop;
        Write(Pin(Enable), Gpio.HIGH); -- Low is to enable - turn off
      or
        terminate;
      end select;
    end loop;
    --  exception
    --   when E: others =>
    --    Stacktrace.Tracebackinfo(E);
  end Motor_Task;


end Motors;
