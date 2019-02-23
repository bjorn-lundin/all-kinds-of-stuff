with Stacktrace;
with Gpio;
with Interfaces.C;

package body Motors is


  procedure Write(Pin: Pin_Type ; Value : Boolean) is
  begin
    Gpio.Digital_Write(Interfaces.C.Int(Pin), Value);
  end Write;


  function Read(Pin: Pin_Type) return Boolean is
  begin
    return Gpio.Digital_Read(Interfaces.C.Int(Pin));
  end Read;


  task body Motor_Task is

    Current_Step                              : Step_Type := 0;
    Wanted_Step                               : Step_Type;
    State                                     : Motor_State_Type := Starting;
    Emg_Stop                                  : Boolean := False;
    Local_Direction_Towards_Emergency_Stop    : Direction_Type;
    Current_Direction                         : Direction_Type;
    Pin : Pin_Array_Type;

  begin
    accept Config(Configuration_Pin : Pin_Array_Type; Direction_Towards_Emergency_Stop : Direction_Type) do
      Pin := Configuration_Pin;
      Local_Direction_Towards_Emergency_Stop := Direction_Towards_Emergency_Stop;
      Current_Direction := Direction_Towards_Emergency_Stop;

      Gpio.Pin_Mode( Interfaces.C.Int(Pin(Emergency_Stop)) , Gpio.INPUT);
      Gpio.Pull_Up_Dn_Control(Interfaces.C.Int(Pin(Emergency_Stop)), Gpio. PUD_DOWN);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Direction)), Gpio.OUTPUT);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Step)), Gpio.OUTPUT);
      Gpio.Pin_Mode(Interfaces.C.Int(Pin(Enable)), Gpio.OUTPUT);
      Write(Pin(Direction), Gpio.LOW);
      Write(Pin(Step), Gpio.LOW);
      Write(Pin(Enable), Gpio.LOW); -- Low is to enable
    end Config;

    loop
      select

        accept Home do
          Wanted_Step := Step_Type'First;
        end Home;

        Current_Direction := Local_Direction_Towards_Emergency_Stop;
        loop
          Emg_Stop := Read(Pin(Emergency_Stop));
          if Emg_Stop then -- reached stop. go back until not affected any more

            if Local_Direction_Towards_Emergency_Stop = Cw then
              Current_Direction := CCw;
            else
              Current_Direction := Cw;
            end if;

            Write(Pin(Step), Gpio.HIGH);
            delay Delay_Time(Slow);
            Write(Pin(Step), Gpio.LOW);
            delay Delay_Time(Slow);
            Current_Step := 0; -- reset the step from here
            State := Running;
            exit when not Read(Pin(Emergency_Stop));

          else -- tic on more towards emg_stop
            Write(Pin(Step), Gpio.HIGH);
            delay Delay_Time(Slow);
            Write(Pin(Step), Gpio.LOW);
            delay Delay_Time(Slow);
          end if;
        end loop;

      or
        when State = Running =>
          accept Goto_Step(S : Step_Type) do
            Wanted_Step := S;
          end Goto_Step;

          Move_Loop : loop
            Emg_Stop := Read(Pin(Emergency_Stop));
            if Emg_Stop then
              Write(Pin(Enable), Gpio.Low); -- shut down power to stepper
              exit; -- error msg?
            end if;

            if Wanted_Step > Current_Step then
              -- do we need to switch direction ?
              case Local_Direction_Towards_Emergency_Stop is
              when CCw =>
                if Current_Direction = CCw then
                    -- change direction
                  Current_Direction := Cw;
                  Write(Pin(Direction), Current_Direction);
                end if;

              when Cw  =>
                if Current_Direction = Cw then
                    -- change direction
                  Current_Direction := Ccw;
                  Write(Pin(Direction), Current_Direction);
                end if;
              end case;

              Write(Pin(Step), Gpio.HIGH);
              delay Delay_Time(Normal);
              Write(Pin(Step), Gpio.LOW);
              delay Delay_Time(Normal);

              if Current_Direction = Local_Direction_Towards_Emergency_Stop then
                Current_Step := Current_Step -1 ;  --Towards 0
              else
                Current_Step := Current_Step +1 ;  --Towards inifinty
              end if;


            elsif  Wanted_Step < Current_Step then
              -- do we need to switch direction ?
              case Local_Direction_Towards_Emergency_Stop is
              when CCw =>
                if Current_Direction = Cw then
                    -- change direction
                  Current_Direction := Ccw;
                  Write(Pin(Direction), Current_Direction);
                end if;

              when Cw =>
                if Current_Direction = CCw then
                    -- change direction
                  Current_Direction := Cw;
                  Write(Pin(Direction), Current_Direction);
                end if;
              end case;

              Write(Pin(Step), Gpio.HIGH);
              delay Delay_Time(Normal);
              Write(Pin(Step), Gpio.LOW);
              delay Delay_Time(Normal);


              if Current_Direction = Local_Direction_Towards_Emergency_Stop then
                Current_Step := Current_Step -1 ;  --Towards 0
              else
                Current_Step := Current_Step +1 ;  --Towards inifinty
              end if;

            else
              exit Move_Loop; --done
            end if;

          end loop Move_Loop;


      end select;
    end loop;
  exception
    when E: others =>
      Stacktrace.Tracebackinfo(E);
  end Motor_Task;



end Motors;
