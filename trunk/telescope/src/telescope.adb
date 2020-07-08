with Text_Io; use Text_Io;
with Motors;
with Joystick;
with Interfaces.C;
--with Gpio;


procedure Telescope is
  package C renames Interfaces.C;
  use type C.Short;

  Motor_1 : Motors.Motor_Task renames Motors.M(2); -- left
  Motor_2 : Motors.Motor_Task renames Motors.M(3); -- right


  procedure Log(Who,What : in String) is
  begin
    --Put_Line(Calendar2.Clock.To_String & " " & Who & " " & What);
    Put_Line( Who & " " & What);
  end Log;

  procedure Handle_Events(Quit : out Boolean) is
    Ok : Boolean := False;
    Event : Joystick.Js_Event;
  begin
    Quit := False;
    Log("Handle_Events","Start");
    Joystick.Read_Event(Reading_Ok => Ok, Jse => Event) ;
    if not OK then
      Log("Handle_Events","BAD READ");
      -- Quit := True;
      return;
    end if;
    Log("Handle_Events","an event occurred -> time" & Event.Time'img &
                  " value " & Event.Value'Img &
                  " c_type " & Event.C_Type'Img &
                  " number " & Event.Number'Img);

    case Event.C_Type is

      when Joystick.Js_Event_Init_Button =>
        Log("Handle_Events","Js_Event_Init_Button");
          null;

      when Joystick.Js_Event_Init_Axis =>
        Log("Handle_Events","Js_Event_Init_Axis");
          null;

      when Joystick.Js_Event_Button =>
        Log("Handle_Events","Js_Event_Button");

        case Event.Number is -- Button
          when 8      =>
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","QUITing");
                             Quit := True;
              when 1      => null;
              when others => null;
            end case;
          when 1      =>
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","Normal speed");
                           --  Steppers.Set_Speed(Steppers.Normal);
              when 1      => Log("Handle_Events","fast speed");
                         --    Steppers.Set_Speed(Steppers.Fast);
              when others => null;
            end case;
          when others =>  null; --buttons
        end case;

      when Joystick.Js_Event_Axis =>
        Log("Handle_Events","Js_Event_Axis");
        case Event.Number is -- Axis
          when 4      =>       --left/right (hat)
            case Event.Value is --
              when -32768 .. -1 =>  --left pressed
                Log("Handle_Events","HAT_LEFT");
                Motors.Set_Direction(Motors.Ccw);
              when 0            => -- released/centered
                Log("Handle_Events","HAT_CENTERED");
                Motors.Set_Direction(Motors.None);
              when 1 .. 32767 =>  --right pressed
                Log("Handle_Events","HAT_RIGHT");
                Motors.Set_Direction(Motors.Cw);
            end case;
          when 5      =>    --up/down   (hat)
            case Event.Value is -- 1=pressed, 0=released
              when -32768 .. -1 =>  --leupft pressed
                Log("Handle_Events","HAT_UP");
                Motors.Set_Direction(Motors.Up);
                Motor_1.Go;
                Motor_2.Go;
              when 0            => -- released/centered
                Log("Handle_Events","HAT_CENTERED");
                Motors.Set_Direction(Motors.None);
              when 1 .. 32767 =>  --down pressed
                Log("Handle_Events","HAT_DOWN");
                Motors.Set_Direction(Motors.Down);
                Motor_1.Go;
                Motor_2.Go;
            end case;
          when others =>  null; --axises
            Log("Handle_Events","other event");
        end case;

      when others => null;
        Log("Handle_Events","Event.The_Type" & Event.C_Type'Img );
    end case;
    -- Log("Handle_Events","Stop");
  end Handle_Events;
  -----------------------------------------------

  Quit             : Boolean := False;

begin

  --Gpio.Setup;
  Log("main","init steppers");
  --Init The Motors with pins
  Put_Line("Config and init motors");
  declare
    use Motors;
  begin
    Motor_1.Config(Configuration_Pin => (Step => 16, Direction => 12, Enable => 20, Emergency_Stop => 1), Name => 1);
    Motor_2.Config(Configuration_Pin => (Step => 19, Direction => 13, Enable => 26, Emergency_Stop => 0), Name => 2);
 --   Motor_Z.Config  (Configuration_Pin => (Step => 27, Direction => 22, Enable =>  6, Emergency_Stop => 4), Direction_Towards_Emergency_Stop => CCw, Name => 3);
  end;
  --Log("main","test start");
  --Steppers.Test;
  --Log("main","test stop");

  Joystick.Open;

  loop
    Handle_Events(Quit);
    exit when Quit;
  end loop;

--  Steppers.Stop;
  Joystick.Close;
exception
  when others =>
--    Steppers.Stop;
    Joystick.Close;

end Telescope;
