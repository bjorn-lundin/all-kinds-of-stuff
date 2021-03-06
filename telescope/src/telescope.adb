with Text_Io; use Text_Io;
with Motors;
with Joystick;
with Interfaces.C;
--with Gpio;
with Steppers;


procedure Telescope is
  package C renames Interfaces.C;
  use type C.Short;

  Motor_2 : Motors.Motor_Task renames Motors.M(2); -- left
  Motor_3 : Motors.Motor_Task renames Motors.M(3); -- right


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
          when 1      =>
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","Normal speed");
                             Steppers.Set_Slow_Is_Pressed(False);
                             Motors.Set_Speed(Motors.Normal);

              when 1      => Log("Handle_Events","slow speed");
                             Steppers.Set_Slow_Is_Pressed(True);
                             Motors.Set_Speed(Motors.Slow);
              when others => null;
            end case;

          when 4      => --Left High Indexfinger
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","focus_off");
                             Steppers.No_Direction;
              when 1      => Log("Handle_Events","focus_plus slow");
                             Steppers.Focus_Plus_Slow;
              when others => null;
            end case;

          when 5      => --right High Indexfinger
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","focus off");
                             Steppers.No_Direction;
              when 1      => Log("Handle_Events","focus_minus slow");
                             Steppers.Focus_Minus_Slow;
              when others => null;
            end case;

          when 6      => --Left Low Indexfinger
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","focus_off");
                             Steppers.No_Direction;
              when 1      => Log("Handle_Events","focus_plus");
                             Steppers.Focus_Plus;
              when others => null;
            end case;

          when 7      => --right Low Indexfinger
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","focus off");
                             Steppers.No_Direction;
              when 1      => Log("Handle_Events","focus_minus");
                             Steppers.Focus_Minus;
              when others => null;
            end case;
          when 8      =>
            case Event.Value is -- 1=pressed, 0=released
              when 0      => Log("Handle_Events","QUITing");
                             Quit := True;
              when 1      => null;
              when others => null;
            end case;
          when others =>  null; --buttons
        end case;

      when Joystick.Js_Event_Axis =>
        Log("Handle_Events","Js_Event_Axis");
        case Event.Number is -- Axis
          when 0      =>       --left/right (hat)
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
          when 1      =>    --up/down   (hat)
            case Event.Value is -- 1=pressed, 0=released
              when -32768 .. -1 =>  --leupft pressed
                Log("Handle_Events","HAT_UP");
                Motors.Set_Direction(Motors.Up);
                Motor_2.Go;
                Motor_3.Go;
              when 0            => -- released/centered
                Log("Handle_Events","HAT_CENTERED");
                Motors.Set_Direction(Motors.None);
              when 1 .. 32767 =>  --down pressed
                Log("Handle_Events","HAT_DOWN");
                Motors.Set_Direction(Motors.Down);
                Motor_2.Go;
                Motor_3.Go;
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

  Log("main","init steppers");
  Steppers.Init;
  declare
    use Motors;
  begin
    Motor_2.Config(Configuration_Pin => (Step => 16, Direction => 12, Enable => 20, Emergency_Stop => 1), Name => 2);
    Motor_3.Config(Configuration_Pin => (Step => 19, Direction => 13, Enable => 26, Emergency_Stop => 0), Name => 3);
 --   Motor_Z.Config  (Configuration_Pin => (Step => 27, Direction => 22, Enable =>  6, Emergency_Stop => 4), Direction_Towards_Emergency_Stop => CCw, Name => 3);
  end;


  Joystick.Open;

  --to avoid neeed to press joystick at boot -- toggle it a bit

  Log("main","Motors.Set_Direction(Motors.Up)");
  Motors.Set_Direction(Motors.Up);
  Log("main","Motor_2.Go");
  Motor_2.Go;
  Log("main","Motor_3.Go");
  Motor_3.Go;

  Log("main","Motors.Set_Direction(Motors.None)");
  Motors.Set_Direction(Motors.None); -- to exit the motor loop

  delay 1.0;

  Log("main","Motors.Set_Direction(Motors.Down)");
  Motors.Set_Direction(Motors.Down);
  Log("main","Motor_2.Go"); 
  Motor_2.Go;
  Log("main","Motor_3.Go"); 
  Motor_3.Go;
  Log("main","Motors.Set_Direction(Motors.None)");
  Motors.Set_Direction(Motors.None); -- to exit the motor loop

  delay 1.0;


  Log("main","start event loop");
  loop
    Handle_Events(Quit);
    exit when Quit;
  end loop;

  Log("main","exit");
  Motor_2.Stop;
  Motor_3.Stop;

  Steppers.Stop;
  Joystick.Close;
  Log("main","all closed");
exception
  when others =>
   Joystick.Close;

end Telescope;
