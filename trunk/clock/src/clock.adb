with Text_Io; use Text_Io;
with Steppers;
with Joystick;
with Interfaces.C;


procedure Clock is
  package C renames Interfaces.C;
  use type C.Short;

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
    --  Log("Handle_Events","Start");
    Joystick.Read_Event(Reading_Ok => Ok, Jse => Event) ;
    if not OK then
      Log("Handle_Events","BAD READ");
      -- Quit := True;
      return;
    end if;
    Log("Handle_Events","an event occurred");
    case Event.C_Type is

      when Joystick.Js_Event_Init_Button =>
          null;

      when Joystick.Js_Event_Init_Axis =>
          null;

      when Joystick.Js_Event_Button =>
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
                             Steppers.Set_Speed(Steppers.Normal);
              when 1      => Log("Handle_Events","fast speed");
                             Steppers.Set_Speed(Steppers.Fast);
              when others => null;
            end case;
          when others =>  null; --buttons
        end case;

      when Joystick.Js_Event_Axis =>
        case Event.Number is -- Axis
          when 4      =>       --left/right (hat)
            case Event.Value is --
              when -32768 .. -1 =>  --left pressed
                Log("Handle_Events","HAT_LEFT");
                Steppers.Left;
              when 0            => -- released/centered
                Log("Handle_Events","HAT_CENTERED");
                Steppers.No_Direction;
              when 1 .. 32767 =>  --right pressed
                Log("Handle_Events","HAT_RIGHT");
                Steppers.Right;
            end case;
          when 5      =>    --up/down   (hat)
            case Event.Value is -- 1=pressed, 0=released
              when -32768 .. -1 =>  --leupft pressed
                Log("Handle_Events","HAT_UP");
                Steppers.Up;
              when 0            => -- released/centered
                Log("Handle_Events","HAT_CENTERED");
                Steppers.No_Direction;
              when 1 .. 32767 =>  --down pressed
                Log("Handle_Events","HAT_DOWN");
                Steppers.Down;
            end case;
          when others =>  null; --axises
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
  Log("main","test start");
  Steppers.Test;
  Log("main","test stop");
  Steppers.Stop;
  return;
  
  
  Joystick.Open;

  loop
    Handle_Events(Quit);
    exit when Quit;
  end loop;

  Steppers.Stop;
  Joystick.Close;
exception
  when others =>
    Steppers.Stop;
    Joystick.Close;

end Clock;
