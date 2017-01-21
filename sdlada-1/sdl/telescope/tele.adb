with Interfaces.C.Strings;
with Gnat.Os_Lib;
with Sdl;
with Sdl.Error;
with Sdl.Events;
with Sdl.Quit;
with Sdl.Keyboard;
with Sdl.Joystick;
with Text_Io; use Text_Io;
with Steppers;
with Interfaces.C;

procedure Tele is
  package C renames Interfaces.C;
  use type C.Int;
  use type C.Double;
  use type C.Unsigned;
  use type Sdl.Init_Flags;

  procedure Log(Who,What : in String) is
  begin
    --Put_Line(Calendar2.Clock.To_String & " " & Who & " " & What);
    Put_Line( Who & " " & What);
  end Log;

  procedure Handle_Events(Quit : out Boolean) is
    Event            : Sdl.Events.Event;
    Pollevent_Result : C.Int;
  begin
    Quit := False;
    --  Log("Handle_Events","Start");
    loop
      Sdl.Events.Polleventvp (Pollevent_Result, Event);
      exit when Pollevent_Result = 0;
      Log("Handle_Events","an event occurred");
      case Event.The_Type is
        when Sdl.Events.Keydown =>
          declare
            -- player 0 -> w,a,s,z
            -- player 1 -> up,left,rigt,down
            Name_Of_Key : String := C.Strings.Value(Sdl.Keyboard.Getkeyname(Event.Key.Keysym.Sym));
          begin
            Log("Handle_Events"," Key down: '" & Name_Of_Key & "'");
            if    Name_Of_Key = "up" then null;
            elsif Name_Of_Key = "right" then null;
            elsif Name_Of_Key = "down" then null;
            elsif Name_Of_Key = "left" then null;
            elsif Name_Of_Key = "w" then null;
            elsif Name_Of_Key = "s" then null;
            elsif Name_Of_Key = "z" then null;
            elsif Name_Of_Key = "a" then null;
            end if;
          end;

        when Sdl.Events.Keyup =>
          null;
          declare
            -- player 0 -> w,a,s,z
            -- player 1 -> up,left,rigt,down
            Name_Of_Key : String := C.Strings.Value(Sdl.Keyboard.Getkeyname(Event.Key.Keysym.Sym));
          begin
            Log("Handle_Events"," Key up: '" & Name_Of_Key & "'");
            if Name_Of_Key = "escape" then
              Quit := True;
            end if;
          end;

        when Sdl.Events.Quit =>
          Log("Handle_Events"," QUIT Event.The_Type " & Event.The_Type'Img );
          Quit := True;

        when Sdl.Events.Mousebuttondown =>
          Log("Handle_Events"," MOUSEBUTTONDOWN x,y:" & Event.Button.X'Img & "," & Event.Button.Y'Img);

        when Sdl.Events.Joyaxismotion =>
          Log("Handle_Events"," JOYAXISMOTION Which,Axis,Value:" & Event.Jaxis.Which'Img & "," &
                Event.Jaxis.Axis'Img & "," &
                Event.Jaxis.Value'Img );
        when Sdl.Events.Joyballmotion =>
          Log("Handle_Events"," JOYBALLMOTION Which,Ball,Xrel,Yrel:" & Event.Jball.Which'Img & "," &
                Event.Jball.Ball'Img & "," &
                Event.Jball.Xrel'Img & "," &
                Event.Jball.Yrel'Img );

        when Sdl.Events.Joybuttondown =>
          Log("Handle_Events"," JOYBUTTONDOWN Which,Button,State:" & Event.Jbutton.Which'Img & "," &
                Event.Jbutton.Button'Img & "," &
                Event.Jbutton.State'Img );
          case Event.Jbutton.Button is
            when 8      => Log("Handle_Events","QUITing");
              Quit := True;
            when 2      => Log("Handle_Events","fast speed");
              Steppers.Set_Speed(Steppers.Fast);
            when others => Log("Handle_Events","others");
          end case;

        when Sdl.Events.Joybuttonup =>
          Log("Handle_Events"," JOYBUTTONUP Which,Button,State:" & Event.Jbutton.Which'Img & "," &
                Event.Jbutton.Button'Img & "," &
                Event.Jbutton.State'Img );
          case Event.Jbutton.Button is
            when 8      => Log("Handle_Events","QUITing");
              Quit := True;
            when 2      => Log("Handle_Events","Normal speed");
              Steppers.Set_Speed(Steppers.Normal);
            when others => Log("Handle_Events","others");
          end case;


        when Sdl.Events.Joyhatmotion =>
          Log("Handle_Events"," JOYHATMOTION Which,Hat,Value:" & Event.Jhat.Which'Img & "," &
                Event.Jhat.Hat'Img & "," &
                Event.Jhat.Value'Img);

          case Event.Jhat.Value is
            when Sdl.Joystick.Hat_Centered => Log("Handle_Events","HAT_CENTERED");
              Steppers.Stop;
            when Sdl.Joystick.Hat_Up       => Log("Handle_Events","HAT_UP");
              Steppers.Up;
            when Sdl.Joystick.Hat_Right    => Log("Handle_Events","HAT_RIGHT");
              Steppers.Right;
            when Sdl.Joystick.Hat_Down     => Log("Handle_Events","HAT_DOWN");
              Steppers.Down;
            when Sdl.Joystick.Hat_Left     => Log("Handle_Events","HAT_LEFT");
              Steppers.Left;
            when others                    => Log("Handle_Events","others");
          end case;

        when others => null;
          Log("Handle_Events","Event.The_Type " & Event.The_Type'Img );
      end case;
    end loop;

    -- Log("Handle_Events","Stop");
  end Handle_Events;
  -----------------------------------------------

  Number_Of_Joysticks : C.Int := 0;
  Quit             : Boolean := False;
  Joy_Ptr          : array (0..1) of Sdl.Joystick.Joystick_Ptr := (others => Sdl.Joystick.Null_Joystick_Ptr);
  --Result : C.Int := 0;

begin
  Steppers.Init;

  Log("main","Hello");
  if Sdl.Init (Sdl.Init_Joystick + Sdl.Init_Video) < 0 then
    Log ("main.Init","Couldn't initialize SDL: " &
           C.Strings.Value (Sdl.Error.Geterror));
    Gnat.Os_Lib.Os_Exit (1);
  end if;

  Sdl.Quit.Atexit (Sdl.Sdl_Quit'Access);

  Number_Of_Joysticks := Sdl.Joystick.Numjoysticks;
  Log("main.Init","NumJoysticks:" & Number_Of_Joysticks'Img);
  for I in 0 .. Number_Of_Joysticks -1 loop
    Joy_Ptr(Integer(I)) := Sdl.Joystick.Joystickopen(I);
    Log("main.Init","opened: " & Interfaces.C.Strings.Value(Sdl.Joystick.Joystickname(I)));
  end loop;

  --enable joy events
  if Sdl.Joystick.Joystickeventstate (Sdl.Events.Enable) < 0 then
    Log ("main.Init","Couldn't enable joystick events: " &
           C.Strings.Value (Sdl.Error.Geterror));
    Gnat.Os_Lib.Os_Exit (1);
  end if;

  loop
    Handle_Events(Quit);
    exit when Quit;

    delay 0.1;
  end loop;
  Steppers.Stop;
exception
  when others =>
    Steppers.Stop;

end Tele;
