with Interfaces.C.Strings;
with GNAT.OS_Lib;
--with Utils; use Utils;
with SDL;
--with SDL.Video;
with SDL.Error;
with SDL.Events;
with SDL.Quit;
with SDL.Keyboard;
--with SDL.Timer;
--with SDL.Types; use SDL.Types;
with SDL.Joystick;
with Text_Io; use Text_Io;

with Interfaces.C;

procedure Tele is
   package C renames Interfaces.C;
   use type C.int;
   use type C.double;
   use type C.unsigned;
    use type SDL.Init_Flags;

  procedure Log(Who,What : in String) is
  begin
    --Put_Line(Calendar2.Clock.To_String & " " & Who & " " & What);
    Put_Line( Who & " " & What);
  end Log;



  procedure Handle_Events(Quit : out Boolean) is
   Event : SDL.Events.Event;
   Pollevent_Result : C.Int;
  begin
    Quit := False;
  --  Log("Handle_Events","Start");
    loop
      SDL.Events.PollEventVP (PollEvent_Result, Event);
      exit when PollEvent_Result = 0;
      Log("Handle_Events","an event occurred");
      case Event.The_Type is
         when SDL.Events.KEYDOWN =>
           declare
             -- player 0 -> w,a,s,z
             -- player 1 -> up,left,rigt,down
             Name_Of_Key : String := C.Strings.Value(SDL.Keyboard.GetKeyName(Event.key.keysym.sym));
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

         when SDL.Events.KEYUP =>
           null;
           declare
             -- player 0 -> w,a,s,z
             -- player 1 -> up,left,rigt,down
             Name_Of_Key : String := C.Strings.Value(SDL.Keyboard.GetKeyName(Event.key.keysym.sym));
           begin
             Log("Handle_Events"," Key up: '" & Name_Of_Key & "'");
             if Name_Of_Key = "escape" then
               Quit := True;
             end if;
           end;

         when SDL.Events.QUIT =>
           Log("Handle_Events"," QUIT Event.The_Type " & Event.The_Type'Img );
           Quit := True;

         when SDL.Events.MOUSEBUTTONDOWN =>
           Log("Handle_Events"," MOUSEBUTTONDOWN x,y:" & Event.Button.X'Img & "," & Event.Button.Y'Img);

         when SDL.Events.JOYAXISMOTION =>
           Log("Handle_Events"," JOYAXISMOTION Which,Axis,Value:" & Event.Jaxis.Which'Img & "," &
                                                                       Event.Jaxis.Axis'Img & "," &
                                                                       Event.Jaxis.Value'Img );
         when SDL.Events.JOYBALLMOTION =>
           Log("Handle_Events"," JOYBALLMOTION Which,Ball,Xrel,Yrel:" & Event.Jball.Which'Img & "," &
                                                                       Event.Jball.Ball'Img & "," &
                                                                       Event.Jball.Xrel'Img & "," &
                                                                       Event.Jball.Yrel'Img );

         when SDL.Events.JOYBUTTONDOWN =>
           Log("Handle_Events"," JOYBUTTONDOWN Which,Button,State:" & Event.Jbutton.Which'Img & "," &
                                                                       Event.Jbutton.Button'Img & "," &
                                                                       Event.Jbutton.State'Img );

         when SDL.Events.JOYBUTTONUP =>
           Log("Handle_Events"," JOYBUTTONUP Which,Button,State:" & Event.Jbutton.Which'Img & "," &
                                                                       Event.Jbutton.Button'Img & "," &
                                                                       Event.Jbutton.State'Img );

         when SDL.Events.JOYHATMOTION =>
           Log("Handle_Events"," JOYHATMOTION Which,Hat,Value:" & Event.Jhat.Which'Img & "," &
                                                                       Event.Jhat.Hat'Img & "," &
                                                                       Event.Jhat.Value'Img);

           case Event.Jhat.Value is
              when SDL.Joystick.HAT_CENTERED => Log("Handle_Events","HAT_CENTERED");
              when SDL.Joystick.HAT_UP       => Log("Handle_Events","HAT_UP");
              when SDL.Joystick.HAT_RIGHT    => Log("Handle_Events","HAT_RIGHT");
              when SDL.Joystick.HAT_DOWN     => Log("Handle_Events","HAT_DOWN");
              when SDL.Joystick.HAT_LEFT     => Log("Handle_Events","HAT_LEFT");
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
  Quit : Boolean := False;
  Joy_Ptr  : array (0..1) of SDL.Joystick.Joystick_Ptr := (others => SDL.Joystick.Null_Joystick_Ptr);
  --Result : C.Int := 0;

begin
    Log("main","Hello");
    if SDL.Init (SDL.INIT_JOYSTICK + SDL.INIT_VIDEO) < 0 then
       Log ("main.Init","Couldn't initialize SDL: " &
                 C.Strings.Value (SDL.Error.GetError));
       GNAT.OS_Lib.OS_Exit (1);
    end if;

    SDL.Quit.Atexit (SDL.SDL_Quit'Access);

    Number_Of_Joysticks := SDL.Joystick.NumJoysticks;
    Log("main.Init","NumJoysticks:" & Number_Of_Joysticks'Img);
    for i in 0 .. Number_Of_Joysticks -1 loop
     Joy_Ptr(integer(i)) := SDL.Joystick.JoystickOpen(i);
     Log("main.Init","opened: " & Interfaces.C.Strings.Value(SDL.Joystick.JoystickName(i)));
    end loop;

    --enable joy events
    if SDL.Joystick.JoystickEventState (SDL.Events.Enable) < 0 then
       Log ("main.Init","Couldn't enable joystick events: " &
                 C.Strings.Value (SDL.Error.GetError));
       GNAT.OS_Lib.OS_Exit (1);
    end if;


    loop
      Handle_Events(Quit);
      exit when Quit;

      delay 0.1;
    end loop;

end Tele;
