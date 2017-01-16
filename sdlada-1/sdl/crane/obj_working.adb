with Interfaces.C.Strings;
with GNAT.OS_Lib;
with Utils; use Utils;
with SDL;
--with SDL.Video;
with SDL.Error;
with SDL.Events;
with SDL.Quit;
with SDL.Keyboard;


package body Obj is
   use type C.int;
   use type C.double;
   use type C.unsigned;
   use type SDL.Video.Surface_Flags;
   use type SDL.Video.Surface_Ptr;

  procedure Init(Game : in out Game_Type) is
    Video_Bpp : Uint8 := 8;
    Videoflags : SDL.Video.Surface_Flags;
    Screen_Width  : constant :=  640; -- 640 800 1024
    Screen_Height : constant :=  480; -- 480 600  768
    OK : Boolean := False;
    use type SDL.Init_Flags;
    Number_Of_Joysticks : C.Int := 0;
  begin 
    Log("Game.Init","Start");
    Game.Is_Running := True;
    if SDL.Init (SDL.INIT_VIDEO + SDL.INIT_JOYSTICK + SDL.INIT_TIMER) < 0 then
       Log ("Game.Init","Couldn't initialize SDL: " &
                 C.Strings.Value (SDL.Error.GetError));
       GNAT.OS_Lib.OS_Exit (1);
    end if;  
    
    SDL.Quit.Atexit (SDL.SDL_Quit'Access);
    Videoflags := SDL.Video.SWSURFACE or SDL.Video.ANYFORMAT;
    -- makes fullscreen + mess up other windows on dual screen
    -- Videoflags := Utils.Fastest_Flags(Videoflags);

    --  Set video mode
    Game.Screen := SDL.Video.SetVideoMode (Screen_Width, Screen_Height,
                             C.Int (Video_Bpp), Videoflags);
       
    if Game.Screen = null then
       Log ("Game.Init","Couldn't set " & Integer'Image (Screen_Width) &
                 "x" & Integer'Image (Screen_Height) &
                 " video mode: " &  C.Strings.Value (SDL.Error.GetError));
       GNAT.OS_Lib.OS_Exit (1);
    end if;
    
    Game.Load_Sprite("../images/tankbrigade.bmp", OK);
    if not OK then
      Log("Game.Init","Load_Sprite failed, exit");
      GNAT.OS_Lib.OS_Exit (1);
    end if;
   
    Game.Background := SDL.Video.MapRGB (Game.Screen.Format, 16#0#, 16#0#, 16#0#); 
    Game.Sprite_Rects := new SDL.Video.Rects_Array (0 .. 0);
    Game.Positions    := new SDL.Video.Rects_Array (0 .. 0);
    Game.Velocities   := new SDL.Video.Rects_Array (0 .. 0);
    
    Game.Positions (0).X  := 100;
    Game.Positions (0).Y  := 100;
    Game.Positions (0).W  := Uint16 (Game.Sprite.W);
    Game.Positions (0).H  := Uint16 (Game.Sprite.H);
    Game.Velocities (0).X := 0;
    Game.Velocities (0).Y := 0;
        
    Number_Of_Joysticks := SDL.Joystick.NumJoysticks;
    Log("Game.Init","NumJoysticks:" & Number_Of_Joysticks'Img);
    for i in 0 .. Number_Of_Joysticks -1 loop
      Game.Jsticks(Integer(i)) := SDL.Joystick.JoystickOpen(i);
      Log("Game.Init","opened: " & Interfaces.C.Strings.Value(SDL.Joystick.JoystickName(i))); 
    end loop;    
    
    -- Jstick        
    Log("Game.Init","Stop");
  end Init;
  
  procedure Load_Sprite (Game : in out Game_Type; File : in String; Result : out Boolean) is
    Temp   : SDL.Video.Surface_Ptr;
    use SDL.Types.Uint8_Ptrs;      
  --  use type SDL.Video.Surface_Ptr;
    use type SDL.Video.Palette_Ptr;
  --  use type SDL.Video.Surface_Flags;
   -- use type Interfaces.C.Int;
  begin
    Log("Utils.LoadSprite", "start");
    Result := True;
    --  Load the sprite image
    Game.Sprite := SDL.Video.LoadBMP (Interfaces.C.Strings.New_String(file));
    if Game.Sprite = null then
      Log("Utils.LoadSprite", "Couldn't load " & File & " : " & SDL.Error.Get_Error);
      Result := False;
      return ;
    end if;
  
    --  Set transparent pixel as the pixel at (0,0)
    if Game.Sprite.Format.Palette /= null then
      SDL.Video.SetColorKey (
          Game.Sprite,
          (SDL.Video.SRCCOLORKEY or SDL.Video.RLEACCEL),
          Uint32 (To_Pointer (Game.Sprite.Pixels).all));
    end if;
  
    --  Convert sprite to video format
    Temp := SDL.Video.DisplayFormat(Game.Sprite);
    if Temp = null then
      Log("Utils.LoadSprite", "Couldn't convert background: " &
                 Interfaces.C.Strings.Value (SDL.Error.GetError));
      Result := False;
      return ;
    end if;
    Game.Sprite := Temp;
    --  we're ready to roll. :)
    Log("Utils.LoadSprite", "stop");
  end Load_Sprite;
  
  
  procedure Update(Game : in out Game_Type) is
  begin 
    Log("Game.Update","Start");
    Log("Game.Update","Stop");
  end Update;
  
  procedure Render(Game : in out Game_Type) is
    Nupdates : C.unsigned := 0;
    Area : SDL.Video.Rect;
    FromArea : SDL.Video.Rect;
  begin 
    Log("Game.Render","Start");
    --  Erase all sprites if necessary
    SDL.Video.FillRect(Game.Screen, null, Game.Background);

    --  Move the sprite, bounce at the wall, and draw
    for i in Game.Sprite_Rects'Range loop
       Game.Positions(i).X := Game.Positions(i).X + Game.Velocities(i).X;
       if (Game.Positions(i).X < 0) or (C.Int (Game.Positions(i).X) >= Game.Screen.W) then
          Game.Velocities(i).X := -Game.Velocities(i).X;
          Game.Positions(i).X := Game.Positions(i).X + Game.Velocities(i).X;
       end if;
       Game.Positions(i).Y := Game.Positions(i).Y + Game.Velocities(i).Y;
       if (Game.Positions(i).Y < 0) or (C.Int (Game.Positions(i).Y) >= Game.Screen.H) then
          Game.Velocities(i).Y := -Game.Velocities(i).Y;
          Game.Positions(i).Y := Game.Positions(i).Y + Game.Velocities(i).Y;
       end if;

       --  Blit the sprite onto the screen
       Area := Game.Positions(i);
       FromArea :=(33*Game.X_Frame,33,32,32);
       if Game.X_Frame < 15 then
         Game.X_Frame := 15;
       elsif Game.X_Frame < 22 then
         Game.X_Frame := Game.X_Frame +1;
       else
         Game.X_Frame := 15;
       end if;
       Log("Game.Render"," Game.X_Frame:" &  Game.X_Frame'Img);
--       delay 1.0;     
--       SDL.Video.BlitSurface (Game.Sprite, null, Game.Screen, Area);
       SDL.Video.BlitSurface (Game.Sprite, FromArea, Game.Screen, Area);
       Game.Sprite_Rects (Nupdates) := Area;
       Nupdates := Nupdates + 1;
    end loop;

    --  Update the screen
    if (Game.Screen.Flags and SDL.Video.DOUBLEBUF) = SDL.Video.DOUBLEBUF then
       SDL.Video.Flip (Game.Screen);
    else
       SDL.Video.UpdateRects (Game.Screen, C.Int (Nupdates), Game.Sprite_Rects.all);
    end if;

    Log("Game.Render","Stop");
  end Render;
  
  procedure Handle_Events(Game : in out Game_Type) is
   Event : SDL.Events.Event;
   Pollevent_Result : C.Int;  
  begin 
    Log("Game.Handle_Events","Start");
    loop
      SDL.Events.PollEventVP (PollEvent_Result, Event);
      exit when PollEvent_Result = 0;
      case Event.The_Type is
         when SDL.Events.KEYDOWN =>
            --  any keypress quits the app
           Log("Game.Handle_Events"," Key " & C.Strings.Value(SDL.Keyboard.GetKeyName(Event.key.keysym.sym)) );
         when SDL.Events.QUIT =>
           Log("Game.Handle_Events"," QUIT Event.The_Type " & Event.The_Type'Img );
           Game.Is_Running := False;
         when SDL.Events.MOUSEBUTTONDOWN =>         
           Log("Game.Handle_Events"," MOUSEBUTTONDOWN x,y:" & Event.Button.X'Img & "," & Event.Button.Y'Img);
           Log("Game.Handle_Events"," (Game.Velocities(0) x,y:" & Game.Positions(0).X'Img & "," & Game.Positions(0).Y'Img);
           if Event.Button.X > Uint16(Game.Positions(0).X) then 
             Game.Velocities (0).X := 1;
           elsif Event.Button.X < Uint16(Game.Positions(0).X) then 
             Game.Velocities (0).X := -1;
           end if;
         
           if Event.Button.Y > Uint16(Game.Positions(0).Y) then 
             Game.Velocities (0).Y := 1;
           elsif Event.Button.Y < Uint16(Game.Positions(0).Y) then 
             Game.Velocities (0).Y := -1;
           end if;

         when SDL.Events.JOYAXISMOTION =>
           Log("Game.Handle_Events"," JOYAXISMOTION Which,Axis,Value:" & Event.Jaxis.Which'Img & "," & 
                                                                       Event.Jaxis.Axis'Img & "," & 
                                                                       Event.Jaxis.Value'Img ); 
         when SDL.Events.JOYBALLMOTION =>
           Log("Game.Handle_Events"," JOYBALLMOTION Which,Ball,Xrel,Yrel:" & Event.Jball.Which'Img & "," & 
                                                                       Event.Jball.Ball'Img & "," & 
                                                                       Event.Jball.Xrel'Img & "," & 
                                                                       Event.Jball.Yrel'Img ); 

         when SDL.Events.JOYBUTTONDOWN =>
           Log("Game.Handle_Events"," JOYBUTTONDOWN Which,Button,State:" & Event.Jbutton.Which'Img & "," & 
                                                                       Event.Jbutton.Button'Img & "," & 
                                                                       Event.Jbutton.State'Img ); 
                                                                       
         when SDL.Events.JOYBUTTONUP =>
           Log("Game.Handle_Events"," JOYBUTTONUP Which,Button,State:" & Event.Jbutton.Which'Img & "," & 
                                                                       Event.Jbutton.Button'Img & "," & 
                                                                       Event.Jbutton.State'Img ); 

         when SDL.Events.JOYHATMOTION =>
           Log("Game.Handle_Events"," JOYHATMOTION Which,Hat,Value:" & Event.Jhat.Which'Img & "," & 
                                                                       Event.Jhat.Hat'Img & "," & 
                                                                       Event.Jhat.Value'Img); 
           
         when others => null;
           Log("Game.Handle_Events","Event.The_Type " & Event.The_Type'Img );
      end case;
    end loop;


    Log("Game.Handle_Events","Stop");
  end Handle_Events;
  
  function Running(Game : Game_Type) return Boolean is
  begin
    Log("Game.Running","Start");
    Log("Game.Running","Stop");
    return Game.Is_Running;
  end Running;  
  
  -------------------- End Game ----------------------------------------
  -------------------- Start SDL_Game_Object ---------------------------
  
  overriding procedure Init(O   : in out SDL_Game_Object; Joystick_Index : Integer) is 
  begin
    O.Joystick_Index := Joystick_Index;
  end Init;
  
  overriding procedure Draw(O   : in out SDL_Game_Object) is
  begin
    null;
  end Draw;
  
  overriding procedure Update(O : in out SDL_Game_Object) is
  begin
    null;
  end Update;  
  
  overriding procedure Clean(O  : in out SDL_Game_Object) is
  begin
    null;
  end Clean;
  -------------------- Stop SDL_Game_Object ---------------------------
   
  
end Obj;