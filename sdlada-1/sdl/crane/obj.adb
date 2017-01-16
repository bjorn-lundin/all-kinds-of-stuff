with Interfaces.C.Strings;
with GNAT.OS_Lib;
with Utils; use Utils;
with SDL;
--with SDL.Video;
with SDL.Error;
with SDL.Events;
with SDL.Quit;
with SDL.Keyboard;
with SDL.Timer;

package body Obj is
   use type C.int;
   use type C.double;
   use type C.unsigned;
   use type SDL.Video.Surface_Flags;
   use type SDL.Video.Surface_Ptr;

  procedure Init(Game : in out Game_Type; Players : Player_Array) is
    Video_Bpp : Uint8 := 8;
    Videoflags : SDL.Video.Surface_Flags;
    Screen_Width  : constant :=  640; -- 640 800 1024
    Screen_Height : constant :=  480; -- 480 600  768
    OK : Boolean := False;
    use type SDL.Init_Flags;
    Number_Of_Joysticks : C.Int := 0;
    Initial_Y,Initial_X : C.Int := 100;    
    
    Moving_Sprite_Coordinates : Moving_Sprite_Coordinates_Array;
    Motion_Sprite_Coordinates : Motion_Sprite_Coordinates_Array ;
    
  begin 
    Log("Game.Init","Start");
    Game.Is_Running := True;
    if SDL.Init (SDL.INIT_VIDEO + SDL.INIT_JOYSTICK + SDL.INIT_TIMER) < 0 then
       Log ("Game.Init","Couldn't initialize SDL: " &
                 C.Strings.Value (SDL.Error.GetError));
       GNAT.OS_Lib.OS_Exit (1);
    end if;  
    
    Game.Player := Players;
    
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
    
    Moving_Sprite_Coordinates := (None => (1,3), Up => (15,2), Right => (20,4), Down => (15,6), Left => (18,3));
    Motion_Sprite_Coordinates := Green_Tank;   
    for i in Game.Player'range loop
      Game.Player(i).Game_Obj_Ptr.Init(Game.Sprite,
                                       Initial_X,
                                       Initial_Y,
                                       Moving_Sprite_Coordinates,
                                       Motion_Sprite_Coordinates); -- sprite set in Load_Sprite
      Initial_X := Initial_X + 200;
      Moving_Sprite_Coordinates := (None => (1,3), Up => (18,6), Right => (20,9), Down => (19,7), Left => (18,8));
      Motion_Sprite_Coordinates := Blue_Tank;   
    end loop;
    
--    Game.Positions    := new SDL.Video.Rects_Array (0 .. 0);
--    Game.Velocities   := new SDL.Video.Rects_Array (0 .. 0);
--    
--    Game.Positions (0).X  := 100;
--    Game.Positions (0).Y  := 100;
--    Game.Positions (0).W  := Uint16 (Game.Sprite.W);
--    Game.Positions (0).H  := Uint16 (Game.Sprite.H);
--    Game.Velocities (0).X := 0;
--    Game.Velocities (0).Y := 0;
        
    Number_Of_Joysticks := SDL.Joystick.NumJoysticks;
    Log("Game.Init","NumJoysticks:" & Number_Of_Joysticks'Img);
    for i in 0 .. Number_Of_Joysticks -1 loop
      Game.Player(Integer(i)).Game_Obj_Ptr.Joy_Ptr := SDL.Joystick.JoystickOpen(i);
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
    for i in Game.Player'range loop
      Game.Player(i).Game_Obj_Ptr.Update(Game.Screen);
    end loop;
    Log("Game.Update","Stop");
  end Update;
  
  procedure Render(Game : in out Game_Type) is
  begin 
    Log("Game.Render","Start");
    
    SDL.Video.FillRect(Game.Screen, null, Game.Background);
    for i in Game.Player'range loop
      Game.Player(i).Game_Obj_Ptr.Update(Game.Screen);
      --  Erase all sprites if necessary
      SDL.Video.BlitSurface (Game.Sprite, Game.Player(i).Game_Obj_Ptr.From_Area, Game.Screen, Game.Player(i).Game_Obj_Ptr.To_Area);
    end loop;  
    --  Update the screen
    SDL.Video.Flip (Game.Screen);
    
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
           declare
             -- player 0 -> w,a,s,z
             -- player 1 -> up,left,rigt,down
             Name_Of_Key : String := C.Strings.Value(SDL.Keyboard.GetKeyName(Event.key.keysym.sym));
             Player_Idx : Integer := -1;
           begin
             Log("Game.Handle_Events"," Key down: '" & Name_Of_Key & "'");
             if    Name_Of_Key = "up" then
               Player_Idx := 1; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := Up;
             elsif Name_Of_Key = "right" then
               Player_Idx := 1; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := Right;
             elsif Name_Of_Key = "down" then
               Player_Idx := 1; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := Down;
             elsif Name_Of_Key = "left" then
               Player_Idx := 1; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := Left;
             elsif Name_Of_Key = "w" then
               Player_Idx := 0; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := Up;
             elsif Name_Of_Key = "s" then
               Player_Idx := 0; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := Right;
             elsif Name_Of_Key = "z" then
               Player_Idx := 0; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := Down;
             elsif Name_Of_Key = "a" then
               Player_Idx := 0; 
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := left;
             end if;             
           end;
           
           
         when SDL.Events.KEYUP =>
           declare
             -- player 0 -> w,a,s,z
             -- player 1 -> up,left,rigt,down
             Name_Of_Key : String := C.Strings.Value(SDL.Keyboard.GetKeyName(Event.key.keysym.sym));
             Player_Idx : Integer := -1;
           begin
             Log("Game.Handle_Events"," Key up: '" & Name_Of_Key & "'");
             if    Name_Of_Key = "up" then
               Player_Idx := 1; 
             elsif Name_Of_Key = "right" then
               Player_Idx := 1; 
             elsif Name_Of_Key = "down" then
               Player_Idx := 1; 
             elsif Name_Of_Key = "left" then
               Player_Idx := 1; 
             elsif Name_Of_Key = "w" then
               Player_Idx := 0; 
             elsif Name_Of_Key = "s" then
               Player_Idx := 0; 
             elsif Name_Of_Key = "z" then
               Player_Idx := 0; 
             elsif Name_Of_Key = "a" then
               Player_Idx := 0; 
             elsif Name_Of_Key = "escape" then
               Game.Is_Running := False;   -- quit on escape
             end if;             
             if Player_Idx > -1 then
               Game.Player(Player_Idx).Game_Obj_Ptr.Last_Heading := 
                 Game.Player(Player_Idx).Game_Obj_Ptr.Heading;
               Game.Player(Player_Idx).Game_Obj_Ptr.Heading := None;
             end if;
           end;
           
           
         when SDL.Events.QUIT =>
           Log("Game.Handle_Events"," QUIT Event.The_Type " & Event.The_Type'Img );
           Game.Is_Running := False;
         when SDL.Events.MOUSEBUTTONDOWN =>         
           Log("Game.Handle_Events"," MOUSEBUTTONDOWN x,y:" & Event.Button.X'Img & "," & Event.Button.Y'Img);
--           Log("Game.Handle_Events"," (Game.Velocities(0) x,y:" & Game.Positions(0).X'Img & "," & Game.Positions(0).Y'Img);
--           if Event.Button.X > Uint16(Game.Positions(0).X) then 
--             Game.Velocities (0).X := 1;
--           elsif Event.Button.X < Uint16(Game.Positions(0).X) then 
--             Game.Velocities (0).X := -1;
--           end if;
         
--           if Event.Button.Y > Uint16(Game.Positions(0).Y) then 
--             Game.Velocities (0).Y := 1;
--           elsif Event.Button.Y < Uint16(Game.Positions(0).Y) then 
--             Game.Velocities (0).Y := -1;
--           end if;

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
                                                                       
           case Event.Jhat.Value is
              when SDL.Joystick.HAT_CENTERED => 
                Game.Player(Integer(Event.Jhat.Which)).Game_Obj_Ptr.Last_Heading := 
                  Game.Player(Integer(Event.Jhat.Which)).Game_Obj_Ptr.Heading;
                Game.Player(Integer(Event.Jhat.Which)).Game_Obj_Ptr.Heading := None;
              when SDL.Joystick.HAT_UP       => 
                Game.Player(Integer(Event.Jhat.Which)).Game_Obj_Ptr.Heading := Up;
              when SDL.Joystick.HAT_RIGHT    => 
                Game.Player(Integer(Event.Jhat.Which)).Game_Obj_Ptr.Heading := Right;
              when SDL.Joystick.HAT_DOWN     => 
                Game.Player(Integer(Event.Jhat.Which)).Game_Obj_Ptr.Heading := Down;
              when SDL.Joystick.HAT_LEFT     => 
                Game.Player(Integer(Event.Jhat.Which)).Game_Obj_Ptr.Heading := Left;
              when others                    => 
                null;
           end case;                                                                        
           
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
  
  overriding procedure Init(O   : in out SDL_Game_Object; 
                            Sprite : SDL.Video.Surface_Ptr; 
                            Initial_X,Initial_Y : C.Int;
                            Moving_Sprite_Coordinates : Moving_Sprite_Coordinates_Array;
                            Motion_Sprite_Coordinates : Motion_Sprite_Coordinates_Array) is 
  begin

    O.Position.X := Sint16(Initial_X);
    O.Position.Y := Sint16(Initial_Y);
    O.Position.W := Uint16 (Sprite.W);
    O.Position.H := Uint16 (Sprite.H);
    O.Velocity.X := 0;
    O.Velocity.Y := 0;
    O.Moving_Sprite_Coordinates := Moving_Sprite_Coordinates;
    O.Motion_Sprite_Coordinates := Motion_Sprite_Coordinates;
  end Init;
  
  overriding procedure Draw(O   : in out SDL_Game_Object) is
  begin
    null;
  end Draw;
  
  overriding procedure Update(O : in out SDL_Game_Object; Screen : SDL.Video.Surface_Ptr) is
   Image_Index : Heading_Type := O.Heading;
   Motion_Idx : Integer := 0;
  begin
    case O.Heading is
      when None  =>
        O.Velocity.Y := 0; 
        O.Velocity.X := 0;
        Image_Index  := O.Last_Heading; -- use the old picture
      when Up    => 
        O.Velocity.Y := -1; 
        O.Velocity.X := 0;
      when Right =>
        O.Velocity.Y := 0; 
        O.Velocity.X := 1;
      when Down  =>
        O.Velocity.Y := 1; 
        O.Velocity.X := 0;
      when Left  =>
        O.Velocity.Y := 0; 
        O.Velocity.X := -1;
    end case;
    
    O.Position.X := O.Position.X + O.Velocity.X;
    O.Position.Y := O.Position.Y + O.Velocity.Y;
    if (O.Position.X < 0) or (C.Int (O.Position.X+32) >= Screen.W) or
       (O.Position.Y < 0) or (C.Int (O.Position.Y+32) >= Screen.H) then
       
       O.Position.X := O.Position.X - O.Velocity.X; -- make it stay at border
       O.Position.Y := O.Position.Y - O.Velocity.Y; -- by undo this move
       
      O.From_Area :=(3 * 33, 1 * 33,32,32);  -- explosion
    else
      O.To_Area := O.Position;
      
       Motion_Idx := Integer((SDL.Timer.GetTicks mod 8) +1);

      
      O.From_Area := (X => O.Motion_Sprite_Coordinates(Image_Index, Motion_Idx).X * 33,
                      Y => O.Motion_Sprite_Coordinates(Image_Index, Motion_Idx).Y * 33,
                      W => 32,
                      H => 32);     
--      O.From_Area := (X => O.Moving_Sprite_Coordinates(Image_Index).X * 33,
--                      Y => O.Moving_Sprite_Coordinates(Image_Index).Y * 33,
--                      W => 32,
--                      H => 32);     
    end if;
  end Update;  
  
  overriding procedure Clean(O  : in out SDL_Game_Object) is
  begin
    null;
  end Clean;
  -------------------- Stop SDL_Game_Object ---------------------------
   
  
end Obj;