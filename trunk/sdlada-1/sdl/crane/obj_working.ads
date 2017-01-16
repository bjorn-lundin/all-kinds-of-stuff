with SDL.Types; use SDL.Types;
with SDL.Video;
with SDL.Joystick;

with Interfaces.C;


package Obj is
   package C renames Interfaces.C;


  type Rects_Array_Access is access SDL.Video.Rects_Array;
  type Joystick_Array is array(0 ..3) of SDL.Joystick.Joystick_Ptr;
  -------------------------------------------------------
  type Game_Type is tagged record
    Is_Running : Boolean := False;
    Background : SDL.Types.Uint32;    
    Sprite     : SDL.Video.Surface_Ptr;
    Screen     : SDL.Video.Surface_Ptr;
    Sprite_Rects, 
    Positions, 
    Velocities : Rects_Array_Access;
    X_Frame    : SInt16 := 0;
    Jsticks    : Joystick_Array := (others => SDL.Joystick.Null_Joystick_Ptr);
  end record;
    
  function Running(Game : Game_Type) return Boolean ;
  procedure Load_Sprite (Game : in out Game_Type; File : in String; Result : out Boolean);
  procedure Init(Game : in out Game_Type);
  procedure Update(Game : in out Game_Type);
  procedure Render(Game : in out Game_Type);
  procedure Handle_Events(Game : in out Game_Type);
  
  ---------------------------------------------------------
  
  type Game_Object is abstract tagged record
    X              : C.Int := 0;
    Y              : C.Int := 0;
    W              : C.Int := 0;
    H              : C.Int := 0;
    Joystick_Index : Integer := -1; 
  end record; 
  procedure Init(O   : in out Game_Object; Joystick_Index : Integer) is abstract;
  procedure Draw(O   : in out Game_Object) is abstract;
  procedure Update(O : in out Game_Object) is abstract;
  procedure Clean(O  : in out Game_Object) is abstract;

  ---------------------------------------------------------
  type SDL_Game_Object is new Game_Object with null record;
  overriding procedure Init(O   : in out SDL_Game_Object; Joystick_Index : Integer);
  overriding procedure Draw(O   : in out SDL_Game_Object);
  overriding procedure Update(O : in out SDL_Game_Object);
  overriding procedure Clean(O  : in out SDL_Game_Object);
  
  
end Obj;