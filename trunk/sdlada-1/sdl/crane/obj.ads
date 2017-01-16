with SDL.Types; use SDL.Types;
with SDL.Video;
with SDL.Joystick;

with Interfaces.C;


package Obj is
   package C renames Interfaces.C;

  type Sprite_Coordinates_Record is record
    X,Y : Sint16 := 0;
  end record;
  type Heading_Type is (None,Up,Right,Down,Left);

  type Motion_Sprite_Coordinates_Array is array (Heading_Type'range, 1..8) of Sprite_Coordinates_Record;
  Green_Tank : Motion_Sprite_Coordinates_Array := (
    None  => ((11,1),(11,1),(11,1),(11,1),(11,1),(11,1),(11,1),(11,1)),
    Up    => ((15,1),(22,1),(21,1),(20,1),(19,1),(18,1),(17,1),(16,1)),
    Right => ((4,20),(16,9),(16,8),(16,7),(16,6),(16,5),(16,4),(16,3)),
    Down  => ((15,6),(22,2),(21,2),(20,2),(19,2),(18,2),(17,2),(16,2)),
    Left  => ((18,3),(17,9),(17,8),(17,7),(17,6),(17,5),(17,4),(17,3))
  );
  Blue_Tank : Motion_Sprite_Coordinates_Array := (
    None  => ((12,1),(12,1),(12,1),(12,1),(12,1),(12,1),(12,1),(12,1)),
    Up    => ((18,5),(22,10),(21,10),(20,10),(19,10),(18,10),(17,10),(16,10)),
    Right => ((20,9),(22,9),(22,8),(22,7),(22,6),(22,5),(22,4),(22,3)),
    Down  => ((19,7),(22,11),(21,11),(20,11),(19,11),(18,11),(17,11),(16,11)),
    Left  => ((18,8),(21,9),(21,8),(21,7),(21,6),(21,5),(21,4),(21,3))
  );




  ---------------- The Game Object------------------------------------
  type Moving_Sprite_Coordinates_Array is array (Heading_Type'range) of Sprite_Coordinates_Record;

  type Game_Object is abstract tagged record
    X              : C.Int := 0;
    Y              : C.Int := 0;
    W              : C.Int := 0;
    H              : C.Int := 0;
    To_Area,
    From_Area      : SDL.Video.Rect;
    Heading        : Heading_Type := None;
    Last_Heading   : Heading_Type := None;
    Moving_Sprite_Coordinates : Moving_Sprite_Coordinates_Array;
    Motion_Sprite_Coordinates : Motion_Sprite_Coordinates_Array ;

--    X_Frame        : SInt16 := 0;
    Position,
    Velocity       : SDL.Video.Rect;
    Joy_Ptr        : SDL.Joystick.Joystick_Ptr := SDL.Joystick.Null_Joystick_Ptr;
  end record;
  procedure Init(O   : in out Game_Object;
                 Sprite : SDL.Video.Surface_Ptr;
                 Initial_X,Initial_Y : C.Int;
                 Moving_Sprite_Coordinates : Moving_Sprite_Coordinates_Array;
                 Motion_Sprite_Coordinates : Motion_Sprite_Coordinates_Array
                 ) is abstract;

  procedure Draw(O   : in out Game_Object) is abstract;
  procedure Update(O : in out Game_Object;  Screen : SDL.Video.Surface_Ptr) is abstract;
  procedure Clean(O  : in out Game_Object) is abstract;

  type Game_Object_Access_Type is access all Game_Object'class;

  ----------------SDL_Game Object-----------------------------------------
  type SDL_Game_Object is new Game_Object with null record;

  overriding procedure Init(O   : in out SDL_Game_Object;
                            Sprite : SDL.Video.Surface_Ptr;
                            Initial_X,Initial_Y : C.Int;
                            Moving_Sprite_Coordinates : Moving_Sprite_Coordinates_Array;
                            Motion_Sprite_Coordinates : Motion_Sprite_Coordinates_Array);
  overriding procedure Draw(O   : in out SDL_Game_Object);
  overriding procedure Update(O : in out SDL_Game_Object; Screen : SDL.Video.Surface_Ptr);
  overriding procedure Clean(O  : in out SDL_Game_Object);


  ---------------- The Game------------------------------------

--  type Joystick_Array is array(0 ..3) of SDL.Joystick.Joystick_Ptr;

  type Player_Record is record
    Game_Obj_Ptr : Game_Object_Access_Type;
  end record;

  type Player_Array is array(0 .. 1) of Player_Record;

  -------------------------------------------------------



  type Game_Type is tagged record
    Is_Running : Boolean := False;
    Background : SDL.Types.Uint32;
    Sprite     : SDL.Video.Surface_Ptr;
    Screen     : SDL.Video.Surface_Ptr;
    Player     : Player_Array ;
  end record;

  function Running(Game : Game_Type) return Boolean ;
  procedure Load_Sprite (Game : in out Game_Type; File : in String; Result : out Boolean);
  procedure Init(Game : in out Game_Type; Players : Player_Array);
  procedure Update(Game : in out Game_Type);
  procedure Render(Game : in out Game_Type);
  procedure Handle_Events(Game : in out Game_Type);


  -- list of game objects ptr
  -- load_spirte + renders -> sdl obj
  -- render object (texture manager)??

  ---------------------------------------------------------

end Obj;