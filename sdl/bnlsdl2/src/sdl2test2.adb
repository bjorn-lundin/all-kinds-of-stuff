with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Text_Io; use Text_Io;
with Stacktrace;


procedure SDL2Test2 is
  use type Interfaces.C.Int;
  package C renames Interfaces.C;
  type Renderer_Flags is mod 2 ** 32 with Convention => C;    
  type Window_Flags is mod 2 ** 32 with Convention => C;     
    
  type Size is record
       Width  : C.int;
       Height : C.int;
  end record with Convention => C;

  type Rectangle is record
       X      : C.int;
       Y      : C.int;
       Width  : C.int;
       Height : C.int;
  end record with Convention => C;
      
  type Rectangle_Pointer is access all Rectangle with Convention => C;     
      
  type Colour_Component is range 0 .. 255 with
    Size       => 8,
    Convention => C;

  type Colour is record
       Red   : Colour_Component;
       Green : Colour_Component;
       Blue  : Colour_Component;
       Alpha : Colour_Component;
  end record with
      Convention => C,
      Size       => Colour_Component'Size * 4;

  for Colour use record
       Red   at 0 range  0 ..  7;
       Green at 0 range  8 .. 15;
       Blue  at 0 range 16 .. 23;
       Alpha at 0 range 24 .. 31;
  end record;

  type RGB_Colour is record
       Red   : Colour_Component;
       Green : Colour_Component;
       Blue  : Colour_Component;
  end record;

  Bad : exception;
      
  type Font_Pointer     is access all Interfaces.C.Int;
  type Surface_Pointer  is access all Interfaces.C.Int;
  type Texture_Pointer  is access all Interfaces.C.Int;
  type Renderer_Pointer is access all Interfaces.C.Int;
  type Window_Pointer   is access all Interfaces.C.Int;
  
  type Init_Flags is mod 2 ** 32 with Convention => C;
  
 
  Enable_Timer           : constant Init_Flags := 16#0000_0001#;
  Enable_Audio           : constant Init_Flags := 16#0000_0010#;
  Enable_Screen          : constant Init_Flags := 16#0000_0020#;
  Enable_Joystick        : constant Init_Flags := 16#0000_0200#;
  Enable_Haptic          : constant Init_Flags := 16#0000_1000#;
  Enable_Game_Controller : constant Init_Flags := 16#0000_2000#;
  Enable_Events          : constant Init_Flags := 16#0000_4000#;
  Enable_No_Parachute    : constant Init_Flags := 16#0010_0000#;
  Enable_Everything      : constant Init_Flags :=
     Enable_Timer or Enable_Audio or Enable_Screen or Enable_Joystick or Enable_Haptic or
     Enable_Game_Controller or Enable_Events or Enable_No_Parachute;  
  -- sdl
    function SDL_Init (Flags : in Init_Flags := Enable_Everything) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_Init";
  
  
  -- start font
    function TTF_Init return C.int;
    pragma Import(C,TTF_Init,"TTF_Init");
    
   --Font *TTF_OpenFont( const char *file, int ptsize)
    function TTF_OpenFont(Filename : Chars_Ptr; 
                          Point_Size : c.Int ) return Font_Pointer ;
    pragma Import (C, TTF_OpenFont, "TTF_OpenFont");
    
    procedure TTF_Quit ;
    pragma Import(C,TTF_Quit,"TTF_Quit");
  
  -- start windows
  
    function SDL_Create
      (Title      : Chars_Ptr;
       X, Y, W, H : in C.int;
       F          : in Window_Flags) return Window_Pointer ;
    pragma Import(C, SDL_Create, "SDL_CreateWindow");

    -- start renderer           
    function SDL_Render_Set_Logical_Size (R : in Renderer_Pointer;
                                          W : C.int;
                                          H : C.int ) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetLogicalSize";
      
    function SDL_Render_Clear (R : in Renderer_Pointer) return C.int with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_RenderClear";
      
    function SDL_Render_Copy
      (R         : in Renderer_Pointer;
       T         : in Texture_Pointer;
       Src       : in Rectangle_Pointer;
       Dest      : in Rectangle_Pointer) return C.int with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_RenderCopy";
     
    procedure SDL_Render_Present (R : in Renderer_Pointer) with
      Import        => True,
      Convention    => C,
      External_Name => "SDL_RenderPresent";
      
    function TTF_RenderText_Blended(Font_Ptr    : Font_Pointer ;  
                                    Text        : Chars_Ptr; 
                                    Fore_Ground : RGB_Colour) return Surface_Pointer;
    pragma Import(C,TTF_RenderText_Blended, "TTF_RenderText_Blended");    

    function TTF_RenderText_Solid(Font_Ptr      : Font_Pointer ;  
                                    Text        : Chars_Ptr; 
                                    Fore_Ground : RGB_Colour) return Surface_Pointer;
    pragma Import(C,TTF_RenderText_Solid, "TTF_RenderText_Solid");   
    
    function TTF_RenderText_Shaded(Font_Ptr     : Font_Pointer ;  
                                    Text        : Chars_Ptr; 
                                    Fore_Ground : RGB_Colour;
                                    Back_Ground : RGB_Colour) return Surface_Pointer;
    pragma Import(C,TTF_RenderText_Shaded, "TTF_RenderText_Shaded");    


    
    function SDL_Create_Texture_From_Surface (R : in  Renderer_Pointer;
                                              S : in  Surface_Pointer)
                                              return Texture_Pointer ;
    pragma Import (C, SDL_Create_Texture_From_Surface, "SDL_CreateTextureFromSurface");
       
    procedure SDL_Free_Surface(S : in Surface_Pointer);
    pragma Import (C, SDL_Free_Surface, "SDL_FreeSurface");
    
    function SDL_Query_Texture (T       : Texture_Pointer;
                                F       : access Interfaces.C.Long ;
                                A, W, H : access Interfaces.C.Int) return C.Int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_QueryTexture";
      
    function SDL_Create_Renderer (W     : in Window_Pointer; 
                                  Index : in C.int; 
                                  Flags : in Renderer_Flags)
                                    return Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRenderer";
    function SDL_Set_Render_Draw_Color
        (R                       : in Renderer_Pointer;
         Red, Green, Blue, Alpha : in C.Int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderDrawColor";
      
   function Get_Error return String is
      function SDL_Get_Error return Chars_Ptr with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetError";

      C_Str : Chars_Ptr := SDL_Get_Error;
   begin
      return C.Strings.Value (C_Str);
   end Get_Error;



   
   Surface_Ptr : Surface_Pointer := null;
   C_Text : Chars_Ptr := New_String("Yes, Finally!");
  
-- Setup
--  Blue            :  Colour := (Red =>   0, Green =>   0, Blue => 255, Alpha => 255);
  Blue  :  RGB_Colour := (Red =>   0, Green =>   0, Blue => 255);
  Green :  RGB_Colour := (Red =>   0, Green => 255, Blue =>   0);
  
  Font_Ptr           :  Font_Pointer := null;
  solidTexture_Ptr   :  Texture_Pointer := null;
  blendedTexture_Ptr :  Texture_Pointer := null;
  shadedTexture_Ptr  :  Texture_Pointer := null;
   
  solidRect   :  aliased Rectangle := (0,0,0,0);
  blendedRect :  aliased Rectangle := (0,0,0,0);
  shadedRect  :  aliased Rectangle := (0,0,0,0);
  
  windowRect : Rectangle := (1, 1, 1200, 700 );

  Window_Ptr       :  Window_Pointer   := null;
  Renderer_Ptr     :  Renderer_Pointer := null;

  

  procedure Render is
     Result : C.int := 0;
  begin
    -- Clear the window and make it all red
    Result := SDL_Render_Clear(Renderer_Ptr);
    if Result /= 0 then
       raise Bad with Get_Error;
    end if;
    
--    Result  := SDL_Render_Copy (Renderer_Ptr,
--                                solidTexture_Ptr,
--                                null,
--                                solidRect'access);
--    if Result /= 0 then
--       raise Bad with Get_Error;
--    end if;
--    Put_Line("solidRect   x y w h" & solidRect.X'Img & solidRect.Y'img & solidRect.Width'img & solidRect.Height'img);
    
    Result  := SDL_Render_Copy (Renderer_Ptr,
                                blendedTexture_Ptr,
                                null,
                                blendedRect'access);
    if Result /= 0 then
       raise Bad with Get_Error;
    end if;
    Put_Line("blendedRect x y w h" & blendedRect.X'Img & blendedRect.Y'img & blendedRect.Width'img & blendedRect.Height'img);
--    
--    Result  := SDL_Render_Copy (Renderer_Ptr,
--                                shadedTexture_Ptr,
--                                null,
--                                shadedRect'access);
--    if Result /= 0 then
--       raise Bad with Get_Error;
--    end if;
    
--    Renderer.Set_Draw_Colour(Blue);
--    Renderer.Draw(solidRect);
    
    
    -- Render the changes above
    SDL_Render_Present(Renderer_Ptr);
  end Render;
  ------------------------------------------------
  procedure RunGame is
  begin
    Render;
    Put_Line( "runs for 5 secs");
    delay 15.0;
  end RunGame;
  
-- Initialization ++
-- ==================================================================
  procedure SetupTTF is
    F_init: C.Int := 0;
    C_Filename : Chars_Ptr := New_String( "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf");
  begin
    F_Init := TTF_Init;
    if F_Init /= 0 then
      Put_Line ("font initilise failed");
      raise Bad;
    end if;  
    Font_Ptr := TTF_OpenFont(C_Filename,90 ) ;
    if Font_Ptr = null then
      Put_Line ("font2 initilise failed");
      raise Bad;
    end if;  
    Free(C_Filename);
  end SetupTTF;
  -----------------------------------------------------
  procedure CreateTextTextures is
    Local_Format : aliased Interfaces.C.Long  := 0;
    Local_Acess  : aliased Interfaces.C.Int := 0;
    Local_W      : aliased Interfaces.C.Int := 0;
    Local_H      : aliased Interfaces.C.Int := 0;
    Result : C.Int := 0;
  begin
    Surface_Ptr := TTF_RenderText_Solid(Font_Ptr    => Font_Ptr,  
                                        Text        => C_Text,
                                        Fore_Ground => Blue); 
    solidTexture_Ptr := SDL_Create_Texture_From_Surface(Renderer_Ptr, Surface_Ptr);
    SDL_Free_Surface(Surface_Ptr);    
       
    Result := SDL_Query_Texture (solidTexture_Ptr, 
                                 Local_Format'access, 
                                 Local_Acess'access, 
                                 Local_W'access, 
                                 Local_H'access);
       
    solidRect.x := 0;
    solidRect.y := 0;
    solidRect.Width := Local_W;
    solidRect.Height := Local_H;
    -- -- -- -- -- -- -- -- 

    Surface_Ptr := TTF_RenderText_Blended(Font_Ptr    => Font_Ptr,  
                                      Text        => C_Text,
                                      Fore_Ground => Blue); 
    blendedTexture_Ptr := SDL_Create_Texture_From_Surface(Renderer_Ptr, Surface_Ptr);
    SDL_Free_Surface(Surface_Ptr);    
       
    Result := SDL_Query_Texture (blendedTexture_Ptr, 
                                 Local_Format'access, 
                                 Local_Acess'access, 
                                 Local_W'access, 
                                 Local_H'access);
       
    blendedRect.Width := Local_W;
    blendedRect.Height := Local_H;

    blendedRect.x := 0;
    blendedRect.y := solidRect.y + solidRect.Height +  20;
    -- -- -- -- -- -- -- -- 

    Surface_Ptr := TTF_RenderText_Shaded(Font_Ptr    => Font_Ptr,  
                                         Text        => C_Text,
                                         Fore_Ground => Blue,
                                         Back_Ground => Green); 
    shadedTexture_Ptr := SDL_Create_Texture_From_Surface(Renderer_Ptr, Surface_Ptr);
    SDL_Free_Surface(Surface_Ptr);    
       
    Result := SDL_Query_Texture (shadedTexture_Ptr, 
                                 Local_Format'access, 
                                 Local_Acess'access, 
                                 Local_W'access, 
                                 Local_H'access);
       
    shadedRect.Width := Local_W;
    shadedRect.Height := Local_H;
    shadedRect.x := 0;
    shadedRect.y := blendedRect.y + blendedRect.Height + 20;    
    Free(C_Text);
  end CreateTextTextures;
  -----------------------------------------------------


---------------------------------------------
  procedure InitSDL is
   
  begin
    if SDL_Init /= 0 then
      Put_Line ("initilise failed");
      return ;
    end if;  
  end InitSDL;
-------------------------------------------
  procedure CreateWindow is
      C_Title_Str : Chars_Ptr := C.Strings.New_String ("Rektanglar test thin");
   begin    
      Window_Ptr := SDL_Create (C_Title_Str,
                                windowRect.X, 
                                windowRect.Y, 
                                windowRect.Width, 
                                windowRect.Height,
                                0);
                                
      C.Strings.Free (C_Title_Str);
      if Window_Ptr = null then
         raise Bad with Get_Error;
      end if;
  end CreateWindow;
-----------------------------------------------------
  procedure CreateRenderer  is 
  begin
     Renderer_Ptr := SDL_Create_Renderer(Window_Ptr, -1, 16#0000_0002#);
     if Window_Ptr = null then
         raise Bad with Get_Error;
      end if;
  end CreateRenderer;
-----------------------------------------------------------
  procedure SetupRenderer is
    Window_Size : Size := (windowRect.Width, windowRect.Height);
    We : aliased Interfaces.C.Int := Window_Size.Width;
    He : aliased Interfaces.C.Int := Window_Size.Height;
    Result : C.int :=0;      
  begin
    -- Set size of renderer to the same as window
      Result := SDL_Render_Set_Logical_Size (Renderer_Ptr, We, He);
      if Result /= 0 then
         raise Bad with Get_Error;
      end if;
      
      Result := SDL_Set_Render_Draw_Color (Renderer_Ptr, 255, 0, 0, 255);
      if Result /= 0 then
         raise Bad with Get_Error;
      end if;
 end SetupRenderer; 

 
  procedure InitEverything  is
  begin
    InitSDL       ;
    CreateWindow  ;
    CreateRenderer;
    SetupRenderer;
    SetupTTF;
    CreateTextTextures;
  end InitEverything;
 
 
 
begin
  InitEverything;
  RunGame;
  TTF_Quit;
  
exception
  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
        Put_Line(Last_Exception_Name);
        Put_Line("Message : " & Last_Exception_Messsage);
        Put_Line(Last_Exception_Info);
        Put_Line("addr2line" & " --functions --basenames --exe=" &
           Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;
  
end SDL2Test2;
