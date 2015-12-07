-- TTF Example - Headerphile.blogspot.no
-- Font used : Sketchy https:--github.com/olevegard/Headerphile/raw/master/SDL2_Tutorial/Tutorial_10/sketchy.ttf
-- Font license : https:--github.com/olevegard/Headerphile/raw/master/SDL2_Tutorial/Tutorial_10/sketchy-license.txt
-- Compilation : g++ text_test.cpp -std=c++1y -lSDL2 -lSDL2_ttf -o fonttest && ./fonttest
--
-- #include <SDL2/SDL.h>
-- #include <SDL2/SDL_ttf.h>
-- #include <iostream>

with Ada.Command_Line;
with Ada.Exceptions;
with SDL.Log;
with Stacktrace;


with SDL.TTF;
with SDL.Video.Palettes;
with SDL.Video.Textures;
with SDL.Video.Windows;
with SDL.Video.Renderers;
with SDL.Video.Surfaces;

with SDL.Video.Textures.Makers;
with SDL.Video.Renderers.Makers;
with SDL.Video.Windows.Makers;

with SDL.Video.Rectangles;
with SDL.Video.Surfaces.Fonts;


with Interfaces.C;


with Text_Io; use Text_Io;


procedure AdaFontTest is
  use type Interfaces.C.Int;

-- Setup
  function InitEverything return Boolean;
  function InitSDL return Boolean;
  function CreateWindow return Boolean;
  function CreateRenderer return Boolean;
  procedure SetupRenderer;

  -- Our new function for setting uo SDL_TTF
  function SetupTTF( fontname : string ) return Boolean;
  Font : Sdl.TTF.Font;

  procedure SurfaceToTexture (T : in out SDL.Video.Textures.Texture;
                              S : in     SDL.Video.Surfaces.Surface ) ;
  --SDL_Texture* SurfaceToTexture( SDL_Surface* surf );
  
  procedure CreateTextTextures ;

  -- Update ( happens every frame )
  procedure Render;
  procedure RunGame;
  
  -- Stuff for text rendering
  --SDL_Color textColor = { 255, 255, 255, 255 }; -- white
  --SDL_Color backgroundColor = { 0, 0, 0, 255 }; -- black
  textColor       : SDL.Video.Palettes.Colour := (Red => 255, Green => 255, Blue => 255, Alpha => 255);-- white
  backgroundColor : SDL.Video.Palettes.Colour := (Red =>   0, Green =>   0, Blue =>   0, Alpha => 255);-- black
  Red             : SDL.Video.Palettes.Colour := (Red => 255, Green =>   0, Blue =>   0, Alpha => 255);-- red


  solidTexture   : SDL.Video.Textures.Texture;
  blendedTexture : SDL.Video.Textures.Texture;
  shadedTexture  : SDL.Video.Textures.Texture;
  
  
  solidRect : SDL.Video.Rectangles.Rectangle := (0,0,0,0);
  blendedRect : SDL.Video.Rectangles.Rectangle := (0,0,0,0);
  shadedRect : SDL.Video.Rectangles.Rectangle := (0,0,0,0);
  
  windowRect : SDL.Video.Rectangles.Rectangle := (900, 300, 400, 400 );

  Window           : SDL.Video.Windows.Window;
  Renderer         : SDL.Video.Renderers.Renderer;


  
  
  procedure RunGame is
  begin
    Render;
    Put_Line( "runs for 5 secs");
    delay 5.0;
  end RunGame;

  procedure Render is
  begin
    -- Clear the window and make it all red
    Renderer.Clear;
    -- Render our text objects ( like normal )
    Renderer.Copy(Copy_From => solidTexture , To => solidRect);
    Renderer.Copy(Copy_From => blendedTexture , To => blendedRect);
    Renderer.Copy(Copy_From => shadedTexture , To => shadedRect);
    
    -- Render the changes above
    Renderer.Present;
  end Render;
-- Initialization ++
-- ==================================================================
  function SetupTTF( Fontname : String ) return Boolean is
  begin
    -- SDL2_TTF needs to be initialized just like SDL2
    SDL.TTF.Init;
    -- Load our fonts, with a huge size
    Font.Open( Fontname, 90 );
    -- Error check
    return true;
  end SetupTTf;

  procedure CreateTextTextures is
     Solid   : SDL.Video.Surfaces.Surface;
     Blended : SDL.Video.Surfaces.Surface;
     Shaded  : SDL.Video.Surfaces.Surface;
     F       : Natural          := 0;
     A       : Interfaces.C.Int := 0;
  begin

    SDL.Video.Surfaces.Fonts.Create(Surface  => Solid,
                                    Font     => Font,
                                    Color    => textColor, 
                                    Text     => "solid");
    SurfaceToTexture(T => solidTexture, S => solid);
    
    solidTexture.Query(Format => F,
                       Acess  => A,
                       W      => solidRect.Width,
                       H      => solidRect.Height);
    solidRect.x := 0;
    solidRect.y := 0;


    SDL.Video.Surfaces.Fonts.Create(Surface  => Blended,
                                    Font     => Font,
                                    Color    => textColor, 
                                    Text     => "blended");
    SurfaceToTexture(T => blendedTexture, S => blended);
    
    blendedTexture.Query(Format => F,
                       Acess  => A,
                       W      => blendedRect.Width,
                       H      => blendedRect.Height);
    blendedRect.x := 0;
    blendedRect.y := solidRect.y + solidRect.Height +  20;

  
    SDL.Video.Surfaces.Fonts.Create(Surface  => Shaded,
                                    Font     => Font,
                                    Color    => textColor, 
                                    Text     => "shaded");
    SurfaceToTexture(T => shadedTexture, S=> Shaded);
    
    shadedTexture.Query(Format => F,
                       Acess  => A,
                       W      => shadedRect.Width,
                       H      => shadedRect.Height);
    shadedRect.x := 0;
    shadedRect.y := blendedRect.y + blendedRect.Height + 20;
  end CreateTextTextures;

  procedure SurfaceToTexture (T : in out SDL.Video.Textures.Texture;
                              S : in     SDL.Video.Surfaces.Surface ) is
  begin
    SDL.Video.Textures.Makers.Create(Tex      => T,
                                     Renderer => Renderer,
                                     Surface  => S);
  end SurfaceToTexture;  



  function InitEverything return Boolean is
  begin
    if not InitSDL        then return false; end if;
    if not CreateWindow   then return false; end if;
    if not CreateRenderer then return false; end if;
    SetupRenderer;
    if not SetupTTF( "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf" ) then return false; end if;
    CreateTextTextures;
    return true;
  end InitEverything;

  function InitSDL return Boolean is
  begin
    if not SDL.Initialise then
      Put_Line ("initilise failed");
      return false;
    end if;  
    return true;
  end InitSDL;

  function CreateWindow return Boolean is
  begin
   SDL.Video.Windows.Makers.Create (Win    => Window,
                                    Title  => "Rektanglar test)",
                                    X      => Integer(windowRect.x),
                                    Y      => Integer(windowRect.y),
                                    Width  => Integer(windowRect.Width),
                                    Height => Integer(windowRect.Height),
                                    Flags  => 0);
    
    return true;
  end CreateWindow;

  function CreateRenderer return Boolean  is 
  begin
    SDL.Video.Renderers.Makers.Create(Renderer,Window,SDL.Video.Renderers.Accelerated);
    return true;
  end CreateRenderer;

  procedure SetupRenderer is
    Window_Size : SDL.Video.Rectangles.Size := (windowRect.Width, windowRect.Height);
  begin
    -- Set size of renderer to the same as window
    Renderer.Set_Logical_Size(Window_Size);
    -- Set color of renderer to red
    Renderer.Set_Draw_Colour(Red);
 end SetupRenderer; 

begin
  SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);
  if not InitEverything
     then return ;
  end if;
  RunGame;
  Font.Close;
  Sdl.TTF.Quit;
  
exception
  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
       SDL.Log.Put_Debug(Last_Exception_Name);
       SDL.Log.Put_Debug("Message : " & Last_Exception_Messsage);
       SDL.Log.Put_Debug(Last_Exception_Info);
       SDL.Log.Put_Debug("addr2line" & " --functions --basenames --exe=" &
           Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;
  
end AdaFontTest;
