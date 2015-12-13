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



procedure AdaFontTest is
  use type Interfaces.C.Int;

  Font : Sdl.TTF.Font;

  -- Stuff for text rendering
  --SDL_Color textColor = { 255, 255, 255, 255 }; -- white
  --SDL_Color backgroundColor = { 0, 0, 0, 255 }; -- black
--  backgroundColor : SDL.Video.Palettes.Colour := (Red =>   0, Green =>   0, Blue =>   0, Alpha => 255);-- black
  Red             : SDL.Video.Palettes.Colour := (Red => 255, Green =>   0, Blue =>   0, Alpha => 255);-- red
  Green           : SDL.Video.Palettes.Colour := (Red =>   0, Green => 255, Blue =>   0, Alpha => 255);-- Green
  Blue            : SDL.Video.Palettes.Colour := (Red =>   0, Green =>   0, Blue => 255, Alpha => 255);-- blue
  White           : SDL.Video.Palettes.Colour := (Red => 255, Green => 255, Blue => 255, Alpha => 255);-- White
--  Black           : SDL.Video.Palettes.Colour := (Red =>   0, Green =>   0, Blue =>   0, Alpha => 255);-- Black


  solidTexture   : SDL.Video.Textures.Texture;
  blendedTexture : SDL.Video.Textures.Texture;
  shadedTexture  : SDL.Video.Textures.Texture;
  
  
  solidRect : SDL.Video.Rectangles.Rectangle := (0,0,0,0);
  blendedRect : SDL.Video.Rectangles.Rectangle := (0,0,0,0);
  shadedRect : SDL.Video.Rectangles.Rectangle := (0,0,0,0);
  
  windowRect : SDL.Video.Rectangles.Rectangle := (1, 1, 640, 480 );

  Window           : SDL.Video.Windows.Window;
  Renderer         : SDL.Video.Renderers.Renderer;
  -------------------------------------------- 
  procedure Render is
   --ws : SDL.Video.Windows.Sizes;
   -- p : SDL.Video.Windows.Positions;
    
  begin
    -- Clear the window and make it all red
    Renderer.Clear;
    
--    Ws := Window.Get_Size;
--    p  := Window.Get_Position;
--    Renderer.Copy(Copy_From => solidTexture,   To => solidRect);
--    Renderer.Copy(Copy_From => blendedTexture, To => blendedRect);
--    Renderer.Copy(Copy_From => shadedTexture,  To => shadedRect);

    Renderer.Copy(Copy_From => solidTexture);
    Renderer.Copy(Copy_From => blendedTexture);
    Renderer.Copy(Copy_From => shadedTexture);
--    Renderer.Set_Draw_Colour(Green);
--    Renderer.Draw(solidRect);
--    Renderer.Set_Draw_Colour(Blue);
--    Renderer.Draw(blendedRect);
--    Renderer.Set_Draw_Colour(White);
--    Renderer.Draw(shadedRect);
    
    -- Render the changes above
    Renderer.Present;
    
  end Render;
-- Initialization ++

  procedure RunGame is
  begin
    Render;
    SDL.Log.Put_Debug( "runs for 5 secs");
    delay 5.0;
  end RunGame;

-- ==================================================================
  procedure SetupTTF( Fontname : String )  is
  begin
    -- SDL2_TTF needs to be initialized just like SDL2
    SDL.TTF.Init;
    -- Load our fonts, with a huge size
    Font.Open(Fontname, 90 );
    -- Error check
  end SetupTTF;
  -------------------------------------------- 

  procedure CreateTextTextures is
     Solid   : SDL.Video.Surfaces.Surface;
     Blended : SDL.Video.Surfaces.Surface;
     Shaded  : SDL.Video.Surfaces.Surface;
     F       : Natural          := 0;
     A       : Interfaces.C.Int := 0;
  begin

    SDL.Video.Surfaces.Fonts.Create(Surface  => Solid,
                                    Font     => Font,
                                    Color    => White, 
                                    Text     => "solid");
                                    
    SDL.Video.Textures.Makers.Create(Tex      => solidTexture,
                                     Renderer => Renderer,
                                     Surface  => Solid);
--    solidTexture.Query(Format => F,
--                       Acess  => A,
--                       W      => solidRect.Width,
--                       H      => solidRect.Height);
    solidRect.x      := 1;
    solidRect.y      := 1;
    solidRect.Width  := 50;
    solidRect.Height := 100;

    SDL.Log.Put_Debug("solid   x y w h" & solidRect.x'Img & solidRect.y'Img & solidRect.Width'Img & solidRect.Height'Img);

    SDL.Video.Surfaces.Fonts.Create(Surface  => blended,
                                    Font     => Font,
                                    Color    => Green, 
                                    Text     => "blended");
                                    
    SDL.Video.Textures.Makers.Create(Tex      => blendedTexture,
                                     Renderer => Renderer,
                                     Surface  => blended);
                                    
    
--    blendedTexture.Query(Format => F,
--                         Acess  => A,
--                         W      => blendedRect.Width,
--                         H      => blendedRect.Height);
--    blendedRect.x := 1;
--    blendedRect.y := solidRect.y + solidRect.Height +  20;

    blendedRect.x      := 1;
    blendedRect.y      := 110;
    blendedRect.Width  := 50;
    blendedRect.Height := 100;

    SDL.Log.Put_Debug("blended x y w h" & blendedRect.x'Img & blendedRect.y'Img & blendedRect.Width'Img & blendedRect.Height'Img);

  
    SDL.Video.Surfaces.Fonts.Create(Surface  => Shaded,
                                    Font     => Font,
                                    Color    => Blue, 
                                    Text     => "shaded");
   
    SDL.Video.Textures.Makers.Create(Tex      => shadedTexture,
                                     Renderer => Renderer,
                                     Surface  => Shaded);

--    shadedTexture.Query(Format => F,
--                        Acess  => A,
--                        W      => shadedRect.Width,
--                        H      => shadedRect.Height);
--    shadedRect.x := 1;
--    shadedRect.y := blendedRect.y + blendedRect.Height + 20;
    shadedRect.x      := 1;
    shadedRect.y      := 220;
    shadedRect.Width  := 100;
    shadedRect.Height := 100;

    
    SDL.Log.Put_Debug("shaded  x y w h" & shadedRect.x'Img & shadedRect.y'Img & shadedRect.Width'Img & shadedRect.Height'Img);
    
  end CreateTextTextures;
  -------------------------------------------- 
  procedure InitSDL is
  begin
    if not SDL.Initialise then
       SDL.Log.Put_Debug ("initilise failed");
    end if;  
  end InitSDL;
  -------------------------------------------- 
  procedure CreateWindow is
  begin
   SDL.Video.Windows.Makers.Create (Win    => Window,
                                    Title  => "Rektanglar test)",
                                    X      => Integer(windowRect.x),
                                    Y      => Integer(windowRect.y),
                                    Width  => Integer(windowRect.Width),
                                    Height => Integer(windowRect.Height),
                                    Flags  => 0);
  end CreateWindow;
  -------------------------------------------- 
  procedure SetupRenderer is
    Window_Size : SDL.Video.Rectangles.Size := (windowRect.Width, windowRect.Height);
  begin
    SDL.Video.Renderers.Makers.Create(Renderer,Window,SDL.Video.Renderers.Accelerated);
    -- Set size of renderer to the same as window
    Renderer.Set_Logical_Size(Window_Size);
    -- Set color of renderer to red
    Renderer.Set_Draw_Colour(Red);
 end SetupRenderer; 
  -------------------------------------------- 
  procedure InitEverything is
  begin
    InitSDL ;
    CreateWindow;
    SetupRenderer;
    SetupTTF("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf");
    CreateTextTextures;
  end InitEverything;
  -------------------------------------------- 
 
begin
  SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);
  InitEverything;
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
