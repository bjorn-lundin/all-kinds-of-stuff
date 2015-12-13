-


with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Text_Io; use Text_Io;

with  SDL2.Log;
with  SDL2.TTF;
with  SDL2.Video.Windows;
with  SDL2.Video.Palettes;
with  SDL2.Video.Textures;
with  SDL2.Video.Renderers;
with  SDL2.Video.Surfaces;
with  SDL2.Video.Rectangles;



with  Stacktrace;


procedure SDL2Test is
  use type Interfaces.C.Int;

-- Setup
  function InitEverything return Boolean;
  function InitSDL return Boolean;
  function CreateWindow return Boolean;
  function CreateRenderer return Boolean;
  procedure SetupRenderer;

  -- Our new function for setting uo SDL_TTF
  function SetupTTF( fontname : string ) return Boolean;
  Font :  SDL2.TTF.Font;

  procedure SurfaceToTexture (T : in out  SDL2.Video.Textures.Texture;
                              S : in      SDL2.Video.Surfaces.Surface ) ;
  --SDL_Texture* SurfaceToTexture( SDL_Surface* surf );
  
  procedure CreateTextTextures ;

  -- Update ( happens every frame )
  procedure Render;
  procedure RunGame;
  
  -- Stuff for text rendering
  --SDL_Color textColor = { 255, 255, 255, 255 }; -- white
  --SDL_Color backgroundColor = { 0, 0, 0, 255 }; -- black
  textColor       :  SDL2.Video.Palettes.Colour := (Red => 255, Green => 255, Blue => 255, Alpha => 255);-- white
  backgroundColor :  SDL2.Video.Palettes.Colour := (Red =>   0, Green =>   0, Blue =>   0, Alpha => 255);-- black
  Red             :  SDL2.Video.Palettes.Colour := (Red => 255, Green =>   0, Blue =>   0, Alpha => 255);-- red


  solidTexture   :  SDL2.Video.Textures.Texture;
  blendedTexture :  SDL2.Video.Textures.Texture;
  shadedTexture  :  SDL2.Video.Textures.Texture;
  
  
  solidRect :  SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  blendedRect :  SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  shadedRect :  SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  
  windowRect :  SDL2.Video.Rectangles.Rectangle := (900, 300, 400, 400 );

  Window           :  SDL2.Video.Windows.Window;
  Renderer         :  SDL2.Video.Renderers.Renderer;


  
  
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
     SDL2.TTF.Init;
    -- Load our fonts, with a huge size
    Font.Open( Fontname, 90 );
    -- Error check
    return true;
  end SetupTTf;

  procedure CreateTextTextures is
     Solid   :  SDL2.Video.Surfaces.Surface;
     Blended :  SDL2.Video.Surfaces.Surface;
     Shaded  :  SDL2.Video.Surfaces.Surface;
     F       : Natural          := 0;
     A       : Interfaces.C.Int := 0;
  begin

     SDL2.Video.Surfaces.Create(Surface  => Solid,
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


     SDL2.Video.Surfaces.Create(Surface  => Blended,
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

  
     SDL2.Video.Surfaces.Create(Surface  => Shaded,
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

  procedure SurfaceToTexture (T : in out  SDL2.Video.Textures.Texture;
                              S : in      SDL2.Video.Surfaces.Surface ) is
  begin
     SDL2.Video.Textures.Create(Tex      => T,
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
    if not  SDL2.Initialise then
      Put_Line ("initilise failed");
      return false;
    end if;  
    return true;
  end InitSDL;

  function CreateWindow return Boolean is
  begin
    SDL2.Video.Windows.Create (Win    => Window,
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
     SDL2.Video.Renderers.Create(Renderer,Window, SDL2.Video.Renderers.Accelerated);
    return true;
  end CreateRenderer;

  procedure SetupRenderer is
    Window_Size :  SDL2.Video.Rectangles.Size := (windowRect.Width, windowRect.Height);
  begin
    -- Set size of renderer to the same as window
    Renderer.Set_Logical_Size(Window_Size);
    -- Set color of renderer to red
    Renderer.Set_Draw_Colour(Red);
 end SetupRenderer; 

begin
   SDL2.Log.Set (Category =>  SDL2.Log.Application, Priority =>  SDL2.Log.Debug);
  if not InitEverything
     then return ;
  end if;
  RunGame;
  Font.Close;
   SDL2.TTF.Quit;
  
exception
  when E: others =>
    declare
      Last_Exception_Name     : constant String  := Ada.Exceptions.Exception_Name(E);
      Last_Exception_Messsage : constant String  := Ada.Exceptions.Exception_Message(E);
      Last_Exception_Info     : constant String  := Ada.Exceptions.Exception_Information(E);
    begin
        SDL2.Log.Put_Debug(Last_Exception_Name);
        SDL2.Log.Put_Debug("Message : " & Last_Exception_Messsage);
        SDL2.Log.Put_Debug(Last_Exception_Info);
        SDL2.Log.Put_Debug("addr2line" & " --functions --basenames --exe=" &
           Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;
  
end SDL2Test;
