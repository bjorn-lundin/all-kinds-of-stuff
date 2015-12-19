with Interfaces.C;
with Text_Io; use Text_Io;
with Stacktrace;

with SDL2; use SDL2;
with SDL2.Video.Palettes;
with SDL2.Video.Rectangles ; 
with SDL2.Video.Windows;
with SDL2.Video.Surfaces;
with SDL2.Video.Textures;
with SDL2.Video.Textures.Makers;
with SDL2.Video.Renderers;
with SDL2.TTF;

procedure Show_Result_Test is
  use type Interfaces.C.Int;
  
-- Setup
  RGB_Blue  :  SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green =>   0, Blue => 255);
--  RGB_Green :  SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green => 255, Blue =>   0);
  RGB_Gray  :  SDL2.Video.Palettes.RGB_Color := (Red =>   192, Green => 192, Blue =>   192);
  Blue  :  SDL2.Video.Palettes.Color := (Red =>   0, Green =>   0, Blue => 255, Alpha => 255);
  Green :  SDL2.Video.Palettes.Color := (Red =>   0, Green => 255, Blue =>   0, Alpha => 255);
--  Gray  :  SDL2.Video.Palettes.Color := (Red =>   192, Green => 192, Blue =>   192, Alpha => 255);
  
  Font : SDL2.TTF.Font;
     
  solidRect   :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  blendedRect :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  shadedRect  :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  
  windowRect     : SDL2.Video.Rectangles.Rectangle := (1, 1, 1200, 700);
  Window         : SDL2.Video.Windows.Window_Type;
  Renderer       : SDL2.Video.Renderers.Renderer; 
  solidTexture   : SDL2.Video.Textures.Texture;
  blendedTexture : SDL2.Video.Textures.Texture;
  shadedTexture  : SDL2.Video.Textures.Texture;

  procedure Render is
  begin
    Renderer.Clear;
    Renderer.Copy(Copy_From => solidTexture, To => solidRect);
    Put_Line("solidRect   x y w h" & solidRect.X'Img & solidRect.Y'img & solidRect.Width'img & solidRect.Height'img);
    Renderer.Copy(Copy_From => blendedTexture, To => blendedRect);
    Put_Line("blendedRect x y w h" & blendedRect.X'Img & blendedRect.Y'img & blendedRect.Width'img & blendedRect.Height'img);
    Renderer.Copy(Copy_From => shadedTexture, To => shadedRect);
    Put_Line("shadedRect  x y w h" & shadedRect.X'Img & shadedRect.Y'img & shadedRect.Width'img & shadedRect.Height'img);
    Renderer.Set_Draw_Color(Blue);
    Renderer.Draw(solidRect);
    Renderer.Present;
  end Render;
  ------------------------------------------------
  procedure RunGame is
  begin
    Render;
    Put_Line( "runs for 5 secs");
    delay 5.0;
  end RunGame;
  
-- Initialization ++
-- ==================================================================

  -----------------------------------------------------
  procedure CreateTextTextures is
    Local_Format : aliased Interfaces.C.Long  := 0;
    Local_Acess  : aliased Interfaces.C.Int := 0;
    Local_W      : aliased Interfaces.C.Int := 0;
    Local_H      : aliased Interfaces.C.Int := 0;
    Surface      : SDL2.Video.Surfaces.Surface;
  begin
    SDL2.Video.Surfaces.Create_Solid(Self  => Surface,
                                     Font  => Font,
                                     Color => RGB_Blue ,
                                     Text  => "Thick as a brick" ) ;
    SDL2.Video.Textures.Makers.Create(Self     => solidTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface);
    Surface.Destroy;                                       
    solidTexture.Query (Natural(Local_Format), 
                        Local_Acess, 
                        Local_W, 
                        Local_H);
                            
    solidRect.x := 0;
    solidRect.y := 0;
    solidRect.Width := Local_W;
    solidRect.Height := Local_H;   
    -----------------------------------------------------------
       
    SDL2.Video.Surfaces.Create_Blended(Self  => Surface,
                                     Font  => Font,
                                     Color => RGB_Blue ,
                                     Text  => "Yes Thick as a brick" ) ;
    
    SDL2.Video.Textures.Makers.Create(Self     => blendedTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface);
    Surface.Destroy;                                       
    blendedTexture.Query (Natural(Local_Format), 
                        Local_Acess, 
                        Local_W, 
                        Local_H);
       
    blendedRect.Width := Local_W;
    blendedRect.Height := Local_H;

    blendedRect.x := 0;
    blendedRect.y := solidRect.y + solidRect.Height +  20;
    -- -- -- -- -- -- -- -- 
    
    SDL2.Video.Surfaces.Create_Shaded(Self  => Surface,
                                     Font  => Font,
                                     Color => RGB_Blue ,
                                     Back_Color => RGB_Gray ,
                                     Text  => "What? Thicker as a brick" ) ;
    
    SDL2.Video.Textures.Makers.Create(Self     => shadedTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface);
    Surface.Destroy;                                       
    shadedTexture.Query (Natural(Local_Format), 
                        Local_Acess, 
                        Local_W, 
                        Local_H);
    
    shadedRect.Width := Local_W;
    shadedRect.Height := Local_H;
    shadedRect.x := 0;
    shadedRect.y := blendedRect.y + blendedRect.Height + 20;    
  end CreateTextTextures;
  -----------------------------------------------------

  procedure InitEverything  is
    Window_Size : SDL2.Video.Rectangles.Size := (windowRect.Width, windowRect.Height);
  begin
    Sdl2.Init;
    Window := SDL2.Video.Windows.Create("Rektanglar test",
                       windowRect, SDL2.Video.Windows.Resizable); 
    SDL2.Video.Renderers.Create(Renderer,
                                Window,
                                SDL2.Video.Renderers.Accelerated);
    Renderer.Set_Logical_Size(Window_Size);
    Renderer.Set_Draw_Color(Green);
    SDL2.TTF.Init;
    Font.Open("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", 90);
    CreateTextTextures;
  end InitEverything;
  
 
begin
  InitEverything;
  RunGame;
  Font.Close;
  SDL2.TTF.Quit;
  
exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end Show_Result_Test;
