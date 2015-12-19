with Ada.Command_Line;
with Ada.Exceptions;
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
with SDL2.Log;

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
    SDL2.Log.Put_Debug("32");
    Renderer.Clear;
    SDL2.Log.Put_Debug("33");
    Renderer.Copy(Copy_From => solidTexture, To => solidRect);
    SDL2.Log.Put_Debug("34");
    Put_Line("solidRect   x y w h" & solidRect.X'Img & solidRect.Y'img & solidRect.Width'img & solidRect.Height'img);
    Renderer.Copy(Copy_From => blendedTexture, To => blendedRect);
    SDL2.Log.Put_Debug("35");
    Put_Line("blendedRect x y w h" & blendedRect.X'Img & blendedRect.Y'img & blendedRect.Width'img & blendedRect.Height'img);
    Renderer.Copy(Copy_From => shadedTexture, To => shadedRect);
    SDL2.Log.Put_Debug("36");
    Put_Line("shadedRect  x y w h" & shadedRect.X'Img & shadedRect.Y'img & shadedRect.Width'img & shadedRect.Height'img);
    Renderer.Set_Draw_Color(Blue);
    SDL2.Log.Put_Debug("37");
    Renderer.Draw(solidRect);
    SDL2.Log.Put_Debug("38");
    Renderer.Present;
    SDL2.Log.Put_Debug("39");
  end Render;
  ------------------------------------------------
  procedure RunGame is
  begin
    SDL2.Log.Put_Debug("30");
    Render;
    SDL2.Log.Put_Debug("31");
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
    SDL2.Log.Put_Debug("14");
    SDL2.Video.Surfaces.Create_Solid(Self  => Surface,
                                     Font  => Font,
                                     Color => RGB_Blue ,
                                     Text  => "Thick as a brick" ) ;
    SDL2.Log.Put_Debug("15");
    SDL2.Video.Textures.Makers.Create(Self     => solidTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface);
    SDL2.Log.Put_Debug("16");
    Surface.Destroy;
    SDL2.Log.Put_Debug("17");
    solidTexture.Query (Natural(Local_Format),
                        Local_Acess,
                        Local_W,
                        Local_H);
    SDL2.Log.Put_Debug("18");

    solidRect.x := 0;
    solidRect.y := 0;
    solidRect.Width := Local_W;
    solidRect.Height := Local_H;
    SDL2.Log.Put_Debug("19");
    -----------------------------------------------------------

    SDL2.Video.Surfaces.Create_Blended(Self  => Surface,
                                     Font  => Font,
                                     Color => RGB_Blue ,
                                     Text  => "Yes Thick as a brick" ) ;
    SDL2.Log.Put_Debug("20");

    SDL2.Video.Textures.Makers.Create(Self     => blendedTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface);
    SDL2.Log.Put_Debug("21");
    Surface.Destroy;
    SDL2.Log.Put_Debug("22");
    blendedTexture.Query (Natural(Local_Format),
                        Local_Acess,
                        Local_W,
                        Local_H);
    SDL2.Log.Put_Debug("23");

    blendedRect.Width := Local_W;
    blendedRect.Height := Local_H;

    blendedRect.x := 0;
    blendedRect.y := solidRect.y + solidRect.Height +  20;
    -- -- -- -- -- -- -- --
    SDL2.Log.Put_Debug("24");

    SDL2.Video.Surfaces.Create_Shaded(Self  => Surface,
                                     Font  => Font,
                                     Color => RGB_Blue ,
                                     Back_Color => RGB_Gray ,
                                     Text  => "What? Thicker as a brick" ) ;
    SDL2.Log.Put_Debug("25");

    SDL2.Video.Textures.Makers.Create(Self     => shadedTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface);
    SDL2.Log.Put_Debug("26");
    Surface.Destroy;
    SDL2.Log.Put_Debug("27");
    shadedTexture.Query (Natural(Local_Format),
                        Local_Acess,
                        Local_W,
                        Local_H);
    SDL2.Log.Put_Debug("28");

    shadedRect.Width := Local_W;
    shadedRect.Height := Local_H;
    shadedRect.x := 0;
    shadedRect.y := blendedRect.y + blendedRect.Height + 20;
    SDL2.Log.Put_Debug("29");

  end CreateTextTextures;
  -----------------------------------------------------

  procedure InitEverything  is
    Window_Size : SDL2.Video.Rectangles.Size := (windowRect.Width, windowRect.Height);
  begin
    SDL2.Log.Put_Debug("1");
    Sdl2.Init;
    SDL2.Log.Put_Debug("2");
    Window := SDL2.Video.Windows.Create("Rektanglar test",
                       windowRect, SDL2.Video.Windows.Resizable);
    SDL2.Log.Put_Debug("3");
    SDL2.Video.Renderers.Create(Renderer,
                                Window,
                                SDL2.Video.Renderers.Accelerated);
    SDL2.Log.Put_Debug("4");
    Renderer.Set_Logical_Size(Window_Size);
    SDL2.Log.Put_Debug("5");
    Renderer.Set_Draw_Color(Green);
    SDL2.Log.Put_Debug("6");
    SDL2.TTF.Init;
    SDL2.Log.Put_Debug("7");
    Font.Open("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", 90);
    SDL2.Log.Put_Debug("8");
    CreateTextTextures;
    SDL2.Log.Put_Debug("9");
  end InitEverything;


begin
  SDL2.Log.Set (Category =>  SDL2.Log.Application, Priority =>  SDL2.Log.Debug);
  SDL2.Log.Put_Debug("0");
  InitEverything;
  SDL2.Log.Put_Debug("10");
  RunGame;
  SDL2.Log.Put_Debug("11");
  Font.Close;
  SDL2.Log.Put_Debug("12");
  SDL2.TTF.Quit;
  SDL2.Log.Put_Debug("13");

exception
  when E: others =>
--    Stacktrace.Tracebackinfo(E);
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
end Show_Result_Test;
