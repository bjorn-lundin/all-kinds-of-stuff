with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Text_Io; use Text_Io;
with Stacktrace;

with SDL2; use SDL2;
with SDL2.Thin ;
--use SDL2.Thin;

with SDL2.Error ; 
with SDL2.Video.Palettes ; 
with SDL2.Video.Rectangles ; 
with SDL2.Video.Windows;
with SDL2.Video.Surfaces;
with SDL2.Video.Textures;
with SDL2.Video.Textures.Makers;
with SDL2.Video.Renderers;
with SDL2.TTF;
with SDL2.Log;

procedure Show_Result is
  use type Interfaces.C.Int;
  package C renames Interfaces.C;
      
  Bad : exception;
   
  C_Text : Chars_Ptr := New_String("Yes, Finally!");
  
-- Setup
--  Blue            :  Colour := (Red =>   0, Green =>   0, Blue => 255, Alpha => 255);
  Blue  : SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green =>   0, Blue => 255);
  Green : SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green => 255, Blue =>   0);
  Red   : SDL2.Video.Palettes.RGB_Color := (Red => 255, Green =>   0, Blue =>   0);
  
  Grey : SDL2.Video.Palettes.Color := (Red => 192, Green => 192, Blue => 192, Alpha => 255);
  
  Font : SDL2.TTF.Font;
  solidTexture   :  SDL2.Video.Textures.Texture;
  blendedTexture :  SDL2.Video.Textures.Texture;
  shadedTexture  :  SDL2.Video.Textures.Texture;
  
  solidRect   :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  blendedRect :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  shadedRect  :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  
  windowRect : SDL2.Video.Rectangles.Rectangle := (1, 1, 1200, 700 );

  Window       :  SDL2.Video.Windows.Window_Type;
  Renderer :  SDL2.Video.Renderers.Renderer;

  solidTexture_Ptr   :  Texture_Pointer := null;


  procedure Render is
     Result : C.int := 0;
  begin
    -- Clear the window and make it all red

    
--    Renderer.Clear;
    Result := SDL2.Thin.SDL_Render_Clear(Renderer.Get_Pointer);

--    Renderer.Copy(Copy_From => solidTexture, To => solidRect);

    Result  :=  SDL2.Thin.SDL_Render_Copy (Renderer.Get_Pointer,
                                           --solidTexture.Get_Pointer,
                                           solidTexture_Ptr ,
                                           null,
                                           solidRect'unchecked_access);

    Put_Line("solidRect   x y w h" & solidRect.X'Img & solidRect.Y'img & solidRect.Width'img & solidRect.Height'img);
   -- Renderer.Copy(Copy_From => blendedTexture, To => blendedRect);
   -- Put_Line("blendedRect x y w h" & blendedRect.X'Img & blendedRect.Y'img & blendedRect.Width'img & blendedRect.Height'img);
   -- Renderer.Present;
    SDL2.Thin.SDL_Render_Present(Renderer.Get_Pointer);
    
    
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
  procedure CreateTextTextures is
    Local_Format : aliased Interfaces.C.Long  := 0;
    Local_Acess  : aliased Interfaces.C.Int := 0;
    Local_W      : aliased Interfaces.C.Int := 0;
    Local_H      : aliased Interfaces.C.Int := 0;
    Result : C.Int := 0;
    
    Surface : SDL2.Video.Surfaces.Surface;
    Surface_Ptr : Surface_Pointer := null;


  begin
    --SDL2.Video.Surfaces.Create_Solid(Self  => Surface,
    --                                 Font  => Font,
    --                                 Color => Blue ,
    --                                 Text  => "Thick as a brick" ) ;
    --
    --SDL2.Video.Textures.Makers.Create(Self     => solidTexture,
    --                                  Renderer => Renderer,
    --                                  Surface  => Surface);
    --Surface.Destroy;                                       
    --                                   
    --solidTexture.Query (Natural(Local_Format), 
    --                    Local_Acess, 
    --                    Local_W, 
    --                    Local_H);
    Surface_Ptr := SDL2.Thin.TTF_RenderText_Solid(Font_Ptr    => Font.Get_Pointer,  
                                        Text        => C_Text,
                                        Fore_Ground => Blue); 
    solidTexture_Ptr := SDL2.Thin.SDL_Create_Texture_From_Surface(Renderer.Get_Pointer, Surface_Ptr);
    Sdl2.Thin.SDL_Free_Surface(Surface_Ptr);    
       
    Result := SDL2.Thin.SDL_Query_Texture (solidTexture_Ptr, 
                                 Local_Format'access, 
                                 Local_Acess'access, 
                                 Local_W'access, 
                                 Local_H'access);
    if Result /= 0 then
      Put_Line ("SDL_Query_Texture failed");
      raise Bad with SDL2.Error.Get;
    end if;  
                        
                        
       
    solidRect.x := 0;
    solidRect.y := 0;
    solidRect.Width := Local_W;
    solidRect.Height := Local_H;
    -- -- -- -- -- -- -- -- 

--    SDL2.Video.Surfaces.Create_Solid(Self  => Surface,
--                                     Font  => Font,
--                                     Color => Blue ,
--                                     Text  => "Thick as a brick" ) ;
--    
--    SDL2.Video.Textures.Makers.Create(Self     => blendedTexture,
--                                      Renderer => Renderer,
--                                      Surface  => Surface);
--    Surface.Destroy;                                       
--    blendedTexture.Query (Natural(Local_Format), 
--                        Local_Acess, 
--                        Local_W, 
--                        Local_H);
--       
--    blendedRect.Width := Local_W;
--    blendedRect.Height := Local_H;
--
--    blendedRect.x := 0;
--    blendedRect.y := solidRect.y + solidRect.Height +  20;
--    -- -- -- -- -- -- -- -- 
--
--    SDL2.Video.Surfaces.Create_Solid(Self  => Surface,
--                                     Font  => Font,
--                                     Color => Blue ,
--                                     Text  => "Thick as a brick" ) ;
--    
--    SDL2.Video.Textures.Makers.Create(Self     => shadedTexture,
--                                      Renderer => Renderer,
--                                      Surface  => Surface);
--    Surface.Destroy;                                       
--    shadedTexture.Query (Natural(Local_Format), 
--                        Local_Acess, 
--                        Local_W, 
--                        Local_H);
--       
--    shadedRect.Width := Local_W;
--    shadedRect.Height := Local_H;
--    shadedRect.x := 0;
--    shadedRect.y := blendedRect.y + blendedRect.Height + 20;    
  end CreateTextTextures;
  -----------------------------------------------------
-----------------------------------------------------------

  procedure InitEverything  is
  begin
    Sdl2.Init;
    Window := SDL2.Video.Windows.Create("Rektanglar test",
            50,50,600,400,SDL2.Video.Windows.Resizable); 
    SDL2.Video.Renderers.Create(Renderer,Window,SDL2.Video.Renderers.Accelerated);    
    Renderer.Set_Logical_Size((windowRect.Width, windowRect.Height));
    Renderer.Set_Draw_Color (Grey);
    SDL2.TTF.Init;
    Font.Open("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", 30);
    CreateTextTextures;
  end InitEverything;
  
 
begin
  SDL2.Log.Set (Category =>  SDL2.Log.Application, Priority =>  SDL2.Log.Debug);
  InitEverything;
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
        Put_Line(Last_Exception_Name);
        Put_Line("Message : " & Last_Exception_Messsage);
        Put_Line(Last_Exception_Info);
        Put_Line("addr2line" & " --functions --basenames --exe=" &
           Ada.Command_Line.Command_Name & " " & Stacktrace.Pure_Hexdump(Last_Exception_Info));
    end ;
  
end Show_Result;
