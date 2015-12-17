with Ada.Command_Line;
with Ada.Exceptions;
with Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with Text_Io; use Text_Io;
with Stacktrace;

with SDL2; use SDL2;
with SDL2.Thin ;
use SDL2.Thin;

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
   
--  Surface_Ptr : Surface_Pointer := null;
  C_Text : Chars_Ptr := New_String("Yes, Finally!");
  
-- Setup
--  Blue            :  Colour := (Red =>   0, Green =>   0, Blue => 255, Alpha => 255);
  Blue  : SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green =>   0, Blue => 255);
  Green : SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green => 255, Blue =>   0);
  Red   : SDL2.Video.Palettes.RGB_Color := (Red => 255, Green =>   0, Blue =>   0);
  
  Grey : SDL2.Video.Palettes.Color := (Red => 192, Green => 192, Blue => 192, Alpha => 255);
  
  --Font_Ptr           :  Font_Pointer := null;
  Font : SDL2.TTF.Font;
  
  
--  solidTexture_Ptr   :  Texture_Pointer := null;
--  blendedTexture_Ptr :  Texture_Pointer := null;
--  shadedTexture_Ptr  :  Texture_Pointer := null;

  solidTexture   :  SDL2.Video.Textures.Texture;
  blendedTexture :  SDL2.Video.Textures.Texture;
  shadedTexture  :  SDL2.Video.Textures.Texture;


  
  solidRect   :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  blendedRect :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  shadedRect  :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  
  windowRect : SDL2.Video.Rectangles.Rectangle := (1, 1, 1200, 700 );

--  Window_Ptr       :  Window_Pointer   := null;
  Window       :  SDL2.Video.Windows.Window_Type;
--  Renderer_Ptr :  Renderer_Pointer := null;
  Renderer :  SDL2.Video.Renderers.Renderer;

  

  procedure Render is
     Result : C.int := 0;
  begin
    -- Clear the window and make it all red
   -- Result := SDL_Render_Clear(Renderer_Ptr);
   -- if Result /= 0 then
   --    raise Bad with SDL2.Error.Get;
   -- end if;
    
    Renderer.Clear;
    
    --Result  := SDL_Render_Copy (Renderer_Ptr,
    --                            solidTexture_Ptr,
    --                            null,
    --                            solidRect'unchecked_access);
    --if Result /= 0 then
    --   raise Bad with SDL2.Error.Get;
    --end if;
    
    Renderer.Copy(Copy_From => solidTexture, To => solidRect);
    
    
    Put_Line("solidRect   x y w h" & solidRect.X'Img & solidRect.Y'img & solidRect.Width'img & solidRect.Height'img);
    
    --Result  := SDL_Render_Copy (Renderer_Ptr,
    --                            blendedTexture_Ptr,
    --                            null,
    --                            blendedRect'unchecked_access);
    --if Result /= 0 then
    --   raise Bad with SDL2.Error.Get;
    --end if;
    Renderer.Copy(Copy_From => blendedTexture, To => blendedRect);

    Put_Line("blendedRect x y w h" & blendedRect.X'Img & blendedRect.Y'img & blendedRect.Width'img & blendedRect.Height'img);
--    
--    Result  := SDL_Render_Copy (Renderer_Ptr,
--                                shadedTexture_Ptr,
--                                null,
--                                shadedRect'unchecked_access);
--    if Result /= 0 then
--       raise Bad with SDL2.Error.Get;
--    end if;
    
--    Renderer.Set_Draw_Colour(Blue);
--    Renderer.Draw(solidRect);
    
    
    -- Render the changes above
   -- SDL_Render_Present(Renderer_Ptr);
    Renderer.Present;
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
  --  F_init: C.Int := 0;
  --  C_Filename : Chars_Ptr := New_String( "/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf");
  begin
   -- F_Init := TTF_Init;
   -- if F_Init /= 0 then
   --   Put_Line ("font initilise failed");
   --   raise Bad;
   -- end if;  
   SDL2.TTF.Init;
   
   Font.Open("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf", 30);
--    Font_Ptr := TTF_OpenFont(C_Filename,90 ) ;
--    if Font_Ptr = null then
--      Put_Line ("font2 initilise failed");
--      raise Bad;
--    end if;  
--    Free(C_Filename);
  end SetupTTF;
  -----------------------------------------------------
  procedure CreateTextTextures is
    Local_Format : aliased Interfaces.C.Long  := 0;
    Local_Acess  : aliased Interfaces.C.Int := 0;
    Local_W      : aliased Interfaces.C.Int := 0;
    Local_H      : aliased Interfaces.C.Int := 0;
    Result : C.Int := 0;
    
    Surface1 : SDL2.Video.Surfaces.Surface;
    Surface2 : SDL2.Video.Surfaces.Surface;
    Surface3 : SDL2.Video.Surfaces.Surface;

  begin
    SDL2.Video.Surfaces.Create_Solid(Self  => Surface1,
                                     Font  => Font,
                                     Color => Blue ,
                                     Text  => "Thick as a brick" ) ;
    
    SDL2.Video.Textures.Makers.Create(Self     => solidTexture,
                                       Renderer => Renderer,
                                       Surface  => Surface1);
    solidTexture.Query (Natural(Local_Format), 
                        Local_Acess, 
                        Local_W, 
                        Local_H);
       
    solidRect.x := 0;
    solidRect.y := 0;
    solidRect.Width := Local_W;
    solidRect.Height := Local_H;
    -- -- -- -- -- -- -- -- 

    SDL2.Video.Surfaces.Create_Solid(Self  => Surface2,
                                     Font  => Font,
                                     Color => Blue ,
                                     Text  => "Thick as a brick" ) ;
    
    SDL2.Video.Textures.Makers.Create(Self     => blendedTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface2);
    blendedTexture.Query (Natural(Local_Format), 
                        Local_Acess, 
                        Local_W, 
                        Local_H);
       
    blendedRect.Width := Local_W;
    blendedRect.Height := Local_H;

    blendedRect.x := 0;
    blendedRect.y := solidRect.y + solidRect.Height +  20;
    -- -- -- -- -- -- -- -- 

    SDL2.Video.Surfaces.Create_Solid(Self  => Surface3,
                                     Font  => Font,
                                     Color => Blue ,
                                     Text  => "Thick as a brick" ) ;
    
    SDL2.Video.Textures.Makers.Create(Self     => shadedTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface3);
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

  procedure InitSDL is
   
  begin
--    if SDL_Init /= 0 then
--      Put_Line ("initilise failed");
--      return ;
--    end if;  
    Sdl2.Init;
  end InitSDL;
-------------------------------------------
  procedure CreateWindow is
--      C_Title_Str : Chars_Ptr := C.Strings.New_String ("Rektanglar test thin");
--   begin    
--      Window_Ptr := SDL_Create (C_Title_Str,
--                                windowRect.X, 
--                                windowRect.Y, 
--                                windowRect.Width, 
--                                windowRect.Height,
--                                0);
--                                
--      C.Strings.Free (C_Title_Str);
--      if Window_Ptr = null then
--         raise Bad with SDL2.Error.Get;
--      end if;
  begin
     Window := SDL2.Video.Windows.Create("Rektanglar test",
          50,50,100,200,SDL2.Video.Windows.Resizable); 
          
     if Window.Get_Pointer = null then
       raise Sdl_Error with SDL2.Error.Get;
     end if;
          
          
  end CreateWindow;
-----------------------------------------------------
  procedure CreateRenderer  is 
  begin
     SDL2.Video.Renderers.Create(Renderer,Window,SDL2.Video.Renderers.Accelerated);
     if Renderer.Get_Pointer = null then
       raise Sdl_Error with SDL2.Error.Get;
     end if;
    
    
    --Window.Get_Renderer;
    --Renderer_Ptr := SDL_Create_Renderer(Window.Get_Pointer, -1, 16#0000_0002#);
    --if Renderer_Ptr = null then
    --    raise Bad with SDL2.Error.Get;
    -- end if;
  end CreateRenderer;
-----------------------------------------------------------
  procedure SetupRenderer is
    Window_Size : SDL2.Video.Rectangles.Size := (windowRect.Width, windowRect.Height);
    We : aliased Interfaces.C.Int := Window_Size.Width;
    He : aliased Interfaces.C.Int := Window_Size.Height;
    Result : C.int :=0;      
  begin
    -- Set size of renderer to the same as window
      Renderer.Set_Logical_Size(Window_Size);
     -- Result := SDL_Render_Set_Logical_Size (Renderer_Ptr, We, He);
     -- if Result /= 0 then
     --    raise Bad with SDL2.Error.Get;
     -- end if;
      
      Renderer.Set_Draw_Color (Grey);
      
  --    Result := SDL_Set_Render_Draw_Color (Renderer_Ptr, 0, 0, 0, 255);
  --    if Result /= 0 then
  --       raise Bad with SDL2.Error.Get;
  --    end if;
 end SetupRenderer; 

 
  procedure InitEverything  is
  begin
    InitSDL;
    CreateWindow;
    CreateRenderer;
    SetupRenderer;
    SetupTTF;
    CreateTextTextures;
  end InitEverything;
  
 
begin
  SDL2.Log.Set (Category =>  SDL2.Log.Application, Priority =>  SDL2.Log.Debug);

  InitEverything;
  RunGame;
  --TTF_Close_Font(Font_Ptr);
  Font.Close;
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
  
end Show_Result;
