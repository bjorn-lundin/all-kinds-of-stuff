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
with SDL2.Video.Palettes;
with SDL2.Video.Rectangles ; 
with SDL2.Video.Windows;


procedure Show_Result_Thin is
  use type Interfaces.C.Int;
  package C renames Interfaces.C;
      
  Bad : exception;
   
  Surface_Ptr : Surface_Pointer := null;
  C_Text : Chars_Ptr := New_String("Yes, Finally!");
  
-- Setup
--  Blue            :  Colour := (Red =>   0, Green =>   0, Blue => 255, Alpha => 255);
  Blue  :  SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green =>   0, Blue => 255);
  Green :  SDL2.Video.Palettes.RGB_Color := (Red =>   0, Green => 255, Blue =>   0);
  
  Font_Ptr           :  Font_Pointer := null;
  solidTexture_Ptr   :  Texture_Pointer := null;
  blendedTexture_Ptr :  Texture_Pointer := null;
  shadedTexture_Ptr  :  Texture_Pointer := null;
   
  solidRect   :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  blendedRect :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  shadedRect  :  aliased SDL2.Video.Rectangles.Rectangle := (0,0,0,0);
  
  windowRect : SDL2.Video.Rectangles.Rectangle := (1, 1, 1200, 700 );

--  Window_Ptr       :  Window_Pointer   := null;
  Window       :  SDL2.Video.Windows.Window_Type;
  Renderer_Ptr :  Renderer_Pointer := null;

  

  procedure Render is
     Result : C.int := 0;
  begin
    -- Clear the window and make it all red
    Result := SDL_Render_Clear(Renderer_Ptr);
    if Result /= 0 then
       raise Bad with SDL2.Error.Get;
    end if;
    
    Result  := SDL_Render_Copy (Renderer_Ptr,
                                solidTexture_Ptr,
                                null,
                                solidRect'unchecked_access);
    if Result /= 0 then
       raise Bad with SDL2.Error.Get;
    end if;
    Put_Line("solidRect   x y w h" & solidRect.X'Img & solidRect.Y'img & solidRect.Width'img & solidRect.Height'img);
    
    Result  := SDL_Render_Copy (Renderer_Ptr,
                                blendedTexture_Ptr,
                                null,
                                blendedRect'unchecked_access);
    if Result /= 0 then
       raise Bad with SDL2.Error.Get;
    end if;
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
    Surface : SDL2.Video.Surfaces.Surface;
    solidTexture   :  SDL2.Video.Textures.Texture;
    
  begin
--    Surface_Ptr := TTF_RenderText_Solid(Font_Ptr    => Font_Ptr,  
--                                        Text        => C_Text,
--                                        Fore_Ground => Blue); 
--    solidTexture_Ptr := SDL_Create_Texture_From_Surface(Renderer_Ptr, Surface_Ptr);
--    SDL_Free_Surface(Surface_Ptr);    
--       
--    Result := SDL_Query_Texture (solidTexture_Ptr, 
--                                 Local_Format'access, 
--                                 Local_Acess'access, 
--                                 Local_W'access, 
--                                 Local_H'access);
--    if Result /= 0 then
--      Put_Line ("SDL_Query_Texture failed");
--      raise Bad with SDL2.Error.Get;
--    end if;  


    SDL2.Video.Surfaces.Create_Solid(Self  => Surface,
                                     Font  => Font,
                                     Color => Blue ,
                                     Text  => "Thick as a brick" ) ;
    
    SDL2.Video.Textures.Makers.Create(Self     => solidTexture,
                                      Renderer => Renderer,
                                      Surface  => Surface);
                                      
    SDL2.Video.Textures.Makers.Create(Self     => solidTexture,
                                      Pointer => Renderer,
                                      Owner   => True);
                                      
    Surface.Destroy;                                       
                                       
    solidTexture.Query (Natural(Local_Format), 
                        Local_Acess, 
                        Local_W, 
                        Local_H);
                        
    solidTexture_Ptr :=solidTexture.Get_Pointer;

       
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
    if Result /= 0 then
      Put_Line ("SDL_Query_Texture failed");
      raise Bad with SDL2.Error.Get;
    end if;  
       
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
    if Result /= 0 then
      Put_Line ("SDL_Query_Texture failed");
      raise Bad with SDL2.Error.Get;
    end if;  
       
    shadedRect.Width := Local_W;
    shadedRect.Height := Local_H;
    shadedRect.x := 0;
    shadedRect.y := blendedRect.y + blendedRect.Height + 20;    
    Free(C_Text);
  end CreateTextTextures;
  -----------------------------------------------------

  procedure InitSDL is
   
  begin
    if SDL_Init /= 0 then
      Put_Line ("initilise failed");
      return ;
    end if;  
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
  end CreateWindow;
-----------------------------------------------------
  procedure CreateRenderer  is 
  begin
     Renderer_Ptr := SDL_Create_Renderer(Window.Get_Pointer, -1, 16#0000_0002#);
     if Renderer_Ptr = null then
         raise Bad with SDL2.Error.Get;
      end if;
  end CreateRenderer;
-----------------------------------------------------------
  procedure SetupRenderer is
    Window_Size : SDL2.Video.Rectangles.Size := (windowRect.Width, windowRect.Height);
    We : aliased Interfaces.C.Int := Window_Size.Width;
    He : aliased Interfaces.C.Int := Window_Size.Height;
    Result : C.int :=0;      
  begin
    -- Set size of renderer to the same as window
      Result := SDL_Render_Set_Logical_Size (Renderer_Ptr, We, He);
      if Result /= 0 then
         raise Bad with SDL2.Error.Get;
      end if;
      
      Result := SDL_Set_Render_Draw_Color (Renderer_Ptr, 0, 0, 0, 255);
      if Result /= 0 then
         raise Bad with SDL2.Error.Get;
      end if;
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
  InitEverything;
  RunGame;
  TTF_Close_Font(Font_Ptr);
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
  
end Show_Result_Thin;
