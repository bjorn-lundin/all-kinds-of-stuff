with Ada.Command_Line;
with Ada.Exceptions;

with SDL;
--with SDL.Error;
with SDL.Log;
with SDL.Video.Palettes;
--with SDL.Video.Pixel_Formats;
--with SDL.Video.Pixels;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
--with SDL.Versions;
with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with SDL.TTF;
with SDL.Video.Surfaces.Fonts;
with Stacktrace;
with Interfaces.C;

procedure Rectangles is
   use type Interfaces.C.Int;


   W                : SDL.Video.Windows.Window;
   Renderer         : SDL.Video.Renderers.Renderer;
   Texture          : SDL.Video.Textures.Texture;
 --  Pixels           : SDL.Video.Pixels.ARGB_8888_Access.Pointer;
 --  Pitches          : SDL.Video.Pixels.Pitch_Access.Pointer;
 --  W_Size           : SDL.Video.Windows.Sizes := (640, 480);

   Surface  :  SDL.Video.Surfaces.Surface;

  -- Rectangle : SDL.Video.Rectangles.Rectangle := (50,50,50,50);
   Text_Rectangle0 : SDL.Video.Rectangles.Rectangle := (150,150,150,150);
   Text_In_Rectangle0 : String := "En test-sträng";
   We,He: Natural := 0;
   Font : SDL.TTF.Font;

   Color1 : SDL.Video.Palettes.Colour := (Red => 255, Green => 255, Blue => 255, Alpha => 255);

begin
   SDL.Log.Set (Category => SDL.Log.Application, Priority => SDL.Log.Debug);
   SDL.Log.Put_Debug ("initilise");

   if SDL.Initialise then
     SDL.Video.Windows.Makers.Create (Win    => W,
                                      Title  => "Rektanglar test)",
                                      X      => 100,
                                      Y      => 100,
                                      Width  => 640,
                                      Height => 480,
                                      Flags  => SDL.Video.Windows.Resizable);
     SDL.Log.Put_Debug ("window created");

     SDL.Video.Renderers.Makers.Create (Renderer, W);
     SDL.Log.Put_Debug ("Renderer created");
     Renderer.Set_Draw_Colour(Color1);

     --create and draw on surface here ?
     --open_font
     SDL.Log.Put_Debug ("Init fonts");
     SDL.TTF.Init;
     --fc-list
     SDL.Log.Put_Debug ("SDL.TTF.Was_Init " & SDL.TTF.Was_Init'Img);
     SDL.Log.Put_Debug ("open font");
     Font.Open("/usr/share/fonts/truetype/dejavu/DejaVuSans.ttf",20);
     --get surface TTF_RenderText_Blended
--     SDL.Log.Put_Debug ("Reset style ");
--     Font.Set_Style(SDL.TTF.Bold); 
--     Font.Reset_Style; 
--     SDL.Log.Put_Debug ("set style Style_Underline");
--     Font.Set_Style(SDL.TTF.Underline); 
     
     
     declare
       SA : SDL.TTF.Style_Array_Type := (others => False);
     begin
       SA := Font.Get_Styles;
     end ;
     

    SDL.Log.Put_Debug ("create surface ");
    SDL.Video.Surfaces.Fonts.Create(Surface  => Surface,
                                     Font     => Font,
                                     Color    => Color1,
                                     Text     => Text_In_Rectangle0);  
    Font.Get_Text_Size(Text   => Text_In_Rectangle0,
                       Width  => We,
                       Height => He);
                       
    Text_Rectangle0 := (Text_Rectangle0.X,
                        Text_Rectangle0.Y,
                        Text_Rectangle0.X + Interfaces.C.Int(We),
                        Text_Rectangle0.Y + Interfaces.C.Int(He));                          

                                     

    SDL.Log.Put_Debug ("create Texture ");
    SDL.Video.Textures.Makers.Create (Tex      => Texture,
                                       Renderer => Renderer,
                                       Surface  => Surface);
                                       
    SDL.Log.Put_Debug ("Destroy Surface");
    SDL.Video.Surfaces.Fonts.Destroy(Surface  => Surface);                                        
                                      
    
    Font.Debug_Print_Style;
    
    Renderer.Draw ( Rectangle => Text_Rectangle0);
    SDL.Log.Put_Debug ("Rectangle drawn by renderer");
    
    Renderer.Set_Viewport(Text_Rectangle0) ;
    Renderer.Copy ( Copy_From => Texture);
    SDL.Log.Put_Debug ("Texture copied by renderer");
         
    Renderer.Present;
    SDL.Log.Put_Debug ("Rectangle presented by renderer");
    
    SDL.Log.Put_Debug ("Wait for part 2");
    delay 5.0;
    
    
    declare
      W,H: Natural := 0;
      T : String := "Björn was here";
      Text_Rectangle : SDL.Video.Rectangles.Rectangle := (50,250,200,350);
      Texture2  : SDL.Video.Textures.Texture;
      Surface2  : SDL.Video.Surfaces.Surface;
    begin
      Renderer.Clear;
      Font.Get_Text_Size(Text   => T,
                         Width  => W,
                         Height => H);
      SDL.Log.Put_Debug ("Get_Text_Size: W/H" & W'Img & H'Img);
                         
--      Text_Rectangle := (Text_Rectangle.X,
--                         Text_Rectangle.Y,
--                         Text_Rectangle.X + Interfaces.C.Int(W),
--                         Text_Rectangle.Y + Interfaces.C.Int(H));                          
    
      SDL.Log.Put_Debug ("create surface ");
      SDL.Video.Surfaces.Fonts.Create(Surface  => Surface2,
                                      Font     => Font,
                                      Color    => Color1,
                                      Text     => T);     
                                      
      SDL.Video.Renderers.Makers.Create (Renderer, Surface2);
      SDL.Log.Put_Debug ("sized Rectangle drawn by renderer");             
    
      SDL.Log.Put_Debug ("create Texture ");
      SDL.Video.Textures.Makers.Create (Tex      => Texture2,
                                        Renderer => Renderer,
                                        Surface  => Surface2);
    
      Renderer.Draw(Rectangle => Text_Rectangle);
      Renderer.Set_Viewport(Text_Rectangle) ;
      Renderer.Copy ( Copy_From => Texture2);
      Renderer.Present;
  
    end ;
  else
    SDL.Log.Put_Debug ("initilise failed");
  end if;
  
  delay 5.0;

  SDL.Log.Put_Debug ("close Font ");
  Font.Close;
  -- close surface
  SDL.Log.Put_Debug ("Quit Font ");
  SDL.TTF.Quit;
  SDL.Log.Put_Debug ("SDL.TTF.Was_Init " & SDL.TTF.Was_Init'Img);
  
  W.Finalize;
  SDL.Log.Put_Debug ("finalised");
  SDL.Finalise;
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

   
end Rectangles;
