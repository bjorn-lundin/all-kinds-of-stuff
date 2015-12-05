with Ada.Command_Line;
with Ada.Exceptions;

with SDL;
with SDL.Error;
with SDL.Log;
with SDL.Video.Palettes;
--with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
--with SDL.Versions;
with SDL.Video.Rectangles;
with SDL.Video.Surfaces;
with SDL.TTF;
with SDL.Video.Surfaces.Fonts;
with Stacktrace;

procedure Rectangles is

   W                : SDL.Video.Windows.Window;
   Renderer         : SDL.Video.Renderers.Renderer;
   Texture          : SDL.Video.Textures.Texture;
 --  Pixels           : SDL.Video.Pixels.ARGB_8888_Access.Pointer;
 --  Pitches          : SDL.Video.Pixels.Pitch_Access.Pointer;
 --  W_Size           : SDL.Video.Windows.Sizes := (640, 480);

   Surface  :  SDL.Video.Surfaces.Surface;

   Rectangle : SDL.Video.Rectangles.Rectangle := (50,50,50,50);
   Text_Rectangle0 : SDL.Video.Rectangles.Rectangle := (150,150,150,150);
   Text_Rectangle1 : SDL.Video.Rectangles.Rectangle := (250,50,50,50);

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

     --create and draw on surface here ?
     --open_font
     SDL.Log.Put_Debug ("Init fonts");
     SDL.TTF.Init;
     --fc-list
     SDL.Log.Put_Debug ("SDL.TTF.Was_Init " & SDL.TTF.Was_Init'Img);
     SDL.Log.Put_Debug ("open font");
     Font.Open("/usr/share/fonts/truetype/freefont/FreeSansOblique.ttf",20);
     --get surface TTF_RenderText_Blended
     SDL.Log.Put_Debug ("set style Style_Underline");
     Font.Set_Style(SDL.TTF.Style_Underline); 

     SDL.Log.Put_Debug ("create surface ");
     SDL.Video.Surfaces.Fonts.Create(Surface  => Surface,
                                     Font     => Font,
                                     Color    => Color1,
                                     Text     => "First out");                                                                        

     SDL.Log.Put_Debug ("create Texture ");
     SDL.Video.Textures.Makers.Create (Tex      => Texture,
                                       Renderer => Renderer,
                                       Surface  => Surface);

     SDL.Log.Put_Debug ("Texture created");
     SDL.Log.Put_Debug ("close Font ");
     Font.Close;
     -- close surface
     SDL.Log.Put_Debug ("Quit Font ");
     SDL.TTF.Quit;
     SDL.Log.Put_Debug ("SDL.TTF.Was_Init " & SDL.TTF.Was_Init'Img);

     Renderer.Draw ( Rectangle => Rectangle);
     SDL.Log.Put_Debug ("Rectangle drawn by renderer");

     Renderer.Set_Viewport(Text_Rectangle0) ;
     Renderer.Copy ( Copy_From => Texture);
     SDL.Log.Put_Debug ("Texture copied by renderer");
          
     Renderer.Present;
     SDL.Log.Put_Debug ("Rectangle presented by renderer");

   else
     SDL.Log.Put_Debug ("initilise failed");
   end if;
   delay 5.0;




   W.Finalize;
   SDL.Log.Put_Debug ("Sdl.TTf.Bom_Type'first'img " & Sdl.TTf.Bom_Type'first'img );
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
