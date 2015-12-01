
with SDL;
with SDL.Error;
with SDL.Log;
with SDL.Video.Palettes;
with SDL.Video.Pixel_Formats;
with SDL.Video.Pixels;
with SDL.Video.Renderers.Makers;
with SDL.Video.Textures.Makers;
with SDL.Video.Windows.Makers;
with SDL.Versions;
with SDL.Video.Rectangles;


procedure Rectangles is

   W                : SDL.Video.Windows.Window;
   Renderer         : SDL.Video.Renderers.Renderer;
   Texture          : SDL.Video.Textures.Texture;
   Pixels           : SDL.Video.Pixels.ARGB_8888_Access.Pointer;
   Pitches          : SDL.Video.Pixels.Pitch_Access.Pointer;
   W_Size           : SDL.Video.Windows.Sizes := (640, 480);
   
   Rectangle1       : SDL.Video.Rectangles.Rectangle := (50,50,50,50);

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

     SDL.Video.Textures.Makers.Create (Tex      => Texture,
                                       Renderer => Renderer,
                                       Format   => SDL.Video.Pixel_Formats.Pixel_Format_ARGB_8888,
                                       Kind     => SDL.Video.Textures.Streaming,
                                       Size     => W_Size);
     SDL.Log.Put_Debug ("Texture created");          
     
     
     Renderer.Draw ( Rectangle => Rectangle1);
     SDL.Log.Put_Debug ("Rectangle drawn by rendered");     
          
     Renderer.Present;
     SDL.Log.Put_Debug ("Rectangle presented by renderer");     

                                   
                                        
   else
     SDL.Log.Put_Debug ("initilise failed");                                        
   end if;
   delay 5.0;
   W.Finalize;
   SDL.Log.Put_Debug ("finalised");                                        
   SDL.Finalise;
end Rectangles;
