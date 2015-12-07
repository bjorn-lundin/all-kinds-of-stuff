
with SDL.Video.Surfaces;
with SDL.Video.Palettes;
with SDL.TTF;

package SDL.Video.Surfaces.Fonts is

  procedure Create(Surface  : in out SDL.Video.Surfaces.Surface;
                   Font     : in     SDL.TTF.Font;
                   Color    : in     SDL.Video.Palettes.Colour ;
                   Text     : in     String );
 
-- Finalize will do this 
--  procedure Destroy(Surface  : in out SDL.Video.Surfaces.Surface);
  --SDL_FreeSurface                   
                   
end SDL.Video.Surfaces.Fonts ;



 