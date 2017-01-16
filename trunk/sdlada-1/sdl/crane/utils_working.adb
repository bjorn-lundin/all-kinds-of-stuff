with Text_Io;
--with SDL.Error;
with SDL.Types; use SDL.Types;
--with Interfaces.C.Strings;
package body Utils is

  use type SDL.Video.Surface_Ptr;
  use type SDL.Video.Palette_Ptr;
  use type SDL.Video.Surface_Flags;
--  use type Interfaces.C.Int;

  procedure Log(Who,What : in String) is
  begin
    Text_Io.Put_Line(Who & " " & What);
  end Log;
  -------------------------------------------------
    function Fastest_Flags(Flags : SDL.Video.Surface_Flags) return SDL.Video.Surface_Flags  is
       Info : SDL.Video.VideoInfo_ConstPtr;
       New_Flags : SDL.Video.Surface_Flags := Flags;
       Screen_Width  : constant :=  640; -- 640 800 1024
       Screen_Height : constant :=  480; -- 480 600  768
    begin
       --  Hardware aceleration is only used in fullscreen mode
       New_Flags := New_Flags or SDL.Video.FULLSCREEN;
 
       --  Check for various video capabilities
       Info := SDL.Video.GetVideoInfo;
       if (Info.Blit_Hw_Cc /= 0) and (Info.Blit_Fill /= 0) then
          --  We use accelerated colorkeying and color filling
          New_Flags := New_Flags or SDL.Video.HWSURFACE;
       end if;
       --  If we have enough video memory, and will use accelerated
       --  blits directly to it, then use page flipping.
       if (New_Flags and SDL.Video.HWSURFACE) = SDL.Video.HWSURFACE then
          --  Direct hardware blitting without double-buffering
          --  causes really bad flickering.
          if Info.Video_Mem > Screen_Width * Screen_Height then
             New_Flags := New_Flags or SDL.Video.DOUBLEBUF;
          else
             New_Flags := New_Flags and not SDL.Video.HWSURFACE;
          end if;
       end if;
       --  Return the flags
       return New_Flags;
    end Fastest_Flags;
-----------------------------------------------------------------------
  
  
end Utils;