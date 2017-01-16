
with SDL.Video;
--with Interfaces.C;
package Utils is

  procedure Log(Who,What : in String);
  function Fastest_Flags(Flags  : SDL.Video.Surface_Flags) return SDL.Video.Surface_Flags;

end Utils;