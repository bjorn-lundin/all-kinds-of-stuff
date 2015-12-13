
with SDL2.Thin;
with Interfaces.C.Strings; use Interfaces.C.Strings;


package body SDL2.Error is

  function Get return String is
    C_Str : Chars_Ptr := SDL2.Thin.SDL_Get_Error;
  begin
     return Value (C_Str);
  end Get;


end SDL2.Error;
