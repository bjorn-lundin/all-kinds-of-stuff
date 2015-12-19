
with Interfaces.C.Strings;


package body SDL2.Error is

  function Get return String is
    use Interfaces.C.Strings;
    function SDL_Get_Error return Chars_Ptr ;
    pragma Import(C, SDL_Get_Error, "SDL_GetError");
    C_Str : Chars_Ptr := Sdl_Get_Error;
  begin
     return Value(C_Str);
  end Get;


end SDL2.Error;
