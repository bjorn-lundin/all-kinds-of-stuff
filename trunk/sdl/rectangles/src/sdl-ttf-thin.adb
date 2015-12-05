with Interfaces.C.Strings;
with SDL.Error;
with SDL.Log;


package body SDL.TTF.Thin is

--int TTF Init()

  --------------------------------------------------------------------------------
  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  function Open(Filename : String; Point_Size : Integer) return SDL.TTF.C_Font_Access is    
    use Interfaces.C;
    use Interfaces.C.Strings;  
   --Font *TTF_OpenFont( const char *file, int ptsize)
    Local_Font_Access : SDL.TTF.C_Font_Access := null;
    function C_Open(Filename : Chars_Ptr; Point_Size : Int ) return SDL.TTF.C_Font_Access ;
    pragma Import (C, C_Open, "TTF_OpenFont");
    C_Filename : Chars_Ptr := New_String(Filename);
  begin
    Local_Font_Access := C_Open(C_Filename, Int(Point_Size));
    Free(C_Filename);
    return Local_Font_Access;
  end Open;
  
  --------------------------------------------------------------------------------
  -- void TTF_CloseFont(TTF_Font* font );
  procedure Close(Font_Ptr : in out SDL.TTF.C_Font_Access) is
    procedure C_Close(Font_Ptr : in SDL.TTF.C_Font_Access);
    pragma Import (C, C_Close, "TTF_CloseFont");
  begin
   -- pragma Compile_Time_Warning(True, "why crash here ?");
    C_Close(Font_Ptr);
    Font_Ptr := null;
  end Close;
  --------------------------------------------------------------------------------
  
  
  
  procedure Init is
    use Interfaces.C;
    function C_Init return Int;
    pragma Import (C, C_Init, "TTF_Init");
  begin
    if C_Init /= 0 then
      raise SDL.TTF.Initialize_Failed with SDL.Error.Get;
    end if;
  end Init;
  --------------------------------------------------------------------------------

  

end SDL.TTF.Thin;

