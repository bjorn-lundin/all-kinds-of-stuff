
with SDL.TTF.Thin;

------------------------------------------------------------------------
package body SDL.TTF is
  
  
  
  function Ptr(Self : Font) return C_Font_Access is
  begin
    return Self.Ptr;
  end Ptr;
  
  --------------------------------------------------------------------------------
  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  procedure Open (Self : in out Font; Filename : in String; Point_Size : in Integer) is    
   --TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  begin
     Self.Ptr := SDL.TTF.Thin.Open(Filename, Point_Size);
  end Open;
  
  --------------------------------------------------------------------------------
  -- void TTF_CloseFont(TTF_Font* font );
  procedure Close (Self : in out Font) is
  begin
    SDL.TTF.Thin.Close(Self.Ptr);
  end Close;
  --------------------------------------------------------------------------------
  procedure Init is
  begin
    SDL.TTF.Thin.Init;
  end Init;
  --------------------------------------------------------------------------------

  function Was_Init return Boolean is
    use type Interfaces.C.Int;
  begin
    if SDL.TTF.Thin.Was_Init = 0 then
      return False;
    else 
      return True;
    end if;  
  end Was_Init;
  --------------------------------------------------------------------------------
  procedure Quit is
  begin
    SDL.TTF.Thin.Quit;
  end Quit;
  --------------------------------------------------------------------------------
    
  procedure Set_Style(Self : in out Font;  Style : in Interfaces.C.Int) is
  begin
    SDL.TTF.Thin.Set_Font_Style(Self.Ptr, Style);
  end Set_Style;
  
  --------------------------------------------------------------------------------
  
  
end SDL.TTF;
