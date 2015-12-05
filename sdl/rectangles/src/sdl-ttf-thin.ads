with Interfaces.C;


with SDL.TTF;

package SDL.TTF.Thin is

  --int TTF Init()
  procedure Init;
  -- int TTF_WasInit()  
  function Was_Init return Interfaces.C.Int;
  -- void TTF_Quit()
  procedure Quit;


  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  function Open(Filename : String; Point_Size : Integer) return SDL.TTF.C_Font_Access;
  
  -- void TTF_CloseFont(TTF_Font* font );
  procedure Close(Font_Ptr : in out SDL.TTF.C_Font_Access);

  --void TTF_SetFontStyle(TTF_Font *font, int style)
  procedure Set_Font_Style(Font_Ptr : in SDL.TTF.C_Font_Access;  Style : in Interfaces.C.Int) ;
  
  
  
private
  pragma Import (C, Was_Init, "TTF_WasInit");
  pragma Import (C, Quit, "TTF_Quit");
  pragma Import (C, Set_Font_Style, "TTF_SetFontStyle");

end SDL.TTF.Thin;

