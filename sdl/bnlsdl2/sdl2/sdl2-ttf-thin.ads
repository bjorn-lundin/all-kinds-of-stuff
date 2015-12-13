with Interfaces.C;
with Interfaces.C.Strings;


with  SDL2.TTF;

package  SDL2.TTF.Thin is

  --int TTF Init()
  procedure Init;
  -- int TTF_WasInit()  
  function Was_Init return Interfaces.C.Int;
  -- void TTF_Quit()
  procedure Quit;


  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  function Open(Filename : String; Point_Size : Integer) return  SDL2.TTF.C_Font_Access;
  
  -- void TTF_CloseFont(TTF_Font* font );
  procedure Close(Font_Ptr : in out  SDL2.TTF.C_Font_Access);

  --void TTF_SetFontStyle(TTF_Font *font, int style)
  procedure Set_Font_Style(Font_Ptr : in  SDL2.TTF.C_Font_Access;  Style : in Interfaces.C.Int) ;
  
  --int TTF GetFontStyle(TTF_Font *font)  
  function Get_Font_Style(Font_Ptr : in  SDL2.TTF.C_Font_Access) return Interfaces.C.Int ;
  
  --int TTF_FontHeight(const TTF_Font *font)
  function Get_Font_Height(Font_Ptr : in  SDL2.TTF.C_Font_Access) return Interfaces.C.Int ;
  
  --int TTF_SizeText(TTF_Font *font, const char *text, int *w, int *h)
  function Get_Text_Size(Font_Ptr : in      SDL2.TTF.C_Font_Access;
                         Text     : in     Interfaces.C.Strings.Chars_Ptr;
                         W        : in out Interfaces.C.Int;
                         H        : in out Interfaces.C.Int) return Interfaces.C.Int ;

  
private
  pragma Import (C, Was_Init, "TTF_WasInit");
  pragma Import (C, Quit, "TTF_Quit");
  pragma Import (C, Set_Font_Style, "TTF_SetFontStyle");
  pragma Import (C, Get_Font_Style, "TTF_GetFontStyle");
  pragma Import (C, Get_Font_Height, "TTF_FontHeight");
  pragma Import (C, Get_Text_Size, "TTF_SizeText");

end  SDL2.TTF.Thin;

