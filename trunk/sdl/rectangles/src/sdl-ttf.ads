
--  fonts
with SDL.Video.Surfaces;
with SDL.Video.Palettes;
with Interfaces.C;

package SDL.TTF is

  Initialize_Failed : exception;

  procedure Init;
  function Was_Init return Boolean;
  procedure Quit;


   
  type Bom_Type is (Native, Swapped);
  for Bom_Type use (
    Native  => 16#FEFF#,
    Swapped => 16#FFFE#
  );
 
  Style_Normal        : constant Interfaces.C.Int := 16#00#; 
  Style_Bold          : constant Interfaces.C.Int := 16#01#; 
  Style_Italic        : constant Interfaces.C.Int := 16#02#; 
  Style_Underline     : constant Interfaces.C.Int := 16#04#; 
  Style_Strikethrough : constant Interfaces.C.Int := 16#08#;

  
  
--  type Font_Access ;
  
  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  type Font is tagged private;
  type C_Font_Access is access Interfaces.C.Int;
  pragma Convention (C, C_Font_Access);
  
  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  procedure Open(Self : in out Font; Filename : in String; Point_Size : in Integer);
  
  -- void TTF SetFontStyle(TTF_Font *font, int style)  
  procedure Set_Style(Self : in out Font;  Style : in Interfaces.C.Int);
    
  
  -- void TTF_CloseFont(TTF_Font* font );
  procedure Close(Self : in out Font);
  
  function Ptr(Self : Font) return C_Font_Access;
  
  type Font_Access is access Font;

  
--SDL_Surface *TTF_RenderText_Solid
--(
--    TTF_Font *font,
--    const char *text,
--    SDL_Color fg
--)

  
private

  type Font is tagged record 
    Ptr : C_Font_Access := null;
  end record;


  
end SDL.TTF;
