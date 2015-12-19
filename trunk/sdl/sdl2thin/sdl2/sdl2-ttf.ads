
--  fonts
--with  SDL2.Video.Surfaces;
--with  SDL2.Video.Palettes;
with Unchecked_Conversion;
with Interfaces; use Interfaces;
with  Ada.Finalization;

package SDL2.TTF is

  Initialize_Failed : exception;
  SDL_Call_Failed   : exception;

  procedure Init;
  function Was_Init return Boolean;
  procedure Quit;


   
  type Bom_Type is (Native, Swapped);
  for Bom_Type use (
    Native  => 16#FEFF#,
    Swapped => 16#FFFE#
  );
    
  type Style_Type is (
    Normal, 
    Bold, 
    Italic, 
    Underline, 
    Strikethrough );

  for Style_Type use  (
    Normal        => 16#00#, 
    Bold          => 16#01#, 
    Italic        => 16#02#, 
    Underline     => 16#04#, 
    Strikethrough => 16#08#);
  for Style_Type'Size use Unsigned_8'Size;  
  function Font_Style is new Unchecked_Conversion(Style_Type,  Unsigned_8);
  function Font_Style is new Unchecked_Conversion(Unsigned_8, Style_Type);
        
  type Style_Array_Type is array (Style_Type'range) of Boolean ;
  
  type Font is new Ada.Finalization.Controlled with private;
    
  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  procedure Open(Self : in out Font; Filename : in String; Point_Size : in Integer);
  
  -- void TTF SetFontStyle(TTF_Font *font, int style)  
  procedure Set_Style (Self : in out Font;  Style : Style_Type);
  procedure Set_Styles(Self : in out Font;  Style_Array : Style_Array_Type);
  
  --int TTF GetFontStyle(TTF_Font *font)  
  function Get_Styles(Self : in out Font) return Style_Array_Type;
  
  procedure Reset_Style(Self : in out Font);
  
  procedure Debug_Print_Style(Self : in out Font);
  
  procedure Get_Text_Size(Self   : in out Font;
                          Text   : in     String;
                          Width  : in out Natural;
                          Height : in out Natural) ;
  
  
  
  -- void TTF_CloseFont(TTF_Font* font );
  procedure Close(Self : in out Font);
 
  function Get_Pointer(Self : Font) return Font_Pointer;


private
  type Font is new Ada.Finalization.Controlled with record
         Pointer : Font_Pointer := null;
         Owner   : Boolean      := True;
   end record;


  
end  SDL2.TTF;
