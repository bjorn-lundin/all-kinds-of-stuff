
with  SDL2.Log;
with  SDL2.Error;
--with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
------------------------------------------------------------------------
package body SDL2.TTF is
  
  
  
  function Get_Pointer(Self : Font) return Font_Pointer is
  begin
    return Self.Pointer;
  end Get_Pointer;
  
  --------------------------------------------------------------------------------
  -- TTF_Font *TTF_OpenFont( const char *file, int ptsize)
  procedure Open (Self : in out Font; Filename : in String; Point_Size : in Integer) is    
   --TTF_Font *TTF_OpenFont( const char *file, int ptsize)
    function TTF_Open_Font(Filename : Chars_Ptr; Point_Size : Int) return SDL2.Font_Pointer ;
    pragma Import (C, TTF_Open_Font, "TTF_OpenFont");
    C_Filename : Chars_Ptr := New_String(Filename);   
  begin
    Self.Pointer := TTF_Open_Font(C_Filename, C.int(Point_Size));
    Free(C_Filename);
  end Open;

  --------------------------------------------------------------------------------
  -- void TTF_CloseFont(TTF_Font* font );
  procedure Close (Self : in out Font) is
    procedure TTF_Close_Font(Font_Ptr : in  SDL2.Font_Pointer);
    pragma Import (C, TTF_Close_Font, "TTF_CloseFont");
  begin
    TTF_Close_Font(Self.Pointer);
    Self.Pointer := null;
  end Close;
  
  
  --------------------------------------------------------------------------------
  procedure Init is
    function TTF_Init return Int;
    pragma Import (C, TTF_Init, "TTF_Init");
  begin
    if TTF_Init /= 0 then
      raise Initialize_Failed with SDL2.Error.Get;
    end if;
  end Init;
  
  
  --------------------------------------------------------------------------------
  function Was_Init return Boolean is
    function TTF_WasInit return Interfaces.C.Int;
    pragma Import (C, TTF_WasInit, "TTF_WasInit");
  begin
    if TTF_WasInit = 0 then
      return False;
    else 
      return True;
    end if;  
  end Was_Init;
  --------------------------------------------------------------------------------
  procedure Quit is
    procedure TTF_Quit;
    pragma Import (C, TTF_Quit, "TTF_Quit");
  begin
    TTF_Quit;
  end Quit;
  --------------------------------------------------------------------------------
    
  procedure Reset_Style(Self : in out Font) is
  begin
    Self.Set_Style(Normal);
  end Reset_Style;    
  
  --------------------------------------------------------------------------------

  procedure Set_Style(Self : in out Font;  Style : in Style_Type) is
    Local_Style : Int := Int(Font_Style(Style));
    --void TTF_SetFontStyle(TTF_Font *font, int style)
    procedure TTF_Set_Font_Style(Font_Ptr : in  SDL2.Font_Pointer;  Style : in Interfaces.C.Int) ;
    pragma Import (C, TTF_Set_Font_Style, "TTF_SetFontStyle");
  begin
     TTF_Set_Font_Style(Self.Pointer, Local_Style);
  end Set_Style;
  
  --------------------------------------------------------------------------------
  
  procedure Set_Styles(Self : in out Font; Style_Array : Style_Array_Type) is
    Local_Style : Int := 0;
    --void TTF_SetFontStyle(TTF_Font *font, int style)
    procedure TTF_Set_Font_Style(Font_Ptr : in SDL2.Font_Pointer; Style : in Interfaces.C.Int) ;
    pragma Import (C, TTF_Set_Font_Style, "TTF_SetFontStyle");
  begin
    for i in Style_Array'range loop
      case Style_Array(i) is
        when True  => Local_Style := Local_Style + Int(Font_Style(i));
        when False => null;        
      end case;
    end loop;
    TTF_Set_Font_Style(Self.Pointer, Local_Style);
  end Set_Styles;

  --------------------------------------------------------------------------------
  function Get_Styles(Self : in out Font) return Style_Array_Type is
    --int TTF GetFontStyle(TTF_Font *font)  
    function TTF_Get_Font_Style(Font_Ptr : in SDL2.Font_Pointer) return Interfaces.C.Int ;
    pragma Import (C, TTF_Get_Font_Style, "TTF_GetFontStyle");
    
    All_Styles_Numeric : Unsigned_8 := Unsigned_8(TTF_Get_Font_Style(Self.Pointer));
    All_Styles_Array   : Style_Array_Type := (others => False);   
  
  begin
    case All_Styles_Numeric is
      when 0 =>  null;
      when 1 =>  All_Styles_Array(Bold)          := True;
      
      when 2 =>  All_Styles_Array(Italic)        := True;
      
      when 3 =>  All_Styles_Array(Italic)        := True;
                 All_Styles_Array(Bold)          := True;
                 
      when 4 =>  All_Styles_Array(Underline)     := True;
      
      when 5 =>  All_Styles_Array(Underline)     := True;
                 All_Styles_Array(Bold)          := True;   
                 
      when 6 =>  All_Styles_Array(Underline)     := True;
                 All_Styles_Array(Italic)        := True;
                 
      when 7 =>  All_Styles_Array(Underline)     := True; 
                 All_Styles_Array(Italic)        := True;
                 All_Styles_Array(Bold)          := True;
                 
      when 8 =>  All_Styles_Array(Strikethrough) := True;
      
      when 9 =>  All_Styles_Array(Strikethrough) := True;  
                 All_Styles_Array(Bold)          := True;        
                 
      when 10 => All_Styles_Array(Strikethrough) := True;
                 All_Styles_Array(Italic)        := True;
                 
      when 11 => All_Styles_Array(Strikethrough) := True;
                 All_Styles_Array(Italic)        := True;
                 All_Styles_Array(Bold)          := True;
                 
      when 12 => All_Styles_Array(Strikethrough) := True;
                 All_Styles_Array(Underline)     := True;
                 
      when 13 => All_Styles_Array(Strikethrough) := True;
                 All_Styles_Array(Underline)     := True;
                 All_Styles_Array(Bold)          := True;
                 
      when 14 => All_Styles_Array(Strikethrough) := True;
                 All_Styles_Array(Underline)     := True;
                 All_Styles_Array(Italic)        := True;
                 
      when 15 => All_Styles_Array(Strikethrough) := True;
                 All_Styles_Array(Underline)     := True;
                 All_Styles_Array(Italic)        := True;
                 All_Styles_Array(Bold)          := True;
      when others => raise SDL_Call_Failed with  SDL2.Error.Get;
    end case;
    
    for i in All_Styles_Array'range loop
       SDL2.Log.Put_Debug ("Font Style " & i'Img & " " & All_Styles_Array(i)'Img );    
    end loop;
    
    return All_Styles_Array;
  end Get_Styles;


  
  --------------------------------------------------------------------------------
--  --int TTF GetFontStyle(TTF_Font *font)  
--  function Get_Style(Self : in out Font) return Interfaces.C.Int is
--  begin
--    return  SDL2.TTF.Thin.Get_Font_Style(Self.Pointer);
--  end Get_Style;
  
  --------------------------------------------------------------------------------
  procedure Debug_Print_Style(Self : in out Font)is
    function TTF_Get_Font_Style(Font_Ptr : in SDL2.Font_Pointer) return Interfaces.C.Int ;
    pragma Import (C, TTF_Get_Font_Style, "TTF_GetFontStyle");
  begin
     SDL2.Log.Put_Debug ("Font Style " &  TTF_Get_Font_Style(Self.Pointer)'Img);
  end Debug_Print_Style;
  --------------------------------------------------------------------------------
  
  --int TTF_FontHeight(const TTF_Font *font)
  function Get_Height(Self : in out Font) return Interfaces.C.Int is
    --int TTF_FontHeight(const TTF_Font *font)
    function TTF_Font_Height(Font_Ptr : in  SDL2.Font_Pointer) return Interfaces.C.Int ;
    pragma Import (C, TTF_Font_Height, "TTF_FontHeight");  
  begin
    return TTF_Font_Height(Self.Pointer);
  end Get_Height;
  --------------------------------------------------------------------------------

  
  --int TTF_SizeText(TTF_Font *font, const char *text, int *w, int *h)
  procedure Get_Text_Size(Self   : in out Font;
                          Text   : in     String;
                          Width  : in out Natural;
                          Height : in out Natural) is
    Result : Interfaces.C.Int := 0;
    C_Text : Interfaces.C.Strings.Chars_Ptr := Interfaces.C.Strings.New_String(Text);
    W,H    : Interfaces.C.Int := 0;
    
    function TTF_Size_Text(Font_Ptr : in     SDL2.Font_Pointer;
                           Text     : in     Interfaces.C.Strings.Chars_Ptr;
                           W        : in out Interfaces.C.Int;
                           H        : in out Interfaces.C.Int) return Interfaces.C.Int ;
    pragma Import (C, TTF_Size_Text, "TTF_SizeText");
    
  begin
    Result := TTF_Size_Text(Font_Ptr => Self.Pointer,
                            Text     => C_Text,
                            W        => W,
                            H        => H);
                                          
    Interfaces.C.Strings.Free(C_Text);
    if Result /= 0 then
      raise SDL_Call_Failed with SDL2.Error.Get;    
    end if;  
    Width := Natural(W);   
    Height := Natural(H);
  
  end Get_Text_Size;                        
  ------------------------------------------------------------------------------

end SDL2.TTF;
