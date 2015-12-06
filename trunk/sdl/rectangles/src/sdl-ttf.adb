
with SDL.TTF.Thin;
with SDL.Log;
with SDL.Error;
--with Interfaces; use Interfaces;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;
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
    
  procedure Reset_Style(Self : in out Font) is
  begin
    Self.Set_Style(Normal);
  end Reset_Style;    

  procedure Set_Style(Self : in out Font;  Style : in Style_Type) is
    Local_Style : Int := Int(Font_Style(Style));
  begin
    SDL.TTF.Thin.Set_Font_Style(Self.Ptr, Local_Style);
  end Set_Style;
  
  procedure Set_Styles(Self : in out Font; Style_Array : Style_Array_Type) is
    Local_Style : Int := 0;
  begin
    for i in Style_Array'range loop
      case Style_Array(i) is
        when True  => Local_Style := Local_Style + Int(Font_Style(i));
        when False => null;        
      end case;
    end loop;
    SDL.TTF.Thin.Set_Font_Style(Self.Ptr, Local_Style);
  end Set_Styles;
  
  
  
  --int TTF GetFontStyle(TTF_Font *font)  
  function Get_Styles(Self : in out Font) return Style_Array_Type is
    All_Styles_Numeric : Unsigned_8 := Unsigned_8( SDL.TTF.Thin.Get_Font_Style(Self.Ptr));
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
      when others => raise SDL_Call_Failed with SDL.Error.Get;
    end case;
    
    for i in All_Styles_Array'range loop
      SDL.Log.Put_Debug ("Font Style " & i'Img & " " & All_Styles_Array(i)'Img );    
    end loop;
    
    return All_Styles_Array;
  end Get_Styles;


  
  --------------------------------------------------------------------------------
--  --int TTF GetFontStyle(TTF_Font *font)  
--  function Get_Style(Self : in out Font) return Interfaces.C.Int is
--  begin
--    return SDL.TTF.Thin.Get_Font_Style(Self.Ptr);
--  end Get_Style;
  
  --------------------------------------------------------------------------------
  procedure Debug_Print_Style(Self : in out Font)is
  begin
    SDL.Log.Put_Debug ("Font Style " & SDL.TTF.Thin.Get_Font_Style(Self.Ptr)'Img);
  end Debug_Print_Style;
  --------------------------------------------------------------------------------
  
  --int TTF_FontHeight(const TTF_Font *font)
  function Get_Height(Self : in out Font) return Interfaces.C.Int is
  begin
    return SDL.TTF.Thin.Get_Font_Height(Self.Ptr);
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
  begin
    Result := SDL.TTF.Thin.Get_Text_Size(Font_Ptr => Self.Ptr,
                                         Text     => C_Text,
                                         W        => W,
                                         H        => H);
                                          
    Interfaces.C.Strings.Free(C_Text);
    if Result /= 0 then
      raise SDL_Call_Failed with SDL.Error.Get;    
    end if;  
    Width := Natural(W);   
    Height := Natural(H);
  
  end Get_Text_Size;
                           
                           
                           
  --------------------------------------------------------------------------------



  
end SDL.TTF;
