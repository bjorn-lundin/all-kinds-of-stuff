
-- ----------------------------------------------------------------- --
--                AdaSDL                                             --
--                Thin binding to Simple Direct Media Layer          --
--                Copyright (C) 2000-2012  A.M.F.Vargas              --
--                Antonio M. M. Ferreira Vargas                      --
--                Manhente - Barcelos - Portugal                     --
--                http://adasdl.sourceforge.net                      --
--                E-mail: amfvargas@gmail.com                        --
-- ----------------------------------------------------------------- --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-- ----------------------------------------------------------------- --

--  **************************************************************** --
--  The SDL ttf library was written in C  by:                        --
--  Sam Lantinga - www.libsld.org                                    --
--  **************************************************************** --

package body SDL_ttf is

   --  ====================================
   function Open_Font (file : String; ptsize : C.int) return Font_ptr is
      File_CString : aliased C.char_array := C.To_C (file);
   begin
      return OpenFont (CS.To_Chars_Ptr (File_CString'Unchecked_Access),
                       ptsize);
   end Open_Font;


   --  ====================================
   function Open_Font_Index (file : String;
                             ptsize : C.int; index : C.long)
                             return Font_ptr is
      File_CString : aliased C.char_array := C.To_C (file);
   begin
      return OpenFontIndex (CS.To_Chars_Ptr (File_CString'Unchecked_Access),
                            ptsize,
                            index);
   end Open_Font_Index;

   --  ====================================
   function Font_Face_Family_Name (font : Font_ptr) return String is
   begin
      return CS.Value (FontFaceFamilyName (font));
   end Font_Face_Family_Name;

   --  ====================================
   function Font_Face_Style_Name (font : Font_ptr) return String is
   begin
      return CS.Value (FontFaceStyleName (font));
   end Font_Face_Style_Name;

   --  ====================================
   function Size_Text (font : Font_ptr; text : String;
                       w, h : C.int) return C.int is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return (SizeText (font, CS.To_Chars_Ptr (Text_CString'Unchecked_Access), w, h));
   end Size_Text;

   --  ====================================
   function Size_UTF8 (font : Font_ptr; text : String;
                       w, h : C.int) return C.int is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return (SizeUTF8 (font, CS.To_Chars_Ptr (Text_CString'Unchecked_Access), w, h));
   end Size_UTF8;

   --  ====================================
   function Render_Text_Solid (font : Font_ptr; text : String;
                              fg : V.Color) return V.Surface_ptr is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return RenderText_Solid (font,
                               CS.To_Chars_Ptr (Text_CString'Unchecked_Access),
                               fg);
   end Render_Text_Solid;

   --  ====================================
   function Render_UTF8_Solid (font : Font_ptr; text : String;
                              fg : V.Color) return V.Surface_ptr is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return RenderUTF8_Solid (font,
                               CS.To_Chars_Ptr (Text_CString'Unchecked_Access),
                               fg);
   end Render_UTF8_Solid;

   --  ====================================
   function Render_Text_Shaded (font : Font_ptr; text : String;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return RenderText_Shaded (font,
                                CS.To_Chars_Ptr (Text_CString'Unchecked_Access),
                                fg,
                                bg);
   end Render_Text_Shaded;

   --  ====================================
   function Render_UTF8_Shaded (font : Font_ptr; text : String;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return RenderUTF8_Shaded (font,
                                CS.To_Chars_Ptr (Text_CString'Unchecked_Access),
                                fg,
                                bg);
   end Render_UTF8_Shaded;

   --  ====================================
   function Render_Text_Blended (font : Font_ptr; text : String;
                                fg : V.Color) return V.Surface is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return RenderText_Blended (font,
                                 CS.To_Chars_Ptr (Text_CString'Unchecked_Access),
                                 fg);
   end Render_Text_Blended;

   --  ====================================
   function Render_UTF8_Blended (font : Font_ptr; text : String;
                                fg : V.Color) return V.Surface is
      Text_CString : aliased C.char_array := C.To_C (text);
   begin
      return RenderUTF8_Blended (font,
                                 CS.To_Chars_Ptr (Text_CString'Unchecked_Access),
                                 fg);
   end Render_UTF8_Blended;

   --  ====================================
   procedure Set_Font_Style (font : Font_ptr; style : TTF_STYLE) is
   begin
      SetFontStyle (font, style);
   end Set_Font_Style;

end SDL_ttf;
