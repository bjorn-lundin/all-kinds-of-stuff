
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

with System;
with Interfaces.C.Strings;
with SDL.Types; use SDL.Types;
with SDL.Video;
with SDL.Error;

package SDL_ttf is

   package C  renames Interfaces.C;
   package CS renames Interfaces.C.Strings;
   package V  renames SDL.Video;
   package Er renames SDL.Error;

   --  Reference to an internal structure containing font information.
   type Font_ptr is new System.Address;

   null_Font_ptr : constant Font_ptr := Font_ptr (System.Null_Address);


   --  Initialize the TTF engine - returns 0 if successful, -1 on error
   function Init return C.int;
   pragma Import (C, Init, "TTF_Init");

   --  Open a font file and create a font of the specified point size
   function OpenFont (file : CS.chars_ptr; ptsize : C.int) return Font_ptr;
   pragma Import (C, OpenFont, "TTF_OpenFont");

   function Open_Font (file : String; ptsize : C.int) return Font_ptr;
   pragma Inline (Open_Font);

   function OpenFontIndex (file : CS.chars_ptr;
                           ptsize : C.int;
                           index : C.long)
                           return Font_ptr;
   pragma Import (C, OpenFontIndex, "TTF_OpenFontIndex");

   function Open_Font_Index (file : String;
                             ptsize : C.int;
                             index : C.long)
                             return Font_ptr;
   pragma Inline (Open_Font_Index);

   type TTF_STYLE is mod 2**8;
   for TTF_STYLE'Size use C.int'Size;

   --  Set and retrieve the font style
   --  This font style is implemented by modifying the
   --  font glyphs, and doesn't reflect any inherent
   --  properties of the truetype font file.

   TTF_STYLE_NORMAL    : constant TTF_STYLE := 16#00#;
   TTF_STYLE_BOLD      : constant TTF_STYLE := 16#01#;
   TTF_STYLE_ITALIC    : constant TTF_STYLE := 16#02#;
   TTF_STYLE_UNDERLINE : constant TTF_STYLE := 16#04#;

   function GetFontStyle (font : Font_ptr) return TTF_STYLE;
   pragma Import (C, GetFontStyle, "TTF_GetFontStyle");

   procedure SetFontStyle (font : Font_ptr; style : TTF_STYLE);
   pragma Import (C, SetFontStyle, "TTF_SetFontStyle");

   procedure Set_Font_Style (font : Font_ptr; style : TTF_STYLE);
   pragma Inline (Set_Font_Style);

   --  Get the total height of the font - usually equal to
   --  point size.
   function FontHeight (font : Font_ptr) return C.int;
   pragma Import (C, FontHeight, "TTF_FontHeight");

   --  Get the offset from the baseline to the top of the
   --  font. This is a positive value, relative to the
   --  baseline.
   function FontAscent (font : Font_ptr) return C.int;
   pragma Import (C, FontAscent, "TTF_FontAscent");

   --  Get the offset from the baseline to the botton of
   --  the font. This is a negative value, relative to the
   --  baseline.
   function FontDescent (font : Font_ptr) return C.int;
   pragma Import (C, FontDescent, "TTF_FontDescent");

   --  Get the recommended spacing between lines of text
   --  for this font.
   function FontLineSkip (font : Font_ptr) return C.int;
   pragma Import (C, FontLineSkip, "TTF_FontLineSkip");

   --  Get the number of faces of the font.
   function FontFaces (font : Font_ptr) return C.long;
   pragma Import (C, FontFaces, "TTF_FontFaces");

   --  Get the font face attributes, if any.
   function FontFaceIsFixedWidth (font : Font_ptr) return C.int;
   pragma Import (c, FontFaceIsFixedWidth, "TTF_FontFaceIsFixedWidth");

   function FontFaceFamilyName (font : Font_ptr) return CS.chars_ptr;
   pragma Import (C, FontFaceFamilyName, "TTF_FontFaceFamilyName");

   function Font_Face_Family_Name (font : Font_ptr) return String;
   pragma Inline (Font_Face_Family_Name);

   function FontFaceStyleName (font : Font_ptr) return CS.chars_ptr;
   pragma Import (C, FontFaceStyleName, "TTF_FontFaceStyleName");

   function Font_Face_Style_Name (font : Font_ptr) return String;
   pragma Inline (Font_Face_Style_Name);

   --  Get the metrics (dimensions) of a glyph.
   function GlyphMetrics (font : Font_ptr;
                          ch : Uint16;
                          minx, maxx : int_ptr;
                          miny, maxy : int_ptr;
                          advance    : int_ptr)
                          return C.int;
   pragma Import (C, GlyphMetrics, "TTF_GlyphMetrics");

   --  Get the dimensions of a rendered string of text.
   function SizeText (font : Font_ptr; text : CS.chars_ptr;
                      w, h : C.int) return C.int;
   pragma Import (C, SizeText, "TTF_SizeText");

   function Size_Text (font : Font_ptr; text : String;
                       w, h : C.int) return C.int;
   pragma Inline (Size_Text);

   function SizeUTF8 (font : Font_ptr; text : CS.chars_ptr;
                      w, h : C.int) return C.int;
   pragma Import (C, SizeUTF8, "TTF_SizeUTF8");

   function Size_UTF8 (font : Font_ptr; text : String;
                       w, h : C.int) return C.int;
   pragma Inline (Size_UTF8);

   function SizeUNICODE (font : Font_ptr; text : Uint16_ptr;
                      w, h : C.int) return C.int;
   pragma Import (C, SizeUNICODE, "TTF_SizeUNICODE");

   --  Create an 8-bit palettized surface and render the given text
   --  at fast quality with the given font and color. The 0 pixel is
   --  the colorkey, giving a transparent background, and the 1 pixel
   --  set to the text color.
   --  This function returns the new surface, or NULL if there was an
   --  error.
   function RenderText_Solid (font : Font_ptr; text : CS.chars_ptr;
                              fg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderText_Solid, "TTF_RenderText_Solid");

   function Render_Text_Solid (font : Font_ptr; text : String;
                              fg : V.Color) return V.Surface_ptr;
   pragma Inline (Render_Text_Solid);

   function RenderUTF8_Solid (font : Font_ptr; text : CS.chars_ptr;
                              fg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderUTF8_Solid, "TTF_RenderUTF8_Solid");

   function Render_UTF8_Solid (font : Font_ptr; text : String;
                              fg : V.Color) return V.Surface_ptr;
   pragma Inline (Render_UTF8_Solid);

   function RenderUNICODE_Solid (font : Font_ptr; text : Uint16_ptr;
                              fg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderUNICODE_Solid, "TTF_RenderUNICODE_Solid");

   --  Create an 8-bit palettized surface and render the given glyph at
   --  fast quality with the given font and color. The 0 pixel is the
   --  colorkey, giving a transparent background, and the 1 pixel is
   --  set to the text color. The glyph is rendered without any padding
   --  or centering in the X direction, and aligned normally in the Y
   --  direction. This function returns the new surface, of NULL if there
   --  was an error.
   function RenderGlyph_Solid (font : Font_ptr;
                               ch : Uint16; fg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderGlyph_Solid, "TTF_RenderGlyph_Solid");


   --  Create an 8-bit palettized surface and render the given text at
   --  high quality with the given font and colors. The 0 pixel is
   --  background, while other pixels have varying degrees of the
   --  foreground color. This function returns the new surface, or NUll if
   --  there was an error.
   function RenderText_Shaded (font : Font_ptr; text : CS.chars_ptr;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderText_Shaded, "TTF_RenderText_Shaded");


   function Render_Text_Shaded (font : Font_ptr; text : String;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr;
   pragma Inline (Render_Text_Shaded);

   function RenderUTF8_Shaded (font : Font_ptr; text : CS.chars_ptr;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderUTF8_Shaded, "TTF_RenderUTF8_Shaded");

   function Render_UTF8_Shaded (font : Font_ptr; text : String;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr;
   pragma Inline (Render_UTF8_Shaded);

   function RenderUNICODE_Shaded (font : Font_ptr; text : Uint16_ptr;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderUNICODE_Shaded, "TTF_RenderUNICODE_Shaded");

   --  Create an 8-bit palettized surface and render the given glyph at
   --  high quality with the given font and colors. The 0 pixel is the
   --  background while other pixels have varying degrees of th foreground color.
   --  The glyph is rendered without any padding or centering in the X
   --  direction, and aligned normally in the Y direction.
   --  This function returns the new surface, or NULL if there was an error.
   function RenderGlyph_Shaded (font : Font_ptr; ch : Uint16;
                                fg : V.Color; bg : V.Color) return V.Surface_ptr;
   pragma Import (C, RenderGlyph_Shaded, "TTF_RenderGlyph_Shaded");

   --  Create a 32-bit ARGB surface and render the given text at high quality,
   --  using alpha blending to dither the font with the given clor.
   --  This function returns the new surface, or NULL if there was an error.
   function RenderText_Blended (font : Font_ptr; text : CS.chars_ptr;
                                fg : V.Color) return V.Surface;
   pragma Import (C, RenderText_Blended, "TTF_RenderText_Blended");

   function Render_Text_Blended (font : Font_ptr; text : String;
                                fg : V.Color) return V.Surface;
   pragma Inline (Render_Text_Blended);

   function RenderUTF8_Blended (font : Font_ptr; text : CS.chars_ptr;
                                fg : V.Color) return V.Surface;
   pragma Import (C, RenderUTF8_Blended, "TTF_RenderUTF8_Blended");

   function Render_UTF8_Blended (font : Font_ptr; text : String;
                                fg : V.Color) return V.Surface;
   pragma Inline (Render_UTF8_Blended);

   function RenderUNICODE_Blended (font : Font_ptr; text : Uint16_ptr;
                                fg : V.Color) return V.Surface;
   pragma Import (C, RenderUNICODE_Blended, "TTF_RenderUNICODE_Blended");


   --  Create a 32-bit ARGB surface and render the given glyph at high quality
   --  using alpha blending to dither the font with the given color.
   --  The glyph is rendered without any padding or centering in the X
   --  direction, and aligned normally in the Y direction.
   --  This function returns the new surface, or NULL if there was an error.
   function RenderGlyph_Blended (font : Font_ptr; ch : Uint16; fg : V.Color)
                                 return V.Surface;
   pragma Import (C, RenderGlyph_Blended, "TTF_RenderGlyph_Blended");

   --  For compatibility with previous versions, here are the old functions.
   function RenderText (font : Font_ptr; text : CS.chars_ptr;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr
            renames RenderText_Shaded;

   function Render_Text (font : Font_ptr; text : String;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr
            renames Render_Text_Shaded;


   function RenderUTF8 (font : Font_ptr; text : CS.chars_ptr;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr
            renames RenderUTF8_Shaded;

   function Render_UTF8 (font : Font_ptr; text : String;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr
            renames Render_UTF8_Shaded;


   function RenderUNICODE (font : Font_ptr; text : Uint16_ptr;
                        fg : V.Color; bg : V.Color) return V.Surface_ptr
            renames RenderUNICODE_Shaded;

   --  Close an opened font file.
   procedure CloseFont (font : Font_ptr);
   pragma Import (C, CloseFont, "TTF_CloseFont");

   --  De-initialize the TTF engine.
   procedure TTF_Quit;
   pragma Import (C, TTF_Quit, "TTF_Quit");

   --  We'll use SDL for reporting errors.
   procedure SetError (fmt : CS.chars_ptr)
             renames Er.SetError;

   procedure Set_Error (fmt : String)
             renames Er.Set_Error;

   function GetError return CS.chars_ptr
            renames Er.GetError;

   function Get_Error return String
            renames Er.Get_Error;
end SDL_ttf;
