
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
--  This is an Ada binding to SDL - Simple DirectMedia Layer from    --
--  Sam Lantinga - www.libsld.org                                    --
--  **************************************************************** --
--  In order to help the Ada programmer, the comments in this file   --
--  are, in great extent, a direct copy of the original text in the  --
--  SDL header files.                                                --
--  **************************************************************** --
--with Lib_C;
package body SDL.Video is
   use type CS.chars_ptr;

   --  Mode strings.
   --  "rb"
   Mode_RB     : aliased  C.char_array := C.To_C ("rb");
   Mode_RB_Ptr : constant C.Strings.chars_ptr := C.Strings.To_Chars_Ptr (Mode_RB'Access);
   --  "wb"
   Mode_WB     : aliased  C.char_array := C.To_C ("wb");
   Mode_WB_Ptr : constant C.Strings.chars_ptr := C.Strings.To_Chars_Ptr (Mode_WB'Access);

   -----------------
   --  Set_Colors --
   -----------------

   function Set_Colors (
      surface    : Surface_ptr;
      colors     : Colors_Array)
      return C.int
   is
   begin
      return SetColors (surface, colors,
                        0, C.int (colors'Length - 1));
   end Set_Colors;

   procedure Set_Colors (
      surface    : Surface_ptr;
      colors     : Colors_Array)
   is
   begin
      SetColors (surface, colors,
                 0, C.int (colors'Length - 1));
   end Set_Colors;

   -------------
   -- LoadBMP --
   -------------

   function LoadBMP (file : C.Strings.chars_ptr) return Surface_ptr is
   begin
      return LoadBMP_RW (
                src     => SDL.RWops.RWFromFile (
                              file => file,
                              mode => Mode_RB_Ptr),
                freesrc => 1);
   end LoadBMP;

   --  ===================================================================
   function LoadBMP (file : String) return Surface_ptr is
      File_CString : aliased C.char_array := C.To_C (file);
   begin
      return LoadBMP_RW (
                src     => SDL.RWops.RWFromFile (
                              file => C.Strings.To_Chars_Ptr (File_CString'Unchecked_Access),
                              mode => Mode_RB_Ptr),
                freesrc => 1);
   end LoadBMP;


   -------------
   -- SaveBMP --
   -------------

   function SaveBMP
     (surface : Surface_ptr;
      file    : C.Strings.chars_ptr)
      return C.int
   is
   begin
      return SaveBMP_RW (
                surface => surface,
                dst     => SDL.RWops.RWFromFile (
                              file => file,
                              mode => Mode_WB_Ptr),
                freedst => 1);
   end SaveBMP;

   -------------
   -- SaveBMP --
   -------------

   procedure SaveBMP
     (surface : Surface_ptr;
      file    : C.Strings.chars_ptr)
   is
      dummy : C.int;
   begin
      dummy := SaveBMP (surface, file);
   end SaveBMP;

   -------------
   -- Save_BMP --
   -------------
   function Save_BMP (
      surface : Surface_ptr;
      file    : String)
      return C.int
   is
      File_CString : aliased C.char_array := C.To_C (file);
   begin
      return SaveBMP (surface, CS.To_Chars_Ptr (File_CString'Unchecked_Access));
   end Save_BMP;

   -------------
   -- Save_BMP --
   -------------
   procedure Save_BMP (
      surface : Surface_ptr;
      file    : String)
   is
      File_CString : aliased C.char_array := C.To_C (file);
      dummy : C.int;
   begin
      dummy := SaveBMP (surface, CS.To_Chars_Ptr (File_CString'Unchecked_Access));
   end Save_BMP;

   --------------
   -- MUSTLOCK --
   --------------

   function MUSTLOCK (surface : Surface_ptr) return Boolean is
   begin
      return (surface.offset /= 0 or
             ((surface.flags and
                (HWSURFACE or ASYNCBLIT or RLEACCEL)) /= 0));
   end MUSTLOCK;


   --  ======================================
   procedure Disable_Clipping (surface : Surface_ptr) is
   begin
      SetClipRect (surface, null);
   end Disable_Clipping;

   --  ======================================
   procedure Update_Rect (
      screen   : Surface_ptr;
      the_rect : Rect)
   is
   begin
      UpdateRect (screen,
                  Sint32 (the_rect.x),
                  Sint32 (the_rect.y),
                  Uint32 (the_rect.w),
                  Uint32 (the_rect.h));
   end Update_Rect;

   --  ======================================
   procedure WM_Set_Caption (
      title : in String;
      icon  : in String)
   is
      Title_CString : aliased C.char_array := C.To_C (title);
      Icon_CString  : aliased C.char_array := C.To_C (icon);
   begin
      WM_SetCaption (CS.To_Chars_Ptr (Title_CString'Unchecked_Access),
                     CS.To_Chars_Ptr (Icon_CString'Unchecked_Access));
   end WM_Set_Caption;

   --  ======================================
   procedure WM_Set_Caption_Title (title : in String)
   is
      Title_CString : aliased C.char_array := C.To_C (title);
   begin
      WM_SetCaption (CS.To_Chars_Ptr (Title_CString'Unchecked_Access),
                     CS.Null_Ptr);
   end WM_Set_Caption_Title;

   --  ======================================
   procedure WM_Set_Caption_Icon (icon  : in String)
   is
      Icon_CString : aliased C.char_array := C.To_C (icon);
   begin
      WM_SetCaption (CS.Null_Ptr,
                     CS.To_Chars_Ptr (Icon_CString'Unchecked_Access));
   end WM_Set_Caption_Icon;

   --  ======================================
   procedure WM_Get_Caption (
      title : out US.Unbounded_String;
      icon  : out US.Unbounded_String)
   is
      the_title : aliased CS.chars_ptr := CS.Null_Ptr;
      the_icon  : aliased CS.chars_ptr := CS.Null_Ptr;
   begin
      WM_GetCaption (the_title'Unchecked_Access, the_icon'Unchecked_Access);
      if the_title /= CS.Null_Ptr then
         title := US.To_Unbounded_String (CS.Value (the_title));
      else
         title := US.Null_Unbounded_String;
      end if;
      if the_icon /= CS.Null_Ptr then
         icon  := US.To_Unbounded_String (CS.Value (the_icon));
      else
         icon := US.Null_Unbounded_String;
      end if;
   end WM_Get_Caption;

   --  ======================================
   procedure WM_Get_Caption_Title (title : out US.Unbounded_String)
   is
      the_title : aliased CS.chars_ptr := CS.Null_Ptr;
   begin
      WM_GetCaption (the_title'Unchecked_Access, null);
      if the_title /= CS.Null_Ptr then
         title := US.To_Unbounded_String (CS.Value (the_title));
      else
         title := US.Null_Unbounded_String;
      end if;
   end WM_Get_Caption_Title;

   --  ======================================
   procedure WM_Get_Caption_Icon (icon  : out US.Unbounded_String)
   is
      the_icon  : aliased CS.chars_ptr := CS.Null_Ptr;
   begin
      WM_GetCaption (null, the_icon'Unchecked_Access);
      if the_icon /= CS.Null_Ptr then
         icon := US.To_Unbounded_String (CS.Value (the_icon));
      else
         icon := US.Null_Unbounded_String;
      end if;
   end WM_Get_Caption_Icon;

end SDL.Video;

