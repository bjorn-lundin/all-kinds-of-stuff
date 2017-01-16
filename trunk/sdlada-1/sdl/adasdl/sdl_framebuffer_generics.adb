
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

--  ##########################################################################
--  ###   These are new extensions to the SDL API in order to improve the
--  ###   Ada code and to isolate the pointer arithmetic inside the library.
--  ##########################################################################

with Lib_C;
package body SDL_Framebuffer_Generics is

   --  ===========================================================
   --                   Go_XY_Generic
   --  ===========================================================
   function Go_XY_Generic (
      Surface : Vd.Surface_ptr;
      X       : Natural;
      Y       : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
      use Uint_Ptrs;
   begin
      if X < Natural (Surface.h) and Y < Natural (Surface.w) then
         return Uint_Ptrs.Object_Pointer (
            Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels))
            + C.ptrdiff_t (
                 Y * Natural (Surface.pitch) / Natural (Surface.format.BytesPerPixel)
                 + X));
      else
         return Uint_Ptrs.Object_Pointer (
            Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels)));
      end if;
   end Go_XY_Generic;

   --  ===========================================================
   --                   Go_XY_Unchecked_Generic
   --  ===========================================================
   function Go_XY_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      X       : Natural;
      Y       : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
      use Uint_Ptrs;
   begin
      return Uint_Ptrs.Object_Pointer (
         Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels))
         + C.ptrdiff_t (
              Y * Natural (Surface.pitch) / Natural (Surface.format.BytesPerPixel)
              + X));


   end Go_XY_Unchecked_Generic;

   --  ===========================================================
   --                 Get_Framebuffer_Generic
   --  ===========================================================
   function Get_Framebuffer_Generic (
      Surface : Vd.Surface_ptr) return Uint_Ptrs.Object_Pointer
   is
   begin
      return Uint_Ptrs.To_Pointer (Surface.pixels);
   end Get_Framebuffer_Generic;

   --  ===========================================================
   --                     Goto_Line_Generic
   --  ===========================================================
   function Goto_Line_Generic (
      Surface     : Vd.Surface_ptr;
      Line_Num    : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
      use type C.int;
   begin
      if Line_Num > Natural (Surface.h - 1) then
         return Uint_Ptrs.Object_Pointer (
            Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels))
            + C.ptrdiff_t (Line_Num * Natural (Surface.pitch)));
      else
         return Uint_Ptrs.Object_Pointer (
            Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels)));
      end if;
   end Goto_Line_Generic;

   --  ===================================================================
   --                   Goto_Line_Unchecked_Generic
   --  ===================================================================
   function Goto_Line_Unchecked_Generic (
      Surface  : Vd.Surface_ptr;
      Line_Num : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
   begin
      return Uint_Ptrs.Object_Pointer (
         Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels))
         + C.ptrdiff_t (Line_Num * Natural (Surface.pitch)));
   end Goto_Line_Unchecked_Generic;

   --  ===========================================================
   --             Goto_Line_End_Unchecked_Generic
   --  ===========================================================
   function Goto_Line_End_Unchecked_Generic (
      Surface  : Vd.Surface_ptr;
      Line_Num : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
      use type C.int;
   begin
      return Uint_Ptrs.Object_Pointer (
         Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels))
         + C.ptrdiff_t (Line_Num * Natural (Surface.pitch)
                        + Natural (Surface.w - 1)));
   end Goto_Line_End_Unchecked_Generic;

   --  ===========================================================
   --                     Goto_Line_End_Generic
   --  ===========================================================
   function Goto_Line_End_Generic (
      Surface  : Vd.Surface_ptr;
      Line_Num : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
      use type C.int;
   begin
      if Line_Num < Natural (Surface.h - 1) then
         return Uint_Ptrs.Object_Pointer (
            Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels))
            + C.ptrdiff_t (Line_Num * Natural (Surface.pitch)
                           + Natural (Surface.w - 1)));
      else
         return Uint_Ptrs.Object_Pointer (
            Uint_PtrOps.Pointer (Uint_Ptrs.To_Pointer (Surface.pixels))
            + C.ptrdiff_t (Natural (Surface.w - 1)));
      end if;
   end Goto_Line_End_Generic;

   --  ===========================================================
   --                 Next_Line_Unchecked_Generic
   --  ===========================================================
   function Next_Line_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual : Uint_Ptrs.Object_Pointer) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
   begin
      return Uint_Ptrs.Object_Pointer (
                Uint_PtrOps.Pointer (Actual)
                + C.ptrdiff_t (Surface.pitch));
   end Next_Line_Unchecked_Generic;

   --  ===========================================================
   --                Prev_Line_Unchecked_Generic
   --  ===========================================================
   function Prev_Line_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual : Uint_Ptrs.Object_Pointer) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
   begin
      return Uint_Ptrs.Object_Pointer (
                Uint_PtrOps.Pointer (Actual)
                - C.ptrdiff_t (Surface.pitch));
   end Prev_Line_Unchecked_Generic;

   --  ===========================================================
   --                   Go_Right_Unchecked_Generic
   --  ===========================================================
   function Go_Right_Unchecked_Generic (
      Actual : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
   begin
      return Uint_Ptrs.Object_Pointer (
                Uint_PtrOps.Pointer (Actual)
                + C.ptrdiff_t (Displacement));
   end Go_Right_Unchecked_Generic;

   --  ===========================================================
   --                  Go_Left_Unchecked_Generic
   --  ===========================================================
   function Go_Left_Unchecked_Generic (
      Actual : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
   begin
      return Uint_Ptrs.Object_Pointer (
                Uint_PtrOps.Pointer (Actual)
                - C.ptrdiff_t (Displacement));
   end Go_Left_Unchecked_Generic;

   --  ===========================================================
   --                   Go_Up_Unchecked_Generic
   --  ===========================================================
   function Go_Up_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual  : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
   begin
      return Uint_Ptrs.Object_Pointer (
                Uint_PtrOps.Pointer (Actual)
                - C.ptrdiff_t (Uint16 (Displacement) * Surface.pitch));
   end Go_Up_Unchecked_Generic;

   --  ===========================================================
   --                  Go_Down_Unchecked_Generic
   --  ===========================================================
   function Go_Down_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual  : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer
   is
      use Uint_PtrOps;
   begin
      return Uint_Ptrs.Object_Pointer (
                Uint_PtrOps.Pointer (Actual)
                + C.ptrdiff_t (Uint16 (Displacement) * Surface.pitch));
   end Go_Down_Unchecked_Generic;

   --  ===========================================================
   --                     Paint_Line_Generic
   --  ===========================================================
   procedure Paint_Line_Generic (
      Surface : Vd.Surface_ptr;
      Line_Num : Natural;
      Color : C.int)
   is
      Line_Begin : Uint_Ptrs.Object_Pointer;
   begin
      Line_Begin := Goto_Line (Surface, Line_Num);
      Lib_C.memset (
         Uint_Ptrs.To_Address (Line_Begin),
         Color,
         --  Surface.w * Surface.format.BytesPerPixel);
         C.size_t (Surface.pitch));
   end Paint_Line_Generic;

   --  ===========================================================
   --                     Paint_Line_Unchecked_Generic
   --  ===========================================================
   procedure Paint_Line_Unchecked_Generic (
      Surface    : Vd.Surface_ptr;
      Line_Begin : Uint_Ptrs.Object_Pointer;
      Color      : C.int) is
   begin
      Lib_C.memset (
         Uint_Ptrs.To_Address (Line_Begin),
         Color,
         --  Surface.w * Surface.format.BytesPerPixel);
         C.size_t (Surface.pitch));
   end Paint_Line_Unchecked_Generic;

   --  ===========================================================
   function MapRGB_Generic (
      format : Vd.PixelFormat_ptr;
      r      : Uint8;
      g      : Uint8;
      b      : Uint8)
      return Element_Type is
   begin
      return Element_Type (Vd.MapRGB (format, r, g, b));
   end MapRGB_Generic;

   --  ===========================================================

end SDL_Framebuffer_Generics;
