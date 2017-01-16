
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

with Interfaces.C;
with Lib_C;
package body SDL_Framebuffer is

   use type C.int;

   --  ===========================================================
   function Go_XY_24b_Unchecked (
      Surface : Vd.Surface_ptr;
      X       : Natural;
      Y       : Natural) return Framebuffer_8bPointer
   is
   begin
      return Go_XY_Unchecked (Surface, Natural (3 * X), Natural (Y));
   end Go_XY_24b_Unchecked;

   --  ===========================================================
   procedure Set_24b_Value_Unchecked (
      Surface  : Vd.Surface_ptr;
      Location : Framebuffer_8bPointer;
      Value    : Uint32)
   is
      use Uint8_Ptrs;
      use Uint8_PtrOps;
      use Interfaces;
      Shift : C.int;
      Pix   : Framebuffer_8bPointer := Location;
   begin
      Shift := C.int (Surface.format.Rshift);
      Uint8_Ptrs.Object_Pointer (
         Uint8_PtrOps.Pointer (Pix)
         + C.ptrdiff_t (Shift / 8)
      ).all := Uint8 (Shift_Right (Value, Integer (Shift)));

      Shift := C.int (Surface.format.Gshift);
      Uint8_Ptrs.Object_Pointer (
         Uint8_PtrOps.Pointer (Pix)
         + C.ptrdiff_t (Shift / 8)
      ).all := Uint8 (Shift_Right (Value, Integer (Shift)));

      Shift := C.int (Surface.format.Bshift);
      Uint8_Ptrs.Object_Pointer (
         Uint8_PtrOps.Pointer (Pix)
         + C.ptrdiff_t (Shift / 8)
      ).all := Uint8 (Shift_Right (Value, Integer (Shift)));
   end Set_24b_Value_Unchecked;

   --  =======================================
   procedure Copy_Colors (
      Source     : Surface_ptr;
      Dest       : Color_PtrOps.Pointer;
      Num_Colors : Natural)
   is
   begin
      Color_PtrOps.Copy_Array (
         Color_PtrOps.Pointer (Source.format.palette.colors),
         Dest,
         C.ptrdiff_t (Num_Colors));
   end Copy_Colors;

   --  =======================================
   function Copy_Colors (
      Surface : Surface_ptr;
      Num_Colors : Natural) return Colors_Array is
   begin
      return Color_PtrOps.Value (
         Color_PtrOps.Pointer (Surface.format.palette.colors),
         C.ptrdiff_t (Num_Colors));
   end Copy_Colors;

   --  ===================================================================
   function Pitch_Gap (Surface : Surface_ptr) return Uint16 is
   begin
      return Surface.pitch
             - Uint16 (Surface.w)* Uint16 (Surface.format.BytesPerPixel);
   end Pitch_Gap;

   --  ==================================================================
   function Get_Palette_Red (
      Surface : Surface_ptr;
      Color_Index : Uint8) return Uint8
   is
      use Color_PtrOps;
   begin
      return
         Color_ptr (
            Color_PtrOps.Pointer (Surface.format.palette.colors)
            + C.ptrdiff_t (Color_Index)
         ).all.r;
   end Get_Palette_Red;

   --  =============================================
   function Get_Palette_Green (
      Surface : Surface_ptr;
      Color_Index : Uint8) return Uint8
   is
      use Color_PtrOps;
   begin
      return
         Color_ptr (
            Color_PtrOps.Pointer (Surface.format.palette.colors)
            + C.ptrdiff_t (Color_Index)
         ).all.g;
   end Get_Palette_Green;

   --  =============================================
   function Get_Palette_Blue (
      Surface : Surface_ptr;
      Color_Index : Uint8) return Uint8
   is
      use Color_PtrOps;
   begin
      return
         Color_ptr (
            Color_PtrOps.Pointer (Surface.format.palette.colors)
            + C.ptrdiff_t (Color_Index)
         ).all.b;
   end Get_Palette_Blue;
--  #######################################################################

end SDL_Framebuffer;
