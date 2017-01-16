
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

with System.Address_To_Access_Conversions;
with Interfaces.C.Pointers;
with SDL.Types; use  SDL.Types;
with UintN_PtrOps;
with SDL.Video; use  SDL.Video;
with SDL_Framebuffer_Generics; use  SDL_Framebuffer_Generics;
package SDL_Framebuffer is
   package C renames Interfaces.C;

   procedure Copy_Colors (
      Source     : Surface_ptr;
      Dest       : Color_PtrOps.Pointer;
      Num_Colors : Natural);
   pragma Inline (Copy_Colors);

   function Copy_Colors (
      Surface : Surface_ptr;
      Num_Colors : Natural) return Colors_Array;
   pragma Inline (Copy_Colors);

   --  ===========================================================
   subtype Framebuffer_8bPointer is
      Uint8_Ptrs.Object_Pointer;

   subtype Framebuffer_16bPointer is
      Uint16_Ptrs.Object_Pointer;

   subtype Framebuffer_32bPointer is
      Uint32_Ptrs.Object_Pointer;

   --  ===========================================================
   function Go_XY_Unchecked is
      new Go_XY_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Go_XY_8Unchecked is
      new Go_XY_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Go_XY_Unchecked is
      new Go_XY_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Go_XY_16Unchecked is
      new Go_XY_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Go_XY_Unchecked is
      new Go_XY_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   function Go_XY_32Unchecked is
      new Go_XY_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   function Go_XY_24b_Unchecked (
      Surface : Vd.Surface_ptr;
      X       : Natural;
      Y       : Natural) return Framebuffer_8bPointer;
   pragma Inline (Go_XY_24b_Unchecked);

   procedure Set_24b_Value_Unchecked (
      Surface  : Vd.Surface_ptr;
      Location : Framebuffer_8bPointer;
      Value    : Uint32);
   pragma Inline (Set_24b_Value_Unchecked);

   --  ===================================================================
   function Get_Framebuffer is
      new Get_Framebuffer_Generic (Uint8,  Uint8_Ptrs);

   function Get_Framebuffer is
      new Get_Framebuffer_Generic (Uint16, Uint16_Ptrs);

   function Get_Framebuffer is
      new Get_Framebuffer_Generic (Uint32, Uint32_Ptrs);

   --  ===================================================================
   function Goto_Line_Unchecked is
      new Goto_Line_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Goto_Line_Unchecked is
      new Goto_Line_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Goto_Line_Unchecked is
      new Goto_Line_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Goto_Line is
      new Goto_Line_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Goto_Line is
      new Goto_Line_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Goto_Line is
      new Goto_Line_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Goto_Line_End_Unchecked is
      new Goto_Line_End_Unchecked_Generic (
         Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Goto_Line_End_Unchecked is
      new Goto_Line_End_Unchecked_Generic (
         Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Goto_Line_End_Unchecked is
      new Goto_Line_End_Unchecked_Generic (
         Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Goto_Line_End is
      new Goto_Line_End_Generic (
         Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Goto_Line_End is
      new Goto_Line_End_Generic (
         Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Goto_Line_End is
      new Goto_Line_End_Generic (
         Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Next_Line_Unchecked is
      new Next_Line_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Next_Line_Unchecked is
      new Next_Line_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Next_Line_Unchecked is
      new Next_Line_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Prev_Line_Unchecked is
      new Prev_Line_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Prev_Line_Unchecked is
      new Prev_Line_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Prev_Line_Unchecked is
      new Prev_Line_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Go_Right_Unchecked is
      new Go_Right_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Go_Right_Unchecked is
      new Go_Right_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Go_Right_Unchecked is
      new Go_Right_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Go_Left_Unchecked is
      new Go_Left_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Go_Left_Unchecked is
      new Go_Left_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Go_Left_Unchecked is
      new Go_Left_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===========================================================
   function Go_Up_Unchecked is
      new Go_Up_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Go_Up_Unchecked is
      new Go_Up_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Go_Up_Unchecked is
      new Go_Up_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===================================================================
   function Go_Down_Unchecked is
      new Go_Down_Unchecked_Generic (Uint8,  Uint8_Ptrs,  Uint8_Array,  Uint8_PtrOps);

   function Go_Down_Unchecked is
      new Go_Down_Unchecked_Generic (Uint16, Uint16_Ptrs, Uint16_Array, Uint16_PtrOps);

   function Go_Down_Unchecked is
      new Go_Down_Unchecked_Generic (Uint32, Uint32_Ptrs, Uint32_Array, Uint32_PtrOps);

   --  ===================================================================
   function Pitch_Gap (Surface : Surface_ptr) return Uint16;
   pragma Inline (Pitch_Gap);

   --  ===================================================================
   procedure Paint_Line_8b is
      new Paint_Line_Generic (Uint8,  Uint8_Ptrs);

   procedure Paint_Line_16b is
      new Paint_Line_Generic (Uint16, Uint16_Ptrs);

   procedure Paint_Line_32b is
      new Paint_Line_Generic (Uint32, Uint32_Ptrs);

   --  ===================================================================
   procedure Paint_Line_Unchecked is
      new Paint_Line_Unchecked_Generic (Uint8,  Uint8_Ptrs);

   procedure Paint_Line_Unchecked is
      new Paint_Line_Unchecked_Generic (Uint16, Uint16_Ptrs);

   procedure Paint_Line_Unchecked is
      new Paint_Line_Unchecked_Generic (Uint32, Uint32_Ptrs);

   --  ===================================================================
   function MapRGB is new MapRGB_Generic (Uint8);
   function MapRGB is new MapRGB_Generic (Uint16);
   function MapRGB is new MapRGB_Generic (Uint32);

   --  ===================================================================
   function Get_Palette_Red (
      Surface : Surface_ptr;
      Color_Index : Uint8) return Uint8;

   function Get_Palette_Green (
      Surface : Surface_ptr;
      Color_Index : Uint8) return Uint8;

   function Get_Palette_Blue (
      Surface : Surface_ptr;
      Color_Index : Uint8) return Uint8;


   --  #######################################################################

end SDL_Framebuffer;
