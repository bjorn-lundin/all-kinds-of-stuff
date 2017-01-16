
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
with Interfaces.C;
with UintN_PtrOps;
with SDL.Types; use SDL.Types;
with SDL.Video;
package SDL_Framebuffer_Generics is
   pragma Elaborate_Body;

   package Vd renames SDL.Video;

   package C  renames Interfaces.C;

   --  ===========================================================
   --                   Go_XY_Generic
   --  ===========================================================
   --  If the pair (X, Y) is inside the surface limits the function
   --  returns a pointer to the new position.
   --  else returns a pointer to position (0, 0).
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Go_XY_Generic (
      Surface : Vd.Surface_ptr;
      X       : Natural;
      Y       : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Go_XY_Generic);

   --  ===========================================================
   --                   Go_XY_Unchecked_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Go_XY_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      X       : Natural;
      Y       : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Go_XY_Unchecked_Generic);

   --  ===========================================================
   --                 Get_Framebuffer_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
   --  ------------------------------------------------------------
   function Get_Framebuffer_Generic (
      Surface : Vd.Surface_ptr) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Get_Framebuffer_Generic);

   --  ===========================================================
   --                     Goto_Line_Generic
   --  ===========================================================
   --  For security reasons line limit is checked.
   --  Returns a pointer to the start of the line.
   --  If "Line_Num" is out of range it will return a pointer to the
   --  the start of the first line of the Surface.
   --  It is slower than "Goto_Line_End_Unchecked".
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Goto_Line_Generic (
      Surface     : Vd.Surface_ptr;
      Line_Num    : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Goto_Line_Generic);

   --  ===================================================================
   --                   Goto_Line_Unchecked_Generic
   --  ===================================================================
   --
   --    Surface
   --    0,0-------->
   --      |
   --      | --- Line
   --      |
   --      v
   --
   --  Goes to a specific pixel line in "Surface".

   --  For speed reasons Line limit is not checked.

   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Goto_Line_Unchecked_Generic (
      Surface     : Vd.Surface_ptr;
      Line_Num    : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Goto_Line_Unchecked_Generic);

   --  ===========================================================
   --             Goto_Line_End_Unchecked_Generic
   --  ===========================================================
   --  For speed reasons Line limit is not checked.
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Goto_Line_End_Unchecked_Generic (
      Surface  : Vd.Surface_ptr;
      Line_Num : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Goto_Line_End_Unchecked_Generic);

   --  ===========================================================
   --                     Goto_Line_End_Generic
   --  ===========================================================
   --  For security reasons line limit is checked.
   --  Returns a pointer to the end of the line.
   --  If "Line" is out of range it will return a pointer to the
   --  the end of the first line of the Surface.
   --  It is slower than "Goto_Line_End_Unchecked".
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Goto_Line_End_Generic (
      Surface  : Vd.Surface_ptr;
      Line_Num : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Goto_Line_End_Generic);

   --  ===========================================================
   --                 Next_Line_Unchecked_Generic
   --  ===========================================================
   --  "Actual" must be a pointer to the beginning of
   --  the present line.
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Next_Line_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual  : Uint_Ptrs.Object_Pointer) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Next_Line_Unchecked_Generic);

   --  ===========================================================
   --                Prev_Line_Unchecked_Generic
   --  ===========================================================
   --  "Actual" must be a pointer to the beginning of
   --  the present line.
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Prev_Line_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual  : Uint_Ptrs.Object_Pointer) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Prev_Line_Unchecked_Generic);

   --  ===========================================================
   --                   Go_Right_Unchecked_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Go_Right_Unchecked_Generic (
      Actual : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Go_Right_Unchecked_Generic);

   --  ===========================================================
   --                  Go_Left_Unchecked_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Go_Left_Unchecked_Generic (
      Actual : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Go_Left_Unchecked_Generic);

   --  ===========================================================
   --                   Go_Up_Unchecked_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Go_Up_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual  : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Go_Up_Unchecked_Generic);

   --  ===========================================================
   --                  Go_Down_Unchecked_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      type Vector is array (C.size_t range <>) of aliased Element_Type;
      with package Uint_PtrOps is
         new UintN_PtrOps (Element_Type, Vector);
   --  ------------------------------------------------------------
   function Go_Down_Unchecked_Generic (
      Surface : Vd.Surface_ptr;
      Actual  : Uint_Ptrs.Object_Pointer;
      Displacement : Natural) return Uint_Ptrs.Object_Pointer;
   pragma Inline (Go_Down_Unchecked_Generic);

   --  ===========================================================
   --                     Paint_Line_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
      with function Goto_Line (
         Surface  : Vd.Surface_ptr;
         Line_Num : Natural) return Uint_Ptrs.Object_Pointer is <>;
   --  ------------------------------------------------------------
   procedure Paint_Line_Generic (
      Surface  : Vd.Surface_ptr;
      Line_Num : Natural;
      Color    : C.int);
   pragma Inline (Paint_Line_Generic);

   --  ===========================================================
   --                     Paint_Line_Unchecked_Generic
   --  ===========================================================
   generic
      type Element_Type is mod <>;
      with package Uint_Ptrs is
         new System.Address_To_Access_Conversions (Element_Type);
   --  ------------------------------------------------------------
   procedure Paint_Line_Unchecked_Generic (
      Surface    : Vd.Surface_ptr;
      Line_Begin : Uint_Ptrs.Object_Pointer;
      Color : C.int);
   pragma Inline (Paint_Line_Unchecked_Generic);

   --  ===========================================================
   generic
      type Element_Type is mod <>;
   function MapRGB_Generic (
      format : Vd.PixelFormat_ptr;
      r      : Uint8;
      g      : Uint8;
      b      : Uint8)
      return Element_Type;
   pragma Inline (MapRGB_Generic);

   --  ===========================================================

end SDL_Framebuffer_Generics;
