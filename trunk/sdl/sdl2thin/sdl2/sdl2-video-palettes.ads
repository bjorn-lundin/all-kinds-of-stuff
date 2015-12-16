--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
--  SDL.Video.Palettes
--
--  Palettes, Colors and various conversions.
--------------------------------------------------------------------------------------------------------------------
with Ada.Finalization;
with Ada.Iterator_Interfaces;
with Interfaces.C.Pointers;

package SDL2.Video.Palettes is
   package C renames Interfaces.C;

   type Color_Component is range 0 .. 255 with
     Size       => 8,
     Convention => C;

   type Color is
      record
         Red   : Color_Component;
         Green : Color_Component;
         Blue  : Color_Component;
         Alpha : Color_Component;
      end record with
        Convention => C,
        Size       => Color_Component'Size * 4;

   for Color use
      record
         Red   at 0 range  0 ..  7;
         Green at 0 range  8 .. 15;
         Blue  at 0 range 16 .. 23;
         Alpha at 0 range 24 .. 31;
      end record;

   Null_Color : constant Color := Color'(others => Color_Component'First);

   type RGB_Color is
      record
         Red   : Color_Component;
         Green : Color_Component;
         Blue  : Color_Component;
      end record;

   Null_RGB_Color : constant RGB_Color := RGB_Color'(others => Color_Component'First);

   --  Cursor type for our iterator.
   type Cursor is private;

   No_Element : constant Cursor;

   function Element (Position : in Cursor) return Color;

   function Has_Element (Position : Cursor) return Boolean;

   --  Create the iterator interface package.
   package Palette_Iterator_Interfaces is new
     Ada.Iterator_Interfaces (Cursor, Has_Element);

   type Palette is tagged limited private with
     Default_Iterator  => Iterate,
     Iterator_Element  => Color,
     Constant_Indexing => Constant_Reference;

   type Palette_Access is access Palette;

   function Constant_Reference
     (Container : aliased Palette;
      Position  : Cursor) return Color;

   function Iterate (Container : Palette)
      return Palette_Iterator_Interfaces.Forward_Iterator'Class;

   function Create (Total_Colors : in Positive) return Palette;

   procedure Free (Container : in out Palette);

   Empty_Palette : constant Palette;
private
   type Color_Array is array (C.size_t range <>) of aliased Color with
     Convention => C;

   package Color_Array_Pointer is new Interfaces.C.Pointers
     (Index              => C.size_t,
      Element            => Color,
      Element_Array      => Color_Array,
      Default_Terminator => Null_Color);

   type Internal_Palette is
      record
         Total     : C.int;
         Colors   : Color_Array_Pointer.Pointer;
         Version   : Interfaces.Unsigned_32;
         Ref_Count : C.int;
      end record with
        Convention => C;

   type Internal_Palette_Access is access Internal_Palette with
     Convention => C;

   type Palette is tagged limited
      record
         Data : Internal_Palette_Access;
      end record;

   type Palette_Constant_Access is access constant Palette;

   type Cursor is
      record
         Container : Palette_Constant_Access;
         Index     : Natural;
         Current   : Color_Array_Pointer.Pointer;
      end record;

   No_Element : constant Cursor := Cursor'(Container => null,
                                           Index     => Natural'First,
                                           Current   => null);

   Empty_Palette : constant Palette := Palette'(Data => null);
end SDL2.Video.Palettes;
