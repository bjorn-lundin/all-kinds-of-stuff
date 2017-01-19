
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
--  This is an Ada binding to SDL ( Simple DirectMedia Layer from    --
--  Sam Lantinga - www.libsld.org )                                  --
--  **************************************************************** --
--  In order to help the Ada programmer, the comments in this file   --
--  are, in great extent, a direct copy of the original text in the  --
--  SDL header files.                                                --
--  **************************************************************** --

with System;
--with Interfaces.C;
with SDL.Types; use SDL.Types;
with SDL.Video;
package SDL.Mouse is

   package I renames Interfaces;

   type WMcursor is private;  -- Implementation dependent
   type Save_Cursor_Array is array (0 .. 1) of Uint8_ptr;
   pragma Convention (C, Save_Cursor_Array);
   type Cursor is
      record
         area : SDL.Video.Rect;    --  The area of the mouse cursor
         hot_x, hot_y : Sint16;    --  The "tip" of the cursor
         data : Uint8_ptr;         --  B/W cursor data
         mask : Uint8_ptr;         --  B/W cursor mask
         save : Save_Cursor_Array; --  Place de save cursor area
         wm_cursor : WMcursor;     --  Window-manager cursor
      end record;
   pragma Convention (C, Cursor);

   type Cursor_ptr is access all Cursor;
   pragma Convention (C, Cursor_ptr);

   --  -------------------
   --  Function prototypes
   --  -------------------
   --

   type Mouse_State is mod 2**8;
   for Mouse_State'Size use 8;
   pragma Convention (C, Mouse_State);

   --
   --  Retrieve the current state of the mouse.
   --  The current button state is returned as a button bitmask, which can
   --  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
   --  current mouse cursor position.  You can pass NULL for either x or y.
   function GetMouseState (x, y : int_ptr) return Mouse_State;

   pragma Import (C, GetMouseState, "SDL_GetMouseState");

   procedure Get_Mouse_State (
      state : out Mouse_State;
      x : in out C.int;
      y : in out C.int);
   pragma Import (C, Get_Mouse_State, "SDL_GetMouseState");
   pragma Import_Valued_Procedure (Get_Mouse_State);

   procedure Get_Mouse_State_X (
      state : out Mouse_State;
      x : in out C.int);
   pragma Inline (Get_Mouse_State_X);

   procedure Get_Mouse_State_Y (
      state : out Mouse_State;
      y : in out C.int);
   pragma Inline (Get_Mouse_State_Y);

   --  Retrieve the current state of the mouse.
   --  The current button state is returned as a button bitmask, which can
   --  be tested using the SDL_BUTTON(X) macros, and x and y are set to the
   --  mouse deltas since the last call to SDL_GetRelativeMouseState().
   function GetRelativeMouseState (x, y : int_ptr) return Mouse_State;
   pragma Import (C, GetRelativeMouseState, "SDL_GetRelativeMouseState");

   --  Set the position of the mouse cursor (generates a mouse motion event)
   procedure WarpMouse (x, y : Uint16);
   pragma Import (C, WarpMouse, "SDL_WarpMouse");


   --  Create a cursor using the specified data and mask (in MSB format).
   --  The cursor width must be a multiple of 8 bits.
   --
   --  The cursor is created in black and white according to the following:
   --  data  mask    resulting pixel on screen
   --   0     1       White
   --   1     1       Black
   --   0     0       Transparent
   --   1     0       Inverted color if possible, black if not.

   --  Cursors created with this function must be freed with SDL_FreeCursor().
   function CreateCursor (
      data : Uint8_ptr;
      mask : Uint8_ptr;
      w, h : C.int;
      hot_x, hot_y : C.int)
      return Cursor_ptr;
   pragma Import (C, CreateCursor, "SDL_CreateCursor");

   --  Set the currently active cursor to the specified one.
   --  If the cursor is currently visible, the change will be immediately
   --  represented on the display.
   procedure SetCursor (the_cursor : Cursor_ptr);
   pragma Import (C, SetCursor, "SDL_SetCursor2");

   --  Returns the currently active cursor.
   function GetCursor return Cursor_ptr;
   pragma Import (C, GetCursor, "SDL_GetCursor");

   --  Deallocates a cursor created with SDL_CreateCursor().
   procedure FreeCursor (the_cursor : Cursor_ptr);
   pragma Import (C, FreeCursor, "SDL_FreeCursor");

   --  Toggle whether or not the cursor is shown on the screen.
   --  The cursor start off displayed, but can be turned off.
   --  SDL_ShowCursor returns 1 if the cursor was being displayed
   --  before the call, or 0 if it was not.
   function ShowCursor (toggle : C.int) return C.int;
   procedure ShowCursor (toggle : C.int);
   pragma Import (C, ShowCursor, "SDL_ShowCursor");


   type Button_ID is new C.int range 1 .. 3;
   pragma Convention (C, Button_ID);

   type Button_Mask is mod 2**32;
   pragma Convention (C, Button_Mask);

   BUTTON_LEFT   : constant Button_ID := 1;
   BUTTON_MIDDLE : constant Button_ID := 2;
   BUTTON_RIGHT  : constant Button_ID := 3;

   BUTTON_LAMSK  : constant Button_Mask :=
      Button_Mask (I.Shift_Left (
                      I.Unsigned_32 (SDL_PRESSED),
                      Natural (BUTTON_LEFT - 1)));
   BUTTON_MMASK  : constant Button_Mask :=
      Button_Mask (I.Shift_Left (
                      I.Unsigned_32 (SDL_PRESSED),
                      Natural (BUTTON_MIDDLE - 1)));
   BUTTON_RMASK  : constant Button_Mask :=
      Button_Mask (I.Shift_Left (
                      I.Unsigned_32 (SDL_PRESSED),
                      Natural (BUTTON_RIGHT - 1)));

   type Mouse_Button_State is mod 2**8;
   for Mouse_Button_State'Size use 8;
   pragma Convention (C, Mouse_Button_State);

   PRESSED : constant Mouse_Button_State :=
      Mouse_Button_State (SDL_PRESSED);
   RELEASED : constant Mouse_Button_State :=
      Mouse_Button_State (SDL_RELEASED);

private
   type WMcursor is new System.Address;

end SDL.Mouse;
