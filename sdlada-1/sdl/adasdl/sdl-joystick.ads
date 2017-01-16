
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
with Interfaces.C.Strings;
with SDL.Types; use SDL.Types;

package SDL.Joystick is

   package C renames Interfaces.C;

   --   The pointer to a internal joystick structure used
   --   to identify an SDL joystick
   type Joystick_ptr is new System.Address;
   Null_Joystick_ptr : constant Joystick_ptr :=
      Joystick_ptr (System.Null_Address);

   --  Function prototypes */
   --
   --  Count the number of joysticks attached to the system
   function NumJoysticks return C.int;
   pragma Import (C, NumJoysticks, "SDL_NumJoysticks");


   --  Get the implementation dependent name of a joystick.
   --  This can be called before any joysticks are opened.
   --  If no name can be found, this function returns NULL.
   function JoystickName (device_index : C.int) return C.Strings.chars_ptr;
   pragma Import (C, JoystickName, "SDL_JoystickName");

   --  Open a joystick for use - the index passed as an argument refers to
   --  the N'th joystick on the system.  This index is the value which will
   --  identify this joystick in future joystick events.

   --  This function returns a joystick identifier, or
   --  NULL if an error occurred.
   function JoystickOpen (device_index : C.int) return Joystick_ptr;
   pragma Import (C, JoystickOpen, "SDL_JoystickOpen");

   --  Returns 1 if the joystick has been opened, or 0 if it has not.
   function JoystickOpened (device_index : C.int) return C.int;
   pragma Import (C, JoystickOpened, "SDL_JoystickOpened");

   --  Get the device index of an opened joystick.
   function JoystickIndex (joystick : Joystick_ptr) return  C.int;
   pragma Import (C, JoystickIndex, "SDL_JoystickIndex");

   --  Get the number of general axis controls on a joystick
   function JoystickNumAxes (joystick : Joystick_ptr) return C.int;
   pragma Import (C, JoystickNumAxes, "SDL_JoystickNumAxes");

   --  Get the number of trackballs on a joystick
   --  Joystick trackballs have only relative motion events associated
   --  with them and their state cannot be polled.
   function JoystickNumBalls (joystick : Joystick_ptr) return C.int;
   pragma Import (C, JoystickNumBalls, "SDL_JoystickNumBalls");

   --  Get the number of POV hats on a joystick
   function JoystickNumHats (joystick : Joystick_ptr) return C.int;
   pragma Import (C, JoystickNumHats, "SDL_JoystickNumHats");

   --  Get the number of buttonYs on a joystick
   function JoystickNumButtons (joystick : Joystick_ptr) return C.int;
   pragma Import (C, JoystickNumButtons, "SDL_JoystickNumButtons");

   --  Update the current state of the open joysticks.
   --  This is called automatically by the event loop if any joystick
   --  events are enabled.
   procedure JoystickUpdate;
   pragma Import (C, JoystickUpdate, "SDL_JoystickUpdate");

   --  Enable/disable joystick event polling.
   --  If joystick events are disabled, you must call JoystickUpdate
   --  yourself and check the state of the joystick when you want joystick
   --  information.
   --  The state can be one of QUERY, ENABLE or IGNORE.
   function JoystickEventState (state : C.int) return C.int;
   pragma Import (C, JoystickEventState, "SDL_JoystickEventState");


   --  Get the current state of an axis control on a joystick
   --  The state is a value ranging from -32768 to 32767.
   --  The axis indices start at index 0.
   function JoystickGetAxis (
      joystick : Joystick_ptr;
      axis : C.int)
      return Sint16;
   pragma Import (C, JoystickGetAxis, "SDL_JoystickGetAxis");

   --  Get the current state of a POV hat on a joystick
   --  The return value is one of the following positions:

   --  TO BE REMOVED type HAT_State is mod 2**16;
   type HAT_State is mod 2**8;
   for HAT_State'Size use 8;

   HAT_CENTERED    : constant HAT_State := 16#00#;
   HAT_UP          : constant HAT_State := 16#01#;
   HAT_RIGHT       : constant HAT_State := 16#02#;
   HAT_DOWN        : constant HAT_State := 16#04#;
   HAT_LEFT        : constant HAT_State := 16#08#;
   HAT_RIGHTUP     : constant HAT_State := (HAT_RIGHT or HAT_UP);
   HAT_RIGHTDOWN   : constant HAT_State := (HAT_RIGHT or HAT_DOWN);
   HAT_LEFTUP      : constant HAT_State := (HAT_LEFT  or HAT_UP);
   HAT_LEFTDOWN    : constant HAT_State := (HAT_LEFT  or HAT_DOWN);

   --  The hat indices start at index 0.

   function JoystickGetHat (
      joystick : Joystick_ptr;
      hat : C.int)
      return Uint8;
   pragma Import (C, JoystickGetHat, "SDL_JoystickGetHat");

   --  Get the ball axis change since the last poll
   --  This returns 0, or -1 if you passed it invalid parameters.
   --  The ball indices start at index 0.
   function JoystickGetBall (
      joystick : Joystick_ptr;
      ball : C.int;
      dx, dy : int_ptr)
      return C.int;
   pragma Import (C, JoystickGetBall, "SDL_JoystickGetBall");

   type Joy_Button_State is mod 2**8;
   for Joy_Button_State'Size use 8;
   pragma Convention (C, Joy_Button_State);

   PRESSED  : constant Joy_Button_State :=
      Joy_Button_State (SDL_PRESSED);
   RELEASED : constant Joy_Button_State :=
      Joy_Button_State (SDL_RELEASED);

   --  Get the current state of a button on a joystick
   --  The button indices start at index 0.
   function JoystickGetButton (
      joystick : Joystick_ptr;
      button : C.int)
      return Joy_Button_State;
   pragma Import (C, JoystickGetButton, "SDL_JoystickGetButton");

   --  Close a joystick previously opened with SDL_JoystickOpen
   procedure JoystickClose (joystick : Joystick_ptr);
   pragma Import (C, JoystickClose, "SDL_JoystickClose");


end SDL.Joystick;
