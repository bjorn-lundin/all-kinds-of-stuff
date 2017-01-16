
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

with SDL.Types; use SDL.Types;
with SDL.Keysym; --  use SDL.Keysym;
with Interfaces.C.Strings;
package SDL.Keyboard is
   package Ks renames SDL.Keysym;
   package C renames Interfaces.C;

   --  Keysym structure
   --   - The scancode is hardware dependent, and should not be used by general
   --     applications.  If no hardware scancode is available, it will be 0.
   --
   --   - The 'unicode' translated character is only available when character
   --     translation is enabled by the EnableUNICODE API.  If non-zero,
   --     this is a UNICODE character corresponding to the keypress.  If the
   --     high 9 bits of the character are 0, then this maps to the equivalent
   --     ASCII character:
   --        ch : Character;
   --        if (keysym.unicode and 0xFF80) = 0 then
   --                ch := keysym.unicode and  0x7F;
   --        else
   --                An international character..
   --        end if;
   --

   type keysym is
      record
         scancode : Uint8;
         sym      : Ks.Key;
         the_mod  : Ks.SDLMod;
         unicode  : Uint16;
      end record;
   for keysym'Size use 8*16; -- A practical adjust
   pragma Convention (C, keysym);

   type keysym_ptr is access all  keysym;
   pragma Convention (C, keysym_ptr);

   type keysym_Const_ptr is access constant keysym;
   pragma Convention (C, keysym_Const_ptr);

   --  This is the mask which refers to all hotkey bindings
   SDL_ALL_HOTKEYS : constant Ks.SDLMod := 16#FFFFFFFF#;

   --  ---------------------  --
   --  Function prototypes    --
   --  ---------------------  --
   --  Enable/Disable UNICODE translation of keyboard input.
   --  This translation has some overhead, so translation defaults off.
   --  If 'enable' is 1, translation is enabled.
   --  If 'enable' is 0, translation is disabled.
   --  If 'enable' is -1, the translation state is not changed.
   --  It returns the previous state of keyboard translation.

   function EnableUNICODE (enable : C.int) return C.int;
   procedure EnableUNICODE (enable : C.int);
   pragma Import (C, EnableUNICODE, "SDL_EnableUNICODE");


   --  Enable/Disable keyboard repeat.  Keyboard repeat defaults to off.
   --  'delay' is the initial delay in ms between the time when a key is
   --  pressed, and keyboard repeat begins.
   --  'interval' is the time in ms between keyboard repeat events.

   DEFAULT_REPEAT_DELAY    : constant := 500;
   DEFAULT_REPEAT_INTERVAL : constant :=  30;

   --  If 'delay' is set to 0, keyboard repeat is disabled.
   function EnableKeyRepeat (
      the_delay : C.int;
      interval  : C.int)
      return C.int;

   procedure EnableKeyRepeat (
      the_delay : C.int;
      interval  : C.int);

   pragma Import (C, EnableKeyRepeat, "SDL_EnableKeyRepeat");


   --  Get a snapshot of the current state of the keyboard.
   --  Returns an array of keystates, indexed by the K_* syms.
   --  Used:
   --      keystates = GetKeyState(NULL);
   --  if keystates(K_RETURN) ... <RETURN> is pressed.

   type KeyStates_Array is
      array (Ks.Key range Ks.K_FIRST .. Ks.K_LAST) of Uint8;
   pragma Convention (C, KeyStates_Array);

   function GetKeyState (numkeys : int_ptr) return Uint8_ptr;
   pragma Import (C, GetKeyState, "SDL_GetKeyState");

   --  This function is not part of the original SDL API
   function Is_Key_Pressed (ref_Keys : Uint8_ptr; The_Key : Ks.Key) return Boolean;
   pragma Inline (Is_Key_Pressed);

   --  Get the current key modifier state
   function GetModState return Ks.SDLMod;
   pragma Import (C, GetModState, "SDL_GetModState");


   --  Set the current key modifier state
   --  This does not change the keyboard state, only the key modifier flags.
   procedure SetModState (modstate : Ks.SDLMod);
   pragma Import (C, SetModState, "SDL_SetModState");

   --  Get the name of an SDL virtual keysym
   function GetKeyName (the_key : Ks.Key) return C.Strings.chars_ptr;
   pragma Import (C, GetKeyName, "SDL_GetKeyName");

end SDL.Keyboard;
