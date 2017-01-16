
-- ----------------------------------------------------------------- --
--                AdaSDL                                             --
--                Thin binding to Simple Direct Media Layer          --
--                Copyright (C) 2000-2012  A.M.F.Vargas              --
--                Antonio M. M. Ferreira Vargas                      --
--                Manhente - Barcelos - Portugal                     --
--                http://adasdl.sourceforge.net                      --
--                E-mail: amfvargas@gmail.com                        --
-- ----------------------------------------------------------------- --
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

with Interfaces.C;
package SDL is
   --  pragma Linker_Options ("-lSDL");
   --  pragma Linker_Options ("-lpthread");

   package C renames Interfaces.C;

   --  As of version 0.5, SDL is loaded dynamically into the application */

   --  These are the flags which may be passed to Init-- you should
   --  specify the subsystems which you will be using in your application.

   type Init_Flags is mod 2**32;
   pragma Convention (C, Init_Flags);

   --  These are the flags which may be passed to SDL.Init(...) -- you should
   --  specify the subsystems which you will be using in your application.
   INIT_TIMER       : constant Init_Flags := 16#00000001#;
   INIT_AUDIO       : constant Init_Flags := 16#00000010#;
   INIT_VIDEO       : constant Init_Flags := 16#00000020#;
   INIT_CDROM       : constant Init_Flags := 16#00000100#;
   INIT_JOYSTICK    : constant Init_Flags := 16#00000200#;
   --  Don't catch fatal signals
   INIT_NOPARACHUTE : constant Init_Flags := 16#00100000#;
   --  Not supported on all OS's
   INIT_EVENTTHREAD : constant Init_Flags := 16#01000000#;
   INIT_EVERYTHING  : constant Init_Flags := 16#0000FFFF#;


   --  This function loads the SDL dynamically linked library and
   --  initializes the subsystems specified by 'flags' (and those
   --  satisfying dependencies) Unless the INIT_NOPARACHUTE flag
   --  is set, it will install cleanup signal handlers for some
   --  commonly ignored fatal signals (like SIGSEGV)
   function Init (flags :  Init_Flags) return C.int;
   procedure Init (flags : Init_Flags);
   pragma Import (C, Init, "SDL_Init");

   --  This function initializes specific SDL subsystems
   function InitSubSystem (flags : Init_Flags) return C.int;
   pragma Import (C, InitSubSystem, "SDL_InitSubSystem");

   --  This function cleans up specific SDL subsystems
   procedure QuitSubSystem (flags : Init_Flags);
   pragma Import (C, QuitSubSystem, "SDL_QuitSubSystem");

   --  This function returns mask of the specified subsystems which have
   --  been initialized.
   --  If 'flags' is 0, it returns a mask of all initialized subsystems.
   function WasInit (flags : Init_Flags) return Init_Flags;
   pragma Import (C, WasInit, "SDL_WasInit");

   --  This function cleans up all initialized subsystems and unloads the
   --  dynamically linked library.  You should call it upon all exit
   --  conditions.
   procedure SDL_Quit;
   pragma Import (C, SDL_Quit, "SDL_Quit");



end SDL;
