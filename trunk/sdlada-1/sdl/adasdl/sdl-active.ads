
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

--  SDL application focus event handling
package SDL.Active is

   --  The available application states
   type Active_State is mod 2**8;
   for Active_State'Size use 8;
   --  The app has mouse coverage
   APPMOUSEFOCUS : constant Active_State := 16#01#;
   --  The app has input focus
   APPINPUTFOCUS : constant Active_State := 16#02#;
   --  The application is active
   APPACTIVE     : constant Active_State := 16#04#;

   --  Function prototypes
   --  This function returns the current state of the application,
   --  which is a bitwise combination of APPMOUSEFOCUS, APPINPUTFOCUS,
   --  and APPACTIVE. If APPACTIVE is set, then the user is able to
   --  see your application, otherwise it has been iconified or
   --  disabled.
   function GetAppState return Active_State;
   pragma Import (C, GetAppState, "SDL_GetAppState");

end SDL.Active;
