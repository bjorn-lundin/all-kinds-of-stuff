
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

package body SDL.Mouse is

   --  ======================================
   procedure Get_Mouse_State_X (
      state : out Mouse_State;
      x : in out C.int)
   is
      procedure GetMouseStateX (
         state : out Mouse_State;
         x : in out C.int;
         y : int_ptr);
      pragma Import (C, GetMouseStateX, "SDL_GetMouseState");
      pragma Import_Valued_Procedure (GetMouseStateX);
   begin
      GetMouseStateX (state, x, null);
   end Get_Mouse_State_X;

   --  ======================================

   procedure Get_Mouse_State_Y (
      state : out Mouse_State;
      y : in out C.int)
   is
      procedure GetMouseStateY (
         state : out Mouse_State;
         x : int_ptr;
         y : in out C.int);
      pragma Import (C, GetMouseStateY, "SDL_GetMouseState");
      pragma Import_Valued_Procedure (GetMouseStateY);
   begin
      GetMouseStateY (state, null, y);
   end Get_Mouse_State_Y;
   --  ======================================

end SDL.Mouse;
