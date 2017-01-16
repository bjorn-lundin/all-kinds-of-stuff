
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

package body SDL.Cdrom is

   --  ===============================================
   function INDRIVE (status : CDstatus) return Boolean is
   begin
      return status > 0;
   end INDRIVE;

   --  ===============================================
   procedure FRAMES_TO_MSF (
      frames : C.int;
      M : in out C.int;
      S : in out C.int;
      F : in out C.int)
   is
      use type Interfaces.C.int;
      value : C.int := frames;
   begin
      F := value mod CD_FPS;
      value := value / CD_FPS;
      S := value mod 60;
      value := value / 60;
      M := value;
   end FRAMES_TO_MSF;

   --  ===============================================
   function MSF_TO_FRAMES (
      M : C.int;
      S : C.int;
      F : C.int) return C.int
   is
      use type Interfaces.C.int;
   begin
      return M * 60 * CD_FPS + S * CD_FPS + F;
   end MSF_TO_FRAMES;

   --  ===============================================
end SDL.Cdrom;
