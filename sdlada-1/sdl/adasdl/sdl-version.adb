
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

package body SDL.Version is

   use type C.int;

   -----------------
   -- SDL_VERSION --
   -----------------

   --  procedure SDL_VERSION (X : version_ptr) is
   procedure SDL_VERSION (X : in out version) is
   begin
      X.major := SDL_MAJOR_VERSION;
      X.minor := SDL_MINOR_VERSION;
      X.patch := SDL_PATCHLEVEL;
   end SDL_VERSION;

   --------------------
   -- SDL_VERSIONNUM --
   --------------------

   function SDL_VERSIONNUM
     (X : Uint8;
      Y : Uint8;
      Z : Uint8)
      return C.int
   is
      use type C.int;
   begin
      return C.int (X) * 1000 + C.int (Y) * 100 + C.int (Z);
   end SDL_VERSIONNUM;

   --  ===================================================================
   function SDL_COMPILEDVERSION return C.int is
   begin
      return SDL_VERSIONNUM (SDL_MAJOR_VERSION, SDL_MINOR_VERSION, SDL_PATCHLEVEL);
   end SDL_COMPILEDVERSION;

   --  ===================================================================
   function SDL_VERSION_ATLEAST (X : Uint8; Y : Uint8; Z : Uint8) return Boolean is
   begin
      return SDL_COMPILEDVERSION >= SDL_VERSIONNUM (X, Y, Z);
   end SDL_VERSION_ATLEAST;



end SDL.Version;

