
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

package SDL.Version is

   --  Printable format: "%d.%d.%d", MAJOR, MINOR, PATCHLEVEL

   SDL_MAJOR_VERSION : constant := 1;
   SDL_MINOR_VERSION : constant := 1;
   SDL_PATCHLEVEL    : constant := 8;

   type version is
      record
         major : Uint8;
         minor : Uint8;
         patch : Uint8;
      end record;
   pragma Convention (C, version);

   type version_ptr is access all version;
   pragma Convention (C, version_ptr);

   --  This macro can be used to fill a version structure with the compile-time
   --  version of the SDL library.
   --  procedure SDL_VERSION (X : version_ptr);
   --  pragma Inline (SDL_VERSION);
   procedure SDL_VERSION (X : in out version);
   pragma Inline (SDL_VERSION);

   --  This original 'C" macro turns the version numbers into a numeric value:
   --  (1,2,3) -> (1203)
   --  This assumes that there will never be more than 100 patchlevels
   function SDL_VERSIONNUM (
      X : Uint8;
      Y : Uint8;
      Z : Uint8)
      return C.int;
   pragma Inline (SDL_VERSIONNUM);

   --  This is the version number macro for the current SDL version
   function SDL_COMPILEDVERSION return C.int;
   pragma Inline (SDL_COMPILEDVERSION);

   --  This macro will evaluate to True if compiled with SDL at least X.Y.Z
   function SDL_VERSION_ATLEAST (X : Uint8; Y : Uint8; Z : Uint8) return Boolean;
   pragma Inline (SDL_VERSION_ATLEAST);

   --  This function gets the version of the dynamically linked SDL library.
   --  it should NOT be used to fill a version structure, instead you should
   --  use the Version inlined function.
   function Linked_Version return version_ptr;
   pragma Import (C, Linked_Version, "SDL_Linked_Version");


end SDL.Version;
