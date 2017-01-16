
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
with Interfaces.C;
with Interfaces.C.Strings;
with Interfaces.C_Streams;
with SDL.Types; use SDL.Types;

--  This is a set of routines from SDL lib that doesn't
--  have a dependency from the SDL Ada package.
package SDL.RWops is

   package C renames Interfaces.C;
   package C_Streams renames Interfaces.C_Streams;
   package CS renames Interfaces.C.Strings;

   type void_ptr is new System.Address;
   type FILE_ptr is new C_Streams.FILEs;

   type RWops;
   type RWops_ptr is access all RWops;

   --  Seek to 'offset' relative to whence, one of stdio's
   --  whence values: SEEK_SET, SEEK_CUR, SEEK_END
   --  returns the finnal offset in the data source.
   type Seek_Type is access function (
                       context : RWops_ptr;
                       offset  : C.int;
                       whence  : C.int) return C.int;
   pragma Convention (C, Seek_Type);

   --  Read up to 'num' objects each of size 'objsize' from
   --  the data source to the ares pointed by 'ptr'.
   --  Returns number of objects read, or -1 if the read failed.
   type Read_Type is access function (
                       context : RWops_ptr;
                       ptr     : void_ptr;
                       size    : C.int;
                       maxnum : C.int) return C.int;
   pragma Convention (C, Read_Type);

   --  Write exactly 'num' objects each of size 'objsize' from
   --  the area pointed by 'ptr' to data source.
   --  Returns 'num', or -1 if the write failed.
   type Write_Type is access function (
                       context : RWops_ptr;
                       ptr     : void_ptr;
                       size    : C.int;
                       num     : C.int) return C.int;
   pragma Convention (C, Write_Type);

   --  Close and free an allocated SDL_FSops structure.
   type Close_Type is access function (
                       context : RWops_ptr) return C.int;
   pragma Convention (C, Close_Type);


   type Stdio_Type is
      record
         autoclose : C.int;
         fp : FILE_ptr;
      end record;
   pragma Convention (C, Stdio_Type);

   type Uint8_ptr is access all Uint8;

   type Mem_Type is
      record
         base,
         here,
         stop : Uint8_ptr;
      end record;
   pragma Convention (C, Mem_Type);

   type Unknown_Type is
      record
         data1 : void_ptr;
      end record;
   pragma Convention (C, Unknown_Type);

   type Hidden_Select_Type is (Is_Stdio, Is_Mem, Is_Unknown);
   type Hidden_Union_Type (Hidden_Select : Hidden_Select_Type := Is_Stdio) is
      record
         case Hidden_Select is
            when Is_Stdio => stdio : Stdio_Type;
            when Is_Mem => mem : Mem_Type;
            when Is_Unknown => unknown : Unknown_Type;
         end case;
      end record;
   pragma Convention (C, Hidden_Union_Type);
   pragma Unchecked_Union (Hidden_Union_Type);


   --  This is the read/write operation structure -- very basic */
   type RWops is
      record
         seek       : Seek_Type;
         read       : Read_Type;
         write      : Read_Type;
         close      : Close_Type;
         type_union : Uint32;
         hidden     : Hidden_Union_Type;
      end record;

   function RWFromFile (
        file : CS.chars_ptr;
        mode : CS.chars_ptr)
        return RWops_ptr;
   pragma Import (C, RWFromFile, "SDL_RWFromFile");

   function RW_From_File (
      file : String;
      mode : String)
      return RWops_ptr;
   pragma Inline (RW_From_File);

   function RWFromFP (
        file      : FILE_ptr;
        autoclose : C.int)
        return RWops_ptr;
   pragma Import (C, RWFromFP, "SDL_RWFromFP");

   function RWFromMem (
        mem  : void_ptr;
        size : C.int)
        return RWops_ptr;
   pragma Import (C, RWFromMem, "SDL_RWFromMem");

   function AllocRW return RWops_ptr;
   pragma Import (C, AllocRW, "SDL_AllocRW");

   procedure FreeRW (
        area : RWops_ptr);
   pragma Import (C, FreeRW, "SDL_FreeRW");

   function RWSeek (
        ctx     : RWops_ptr;
        offset  : C.int;
        whence  : C.int)
        return C.int;
   pragma Inline (RWSeek);

   function RWtell (
        ctx : RWops_ptr)
        return C.int;
   pragma Inline (RWtell);

   function RWread (
        ctx     : RWops_ptr;
        ptr     : void_ptr;
        size    : C.int;
        n       : C.int)
        return C.int;
   pragma Inline (RWread);

   function RWwrite (
        ctx     : RWops_ptr;
        ptr     : void_ptr;
        size    : C.int;
        n       : C.int)
        return C.int;
   pragma Inline (RWwrite);

   function RWclose (
        ctx : RWops_ptr)
        return C.int;
   pragma Inline (RWclose);

end SDL.RWops;
