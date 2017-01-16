
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

package body SDL.RWops is

   -------------
   -- RWclose --
   -------------

   function RWclose
     (ctx : RWops_ptr)
      return C.int
   is
   begin
      return ctx.close (ctx);
   end RWclose;

   ------------
   -- RWread --
   ------------

   function RWread
     (ctx     : RWops_ptr;
      ptr     : void_ptr;
      size    : C.int;
      n       : C.int)
      return C.int
   is
   begin
      return ctx.read (ctx, ptr, size, n);
   end RWread;

   ------------
   -- RWSeek --
   ------------

   function RWSeek
     (ctx     : RWops_ptr;
      offset  : C.int;
      whence  : C.int)
      return C.int
   is
   begin
      return ctx.seek (ctx, offset, whence);
   end RWSeek;

   ------------
   -- RWtell --
   ------------

   function RWtell
     (ctx : RWops_ptr)
      return C.int
   is
   begin
      return ctx.seek (ctx, 0, C.int (C_Streams.SEEK_CUR));
   end RWtell;

   -------------
   -- RWwrite --
   -------------

   function RWwrite
     (ctx     : RWops_ptr;
      ptr     : void_ptr;
      size    : C.int;
      n       : C.int)
      return C.int
   is
   begin
      return ctx.write (ctx, ptr, size, n);
   end RWwrite;

   --  ======================================
   function RW_From_File (
      file : String;
      mode : String)
      return RWops_ptr is
      File_CString : aliased C.char_array := C.To_C (file);
      Mode_CString : aliased C.char_array := C.To_C (mode);
   begin
      return RWFromFile (CS.To_Chars_Ptr (File_CString'Unchecked_Access),
                         CS.To_Chars_Ptr (Mode_CString'Unchecked_Access));
   end RW_From_File;

   --  ======================================

end SDL.RWops;

