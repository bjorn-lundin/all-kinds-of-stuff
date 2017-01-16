
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
--  The SDL image library was written in C  by:                      --
--  Sam Lantinga - www.libsld.org                                    --
--  **************************************************************** --
package body SDL_Image is

   --  =================================================
   function Load (file : String) return V.Surface_ptr is
      File_CString : aliased C.char_array := C.To_C (file);
   begin
      return Load (CS.To_Chars_Ptr (File_CString'Unchecked_Access));
   end Load;

   --  =================================================
   function LoadTyped_RW (src : RW.RWops_ptr;
                          freesrc : C.int;
                          the_type : String)
                          return V.Surface_ptr is
      Type_CString : aliased C.char_array := C.To_C (the_type);
   begin
      return LoadTyped_RW (src,
                           freesrc,
                           CS.To_Chars_Ptr (Type_CString'Unchecked_Access));
   end LoadTyped_RW;

   --  =================================================
end SDL_Image;
