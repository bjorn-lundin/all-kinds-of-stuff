
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

with System;
with Interfaces.C;

package Lib_C is

   package C  renames Interfaces.C;

   function memset (
      BLOCK : System.Address;
      Ch : C.int;
      SIZE : C.size_t)
      return System.Address;

   procedure memset (
      BLOCK : System.Address;
      Ch : C.int;
      SIZE : C.size_t);
   pragma Import (C, memset);

   procedure Mem_Set (
      BLOCK : System.Address;
      Ch : C.int;
      SIZE : C.size_t);
   pragma Import (C, Mem_Set, "memset");

   --  function malloc (SIZE : C.size_t)
   --     return System.Address;
   --  pragma Import (C, malloc);

   --  This procedure is necessary to deallocate
   --  valid data created inside de SDL C library.
   procedure free (PTR : System.Address);
   pragma Import (C, free);

   type sighandler_t is access procedure (signum : C.int);
   pragma Convention (C, sighandler_t);

   function signal (
      signum : C.int; -- Ex: System.OS_Interface.SIGKILL
      handler : sighandler_t)
      return sighandler_t;
   pragma Import (C, signal);

   procedure Set_Signal (
      signum : C.int; -- Ex: System.OS_Interface.SIGKILL
      handler : sighandler_t);
   pragma Import (C, Set_Signal, "signal");

   function Raise_Signal (sig : C.int) return C.int;
   pragma Import (C, Raise_Signal, "raise");

   procedure Raise_The_Signal (sig : C.int);
   pragma Import (C, Raise_The_Signal, "raise");

end Lib_C;
