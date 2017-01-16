
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
with SDL.Types; use SDL.Types;

package SDL.Thread is

   --  The SDL thread structure, defined in SDL_thread.c
   type SDL_Thread_ptr is new System.Address;
   null_SDL_Thread_ptr : constant SDL_Thread_ptr :=
      SDL_Thread_ptr (System.Null_Address);

   type Thread_Func_Type is
      access function (param_ptr : System.Address)
                return C.int;
   pragma Convention (C, Thread_Func_Type);

   --  Create a thread
   function CreateThread (
      fn : Thread_Func_Type;
      data : System.Address)
      return SDL_Thread_ptr;
   pragma Import (C, CreateThread, "SDL_CreateThread");

   --  Get the 32-bit thread identifier for the current thread
   function ThreadID return Uint32;
   pragma Import (C, ThreadID, "SDL_ThreadID");

   --  Get the 32-bit thread identifier for the specified thread,
   --  equivalent to ThreadID if the specified thread is NULL.
   function GetThreadID (thread : SDL_Thread_ptr) return Uint32;
   pragma Import (C, GetThreadID, "SDL_GetThreadID");

   --  Wait for a thread to finish.
   --  The return code for the thread function is placed in the area
   --  pointed to by 'status', if 'status' is not NULL.
   procedure WaitThread (
      thread : SDL_Thread_ptr;
      status : int_ptr);
   pragma Import (C, WaitThread, "SDL_WaitThread");

   --  Forcefully kill a thread without worrying about its state
   procedure KillThread (thread : SDL_Thread_ptr);
   pragma Import (C, KillThread, "SDL_KillThread");

end SDL.Thread;
