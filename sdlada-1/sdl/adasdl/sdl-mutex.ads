
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

----------------------------------------------
--  This package is here for compatibility  --
--  with SDL. The Ada programming language  --
--  has far better portable multithread and --
--  sincronization mechanisms.              --
----------------------------------------------

with SDL.Types; use SDL.Types;
with Interfaces.C;
with System;
package SDL.Mutex is

   package C renames Interfaces.C;

   MUTEX_TIMEDOUT : constant := 1;
   MUTEX_MAXWAIT  : constant := 16#FFFFFFFF#;


   ------------------------
   --  Mutex subprograms --
   ------------------------

   --  A pointer to the SDL mutex structure defined
   --  in SDL_mutex.c
   type mutex_ptr is new System.Address;

   null_mutex_ptr : constant mutex_ptr := mutex_ptr (System.Null_Address);

   --  Create a mutex, initialized unlocked
   function CreateMutex return mutex_ptr;
   pragma Import (C, CreateMutex, "SDL_CreateMutex");

   --  Lock the mutex (Returns 0 or -1 on error)
   function mutexP (mutex : mutex_ptr) return C.int;
   pragma Import (C, mutexP, "SDL_mutexP");

   --  The same as MutexP
   function LockMutex (mutex : mutex_ptr) return C.int;
   pragma Inline (LockMutex);

   --  Unlock the mutex (Returns 0 or -1 on error)
   function mutexV (mutex : mutex_ptr) return C.int;
   pragma Import (C, mutexV, "SDL_mutexV");

   --  The same as MutexV
   function UnlockMutex (mutex : mutex_ptr) return C.int;
   pragma Inline (UnlockMutex);

   --  Destroy a mutex
   procedure DestroyMutex (mutex : mutex_ptr);
   pragma Import (C, DestroyMutex, "SDL_DestroyMutex");

   ---------------------------
   -- Semaphore subprograms --
   ---------------------------

   --  A pointer to the SDL semaphore structure defined
   --  in SDL_sem.c
   type sem_ptr is new System.Address;

   --  Create a semaphore, initialized with value, returns
   --  NULL on failure.
   function CreateSemaphore (initial_value : Uint32)
      return sem_ptr;
   pragma Import (C, CreateSemaphore, "SDL_CreateSemaphore");

   --  Destroy a semaphore
   procedure DestroySemaphore (sem : sem_ptr);
   pragma Import (C, DestroySemaphore, "SDL_DestroySemaphore");

   --  This function suspends the calling thread until the semaphore
   --  pointed to by sem has a positive count. It then atomically
   --  decreases the semaphore count.
   function SemWait (sem : sem_ptr) return C.int;
   procedure SemWait (sem : sem_ptr);
   pragma Import (C, SemWait, "SDL_SemWait");

   --  Non-blocking variant of Sem_Wait, returns 0 if the wait
   --  succeeds, SDL_MUTEX_TIMEDOUT if the wait would block, and -1
   --  on error.
   function SemTryWait (sem : sem_ptr) return C.int;
   pragma Import (C, SemTryWait, "SDL_SemTryWait");

   --  Varian of Sem_Wait with timeout in miliseconds, returns 0
   --  if the wait succeeds, SDL_MUTEX_TIMEDOUT if the whait does
   --  not succeed in the allotted time, and -1 in error.
   --  On some platforms this function is implemented by looping
   --  with a delay of 1 ms, and so should be avoided if possible.
   function SemWaitTimeout (sem : sem_ptr; ms : Uint32)
      return C.int;
   pragma Import (C, SemWaitTimeout, "SDL_SemWaitTimeout");

   --  Atomically increases the semaphore's count (not blocking),
   --  returns 0, or -1 on error.
   function SemPost (sem : sem_ptr) return C.int;
   procedure SemPost (sem : sem_ptr);
   pragma Import (C, SemPost, "SDL_SemPost");

   --  Returns the current count of the semaphore
   function SemValue (sem : sem_ptr) return Uint32;
   pragma Import (C, SemValue, "SDL_SemValue");

   ------------------------------------
   --  Condition variable functions  --
   ------------------------------------

   --  The SDL condition variable structure, defined in SDL_cond.c
   type cond_ptr is new System.Address;

   --  Create a condition variable
   function CreateCond return cond_ptr;
   pragma Import (C, CreateCond, "SDL_CreateCond");

   --  Destroy a condition variable
   procedure DestroyCond (cond : cond_ptr);
   pragma Import (C, DestroyCond, "SDL_DestroyCond");

   --  Restart one of the threads  that are waiting on the
   --  condition variable, returns 0, or -1 on error.
   function CondSignal (cond : cond_ptr) return C.int;
   pragma Import (C, CondSignal, "SDL_CondSignal");

   --  Restart all threads that are waiting on the condition
   --  variable, returns 0, or -1 on error.
   function CondBroadcast (cond : cond_ptr) return C.int;
   pragma Import (C, CondBroadcast, "SDL_CondBroadcast");

   --  Wait on the condition variable, unlocking the provided
   --  mutex. The mutex must be locked before entering this
   --  function! returns 0 when it is signaled, or -1 on error.
   function CondWait (cond : cond_ptr; mut : mutex_ptr)
     return C.int;
   pragma Import (C, CondWait, "SDL_CondWait");

   --  Waits for at most 'ms' milliseconds, and returns 0 if the
   --  condition variable is signaled, SDL_MUTEX_TIMEDOUT if the
   --  condition is not signaled in the allocated time, and -1
   --  on error.
   --  On some platforms this function is implemented by looping
   --  with a delay of 1 ms, and so should be avoided if possible.
   function CondWaitTimeout (
      cond : cond_ptr;
      mut : mutex_ptr;
      ms : Uint32)
      return C.int;
   pragma Import (C, CondWaitTimeout, "SDL_CondWaitTimeout");

end SDL.Mutex;
