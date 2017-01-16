
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
with SDL.Types; use SDL.Types;

package SDL.Timer is

   --  This is the OS scheduler timeslice, in milliseconds
   TIMESLICE  : constant := 10;
   --  This is the maximum resolution of the SDL timer on all platforms
   RESOLUTION : constant := 10; --  Experimentally determined

   --  Get the number of milliseconds since the SDL library initialization.
   --  Note that this value wraps if the program runs for more than ~49 days.
   function GetTicks return Uint32;
   pragma Import (C, GetTicks, "SDL_GetTicks");

   --  Wait a specified number of milliseconds before returning
   procedure SDL_Delay (ms : Uint32);
   pragma Import (C, SDL_Delay, "SDL_Delay");

   --  Function pointer prototype for the timer callback function
   type TimerCallback_ptr is
      access function (interval : Uint32) return Uint32;
   pragma Convention (C, TimerCallback_ptr);

   --  Set a callback to run after the specified number of milliseconds has
   --  elapsed. The callback function is passed the current timer interval
   --  and returns the next timer interval.  If the returned value is the
   --  same as the one passed in, the periodic alarm continues, otherwise a
   --  new alarm is scheduled.  If the callback returns 0, the periodic alarm
   --  is cancelled.

   --  To cancel a currently running timer, call SetTimer(0, NULL);

   --  The timer callback function may run in a different thread than your
   --  main code, and so shouldn't call any functions from within itself.

   --  The maximum resolution of this timer is 10 ms, which means that if
   --  you request a 16 ms timer, your callback will run approximately 20 ms
   --  later on an unloaded system.  If you wanted to set a flag signaling
   --  a frame update at 30 frames per second (every 33 ms), you might set a
   --  timer for 30 ms:
   --       SetTimer((33/10)*10, flag_update);

   --  If you use this function, you need to pass INIT_TIMER to SDL_Init().

   --  Under UNIX, you should not use raise or use SIGALRM and this function
   --  in the same program, as it is implemented using setitimer().  You also
   --  should not use this function in multi-threaded applications as signals
   --  to multi-threaded apps have undefined behavior in some implementations.
   function SetTimer (
      interval : Uint32;
      callback : TimerCallback_ptr)
      return C.int;

   procedure SetTimer (
      interval : Uint32;
      callback : TimerCallback_ptr);

   pragma Import (C, SetTimer, "SDL_SetTimer");


   --  New timer API, supports multiple timers
   --  Written by Stephane Peter <megastep@lokigames.com>

   --  Function prototype for the new timer callback function.
   --  The callback function is passed the current timer interval and returns
   --  the next timer interval.  If the returned value is the same as the one
   --  passed in, the periodic alarm continues, otherwise a new alarm is
   --  scheduled.  If the callback returns 0, the periodic alarm is cancelled.
   type NewTimerCallback_ptr is
      access function (
                interval : Uint32;
                param    : System.Address)
                return Uint32;
   pragma Convention (C, NewTimerCallback_ptr);

   --  Definition of the timer ID type
   type TimerID is new System.Address;

   Null_TimerID : constant TimerID := TimerID (System.Null_Address);

   --  Add a new timer to the pool of timers already running.
   --  Returns a timer ID, or NULL when an error occurs.
   function AddTimer (
      interval : Uint32;
      callback : NewTimerCallback_ptr;
      param    : System.Address)
      return TimerID;
   pragma Import (C, AddTimer, "SDL_AddTimer");

   --  Remove one of the multiple timers knowing its ID.
   --  Returns a boolean value indicating success.
   function RemoveTimer (t : TimerID) return SDL_bool;
   pragma Import (C, RemoveTimer, "SDL_RemoveTimer");
end SDL.Timer;
