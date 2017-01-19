
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
--with Interfaces.C;
with SDL.Types; use SDL.Types;
with SDL.Keyboard;
with SDL.Active;
with SDL.Mouse;
with SDL.Joystick;

package SDL.Events is

   type Event_Type is new Interfaces.Unsigned_8;
   for Event_Type'Size use 8;
   --  pragma Convention (C, Event_Type);

   package I  renames Interfaces;
   package M  renames SDL.Mouse;
   package Jy renames SDL.Joystick;

   --  ------------------
   --  Orginal C Event enumerations
   --  ------------------

   --  Unused (do not remove)
   NOEVENT                   : constant Event_Type :=  0;
   --  Application loses/gains visibility
   ISACTIVEEVENT             : constant Event_Type :=  1;
   --  Keys pressed
   KEYDOWN                   : constant Event_Type :=  2;
   --  Keys released
   KEYUP                     : constant Event_Type :=  3;
   --  Mouse moved
   MOUSEMOTION               : constant Event_Type :=  4;
   --  Mouse button pressed
   MOUSEBUTTONDOWN           : constant Event_Type :=  5;
   --  Mouse button released
   MOUSEBUTTONUP             : constant Event_Type :=  6;
   --  Joystick axis motion
   JOYAXISMOTION             : constant Event_Type :=  7;
   --  Joystick trackball motion
   JOYBALLMOTION             : constant Event_Type :=  8;
   --  Joystick hat position change
   JOYHATMOTION              : constant Event_Type :=  9;
   --  Joystick button pressed
   JOYBUTTONDOWN             : constant Event_Type := 10;
   --  Joystick button released
   JOYBUTTONUP               : constant Event_Type := 11;
   --  User-requested quit
   QUIT                      : constant Event_Type := 12;
   --  System specific event
   ISSYSWMEVENT              : constant Event_Type := 13;
   --  Reserved for future use..
   EVENT_RESERVEDA           : constant Event_Type := 14;
   --  Reserved for future use..
   EVENT_RESERVEDB           : constant Event_Type := 15;
   --  User resized video mode
   VIDEORESIZE               : constant Event_Type := 16;
   --  Reserved for future use..
   EVENT_RESERVED1           : constant Event_Type := 17;
   --  Reserved for future use..
   EVENT_RESERVED2           : constant Event_Type := 18;
   --  Reserved for future use..
   EVENT_RESERVED3           : constant Event_Type := 19;
   --  Reserved for future use..
   EVENT_RESERVED4           : constant Event_Type := 20;
   --  Reserved for future use..
   EVENT_RESERVED5           : constant Event_Type := 21;
   --  Reserved for future use..
   EVENT_RESERVED6           : constant Event_Type := 22;
   --  Reserved for future use..
   EVENT_RESERVED7           : constant Event_Type := 23;

   --  Events USEREVENT through MAXEVENTS-1 are for your use

   ISUSEREVENT               : constant Event_Type := 24;
   --  This last event is only for bounding internal arrays
   --  It is the number of bits in the event mask datatype -- Uint32

   NUMEVENTS                 : constant Event_Type := 32;

   --  Predefined event masks

   type Event_Mask is mod 2**Integer (NUMEVENTS);

   ACTIVEEVENTMASK     : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (ISACTIVEEVENT)));
   KEYDOWNMASK         : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (KEYDOWN)));
   KEYUPMASK           : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (KEYUP)));
   MOUSEMOTIONMASK     : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (MOUSEMOTION)));
   MOUSEBUTTONDOWNMASK : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (MOUSEBUTTONDOWN)));
   MOUSEBUTTONUPMASK   : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (MOUSEBUTTONUP)));
   MOUSEEVENTMASK      : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (MOUSEMOTION))) or
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (MOUSEBUTTONDOWN))) or
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (MOUSEBUTTONUP)));
   JOYAXISMOTIONMASK   : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYAXISMOTION)));
   JOYBALLMOTIONMASK   : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYBALLMOTION)));
   JOYHATMOTIONMASK    : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYHATMOTION)));
   JOYBUTTONDOWNMASK   : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYBUTTONDOWN)));
   JOYBUTTONUPMASK     : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYBUTTONUP)));
   JOYEVENTMASK        : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYAXISMOTION))) or
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYBALLMOTION))) or
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYHATMOTION)))  or
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYBUTTONDOWN))) or
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (JOYBUTTONUP)));
   VIDEORESIZEMASK     : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (VIDEORESIZE)));
   QUITMASK            : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (QUIT)));
   SYSWMEVENTMASK      : constant Event_Mask :=
      Event_Mask (I.Shift_Left (I.Unsigned_32 (1), Integer (ISSYSWMEVENT)));

   ALLEVENTS           : constant Event_Mask := 16#FFFFFFFF#;

   --  Application visibility event structure
   type ActiveEvent is
      record
         --  the_type,         --  ISACTIVEEVENT
         the_type : Event_Type; --  ISACTIVEEVENT;
         gain,          --  Whether given states were gained or lost (1/0)
         state    : SDL.Active.Active_State; --  A mask of the focus states
      end record;
   pragma Convention (C, ActiveEvent);


   --  Keyboard event structure
   type KeyboardEvent is
      record
         the_type : Event_Type; --  KEYDOWN or KEYUP
         which    : Uint8;      --  The keyboard device index
         state    : Uint8;      --  PRESSED or RELEASED
         keysym   : aliased SDL.Keyboard.keysym;
      end record;
   pragma Convention (C, KeyboardEvent);

   --  Mouse motion event structure
   type MouseMotionEvent is
      record
         the_type : Event_Type; --  MOUSEMOTION
         which    : Uint8;      --  The mouse device index
         state    : Uint8;      --  The current state
         x, y     : Uint16;     --  The X/Y coordinates of the mouse
         xrel     : Sint16;     --  The relative motion in the X direction
         yrel     : Sint16;     --  The relative motion in the Y direction
      end record;
   pragma Convention (C, MouseMotionEvent);

   --  Mouse button event structure
   type MouseButtonEvent is
      record
         the_type : Event_Type;  --  MOUSEBUTTONDOWN or MOUSEBUTTONUP
         which    : Uint8;       --  The mouse device index
         button   : Uint8;       --  The mouse button index
         state    : M.Mouse_Button_State; --  PRESSED or RELEASED
         x, y     : Uint16;      --  The X/Y coordinates of the mouse at
                                 --  press time
      end record;
   pragma Convention (C, MouseButtonEvent);

   --  Joystick axis motion event structure
   type JoyAxisEvent is
      record
         the_type : Event_Type; --  JOYAXISMOTION
         which    : Uint8;      --  The joystick device index
         axis     : Uint8;      --  The joystick axis index
         value    : Sint16;     --  The axis value (range: -32768 to 32767)
      end record;
   pragma Convention (C, JoyAxisEvent);

   --  Joystick trackball motion event structure
   type JoyBallEvent is
      record
         the_type : Event_Type; --  JOYBALLMOTION
         which    : Uint8;      --  The joystick device index
         ball     : Uint8;      --  The joystick trackball index
         xrel     : Sint16;     --  The relative motion in the X direction
         yrel     : Sint16;     --  The relative motion in the Y direction
      end record;
   pragma Convention (C, JoyBallEvent);

   --  Joystick hat position change event structure
   type JoyHatEvent is
      record
         the_type : Event_Type;     --  JOYHATMOTION
         which    : Uint8;          --  The joystick device index
         hat      : Uint8;          --  The joystick hat index
         value    : Jy.HAT_State;   --  The hat position value
                                    --       8   1   2
                                    --       7   0   3
                                    --       6   5   4
                                    --  Note that zero means the POV is centered.
      end record;
   pragma Convention (C, JoyHatEvent);


   --  Joystick button event structure */
   type JoyButtonEvent is
      record
         the_type : Event_Type;  --  JOYBUTTONDOWN or JOYBUTTONUP
         which    : Uint8;       --  The joystick device index
         button   : Uint8;       --  The joystick button index
         state    : Uint8;       --  PRESSED or RELEASED
      end record;
   pragma Convention (C, JoyButtonEvent);


   --  The "window resized" event
   --  When you get this event, you are responsible for setting a new video
   --  mode with the new width and height.
   type ResizeEvent is
      record
         the_type : Event_Type; --  VIDEORESIZE
         w, h : C.int;          -- New width and height
      end record;
   pragma Convention (C, ResizeEvent);


   --  The "quit requested" event
   type QuitEvent is
      record
         the_type : Event_Type; -- QUIT
      end record;
   pragma Convention (C, QuitEvent);

   --  A user-defined event type
   type UserEvent is
      record
         the_type : Event_Type; --  USEREVENT through NUMEVENTS-1
         code     : C.int;      --  User defined event code
         data1    : void_ptr;   --  User defined data pointer
         data2    : void_ptr;   --  User defined data pointer
      end record;
   pragma Convention (C, UserEvent);

   type SysWMmsg_ptr is new System.Address;
   --  If you want to use this event, you should use SDL.Syswm
   type SysWMEvent is
      record
         the_type : Event_Type;
         msg      : SysWMmsg_ptr;
      end record;
   pragma Convention (C, SysWMEvent);


   type Event_Selection is (
      Is_Event_Type,
      Is_ActiveEvent,
      Is_KeyboardEvent,
      Is_MouseMotionEvent,
      Is_MouseButtonEvent,
      Is_JoyAxisEvent,
      Is_JoyBallEvent,
      Is_JoyHatEvent,
      Is_JoyButtonEvent,
      Is_ResizeEvent,
      Is_QuitEvent,
      Is_UserEvent,
      Is_SysWMEvent);

   --  General event structure
   type Event (Event_Selec : Event_Selection := Is_Event_Type) is
      record
         case Event_Selec is
            when Is_Event_Type       => the_type : Event_Type;
            when Is_ActiveEvent      => active   : ActiveEvent;
            when Is_KeyboardEvent    => key      : KeyboardEvent;
            when Is_MouseMotionEvent => motion   : MouseMotionEvent;
            when Is_MouseButtonEvent => button   : MouseButtonEvent;
            when Is_JoyAxisEvent     => jaxis    : JoyAxisEvent;
            when Is_JoyBallEvent     => jball    : JoyBallEvent;
            when Is_JoyHatEvent      => jhat     : JoyHatEvent;
            when Is_JoyButtonEvent   => jbutton  : JoyButtonEvent;
            when Is_ResizeEvent      => resize   : ResizeEvent;
            when Is_QuitEvent        => quit     : QuitEvent;
            when Is_UserEvent        => user     : UserEvent;
            when Is_SysWMEvent       => syswm    : SysWMEvent;
         end case;
      end record;
   pragma Convention (C, Event);
   pragma Unchecked_Union (Event);

   type Event_ptr is access all Event;
   pragma Convention (C, Event_ptr);

   --  -------------------
   --  Function prototypes
   --  -------------------

   --  Pumps the event loop, gathering events from the input devices.
   --  This function updates the event queue and internal input device state.
   --  This should only be run in the thread that sets the video mode.
   procedure PumpEvents;
   pragma Import (C, PumpEvents, "SDL_PumpEvents");

   --  Checks the event queue for messages and optionally returns them.
   --  If 'action' is ADDEVENT, up to 'numevents' events will be added to
   --  the back of the event queue.
   --  If 'action' is PEEKEVENT, up to 'numevents' events at the front
   --  of the event queue, matching 'mask', will be returned and will not
   --  be removed from the queue.
   --  If 'action' is GETEVENT, up to 'numevents' events at the front
   --  of the event queue, matching 'mask', will be returned and will be
   --  removed from the queue.
   --  This function returns the number of events actually stored, or -1
   --  if there was an error.  This function is thread-safe.

   type eventaction is new C.int;
   ADDEVENT  : constant := 0;
   PEEKEVENT : constant := 1;
   GETEVENT  : constant := 2;


   type Events_Array is array (Natural range <>) of Event;

   procedure PeepEventsVP (
      result : out C.int;
      events : in out Events_Array;
      numevents : C.int;
      action : eventaction;
      mask : Event_Mask);
   pragma Import (C, PeepEventsVP, "SDL_PeepEvents");
   pragma Import_Valued_Procedure (PeepEventsVP);

   --  From Ada this function is to be called only as
   --  ... := PeepEvents (null, 0, the_action, the_mask);
   --  in other cases use PeepEventsVP.
   function PeepEvents (
      events : Event_ptr;
      numevents : C.int;
      action : eventaction;
      mask : Event_Mask)
      return C.int;
   pragma Import (C, PeepEvents, "SDL_PeepEvents");

   --  pending events, or 0 if there are none available.  If 'event' is not
   --  NULL, the next event is removed from the queue and stored in that area.
   function PollEvent (the_event : access Event) return C.int;
   pragma Import (C, PollEvent, "SDL_PollEvent");

   --  Check the pending events. Doesn't remove them.
   function Poll_Event return C.int;
   pragma Inline (Poll_Event);

   --  A corresponding Valued Procedure
   procedure PollEventVP (result : out C.int; the_event : in out Event);
   pragma Import (C, PollEventVP, "SDL_PollEvent");
   pragma Import_Valued_Procedure (PollEventVP);


   --  Waits indefinitely for the next available event, returning 1, or 0
   --  if there was an error while waiting for events.  If 'event' is not
   --  NULL, the next event is removed from the queue and stored in that area.
   function  WaitEvent (event : Event_ptr) return C.int;
   procedure WaitEvent (event : Event_ptr);
   procedure WaitEvent (the_event : in out Event);
   pragma Import (C, WaitEvent, "SDL_WaitEvent");

   procedure Wait_Event (
      Result : out C.int;
      the_event : in out  Event);
   pragma Import (C, Wait_Event, "SDL_WaitEvent");
   pragma Import_Valued_Procedure (Wait_Event);

   function Wait_Any_Event return C.int;
   pragma Inline (Wait_Any_Event);

   --  Add an event to the event queue.
   --  This function returns 0, or -1 if the event couldn't be added to
   --  the event queue.  If the event queue is full, this function fails.
   function PushEvent (event : Event_ptr) return C.int;
   procedure PushEvent (event : Event_ptr);
   function PushEvent (the_event : Event) return C.int;
   procedure PushEvent (the_event : Event);
   pragma Import (C, PushEvent, "SDL_PushEvent");


   --  This function sets up a filter to process all events before they
   --  change internal state and are posted to the internal event queue.

   --  The filter is protypted as:
   type EventFilter_ptr is access function (event : Event_ptr) return C.int;
   pragma Convention (C, EventFilter_ptr);

   --  If the filter returns 1, then the event will be added to the internal
   --  queue. If it returns 0, then the event will be dropped from the queue,
   --  but the internal state will still be updated.  This allows selective
   --  filtering of dynamically arriving events.

   --  WARNING:  Be very careful of what you do in the event filter function,
   --            as it may run in a different thread!

   --  There is one caveat when dealing with the QUITEVENT event type.  The
   --  event filter is only called when the window manager desires to close the
   --  application window.  If the event filter returns 1, then the window will
   --  be closed, otherwise the window will remain open if possible.
   --  If the quit event is generated by an interrupt signal, it will bypass
   --  the internal queue and be delivered to the application at the next event
   --  poll.
   procedure SetEventFilter (filter : EventFilter_ptr);
   pragma Import (C, SetEventFilter, "SDL_SetEventFilter");

   --  Return the current event filter - can be used to "chain" filters.
   --  If there is no event filter set, this function returns NULL.
   function GetEventFilter return EventFilter_ptr;
   pragma Import (C, GetEventFilter, "SDL_GetEventFilter");

   --  This function allows you to set the state of processing certain events.
   --  If 'state' is set to IGNORE, that event will be automatically dropped
   --  from the event queue and will not event be filtered.
   --  If 'state' is set to ENABLE, that event will be processed normally.
   --  If 'state' is set to QUERY, EventState will return the
   --  current processing state of the specified event.
   QUERY   : constant  := -1;
   IGNORE  : constant  :=  0;
   DISABLE : constant  :=  0;
   ENABLE  : constant  :=  1;

   function EventState (
      the_type : Event_Type;
      state    : C.int) return Uint8;

   procedure EventState (
      the_type : Event_Type;
      state    : C.int);

   pragma Import (C, EventState, "SDL_EventState");

end SDL.Events;
