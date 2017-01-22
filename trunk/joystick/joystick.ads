

with Interfaces.C; use Interfaces.C;

package Joystick is

  package C renames Interfaces.C;


  -- event timestamp in milliseconds
   type js_event is record
      time : aliased unsigned;  -- joystick.h:33
      value : aliased short;  -- joystick.h:34
      c_type : aliased unsigned_char;  -- joystick.h:35
      number : aliased unsigned_char;  -- joystick.h:36
   end record;
   pragma Convention (C_Pass_By_Copy, js_event);  -- joystick.h:32
   for js_event'size use 64;

  -- value
  -- event type
  -- axis/button number
   type wwvi_js_event_button_array is array (0 .. 10) of aliased int;
   type wwvi_js_event is record
      button : aliased wwvi_js_event_button_array;  -- joystick.h:40
      stick1_x : aliased int;  -- joystick.h:41
      stick1_y : aliased int;  -- joystick.h:42
      stick2_x : aliased int;  -- joystick.h:43
      stick2_y : aliased int;  -- joystick.h:44
   end record;
   pragma Convention (C_Pass_By_Copy, wwvi_js_event);  -- joystick.h:39


--extern int open_joystick();
  procedure Open_Joystick;
  Open_Failure : exception;
--extern int read_joystick_event(struct js_event *jse);
  procedure Read_Joystick_Event(Reading_Ok : out Boolean; Jse : out Js_Event) ;
--extern void close_joystick();
  procedure Close_Joystick;
  Close_Failure : exception;
--extern int get_joystick_status(struct wwvi_js_event *wjse);

  procedure Test ;

end Joystick;



