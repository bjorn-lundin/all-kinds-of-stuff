

with Interfaces.C; use Interfaces.C;

package Joystick is

  package C renames Interfaces.C;

  Js_Event_Button         : constant C.Unsigned_Char  := 16#01#;
  Js_Event_Axis           : constant C.Unsigned_Char  := 16#02#;
  Js_Event_Init           : constant C.Unsigned_Char  := 16#80#;
  Js_Event_Init_Button    : constant C.Unsigned_Char  := Js_Event_Init + Js_Event_Button;
  Js_Event_Init_Axis      : constant C.Unsigned_Char  := Js_Event_Init + Js_Event_Axis;


  -- event timestamp in milliseconds
  type Js_Event is record
    Time   : aliased Unsigned;  -- joystick.h:33
    Value  : aliased Short;  -- joystick.h:34
    C_Type : aliased Unsigned_Char;  -- joystick.h:35
    Number : aliased Unsigned_Char;  -- joystick.h:36
  end record;
  pragma Convention (C_Pass_By_Copy, Js_Event);  -- joystick.h:32
  for Js_Event'Size use 64;

  ---- value
  ---- event type
  ---- axis/button number
  --type Wwvi_Js_Event_Button_Array is array (0 .. 10) of aliased Int;
  --type Wwvi_Js_Event is record
  --  Button   : aliased Wwvi_Js_Event_Button_Array;  -- joystick.h:40
  --  Stick1_X : aliased Int;  -- joystick.h:41
  --  Stick1_Y : aliased Int;  -- joystick.h:42
  --  Stick2_X : aliased Int;  -- joystick.h:43
  --  Stick2_Y : aliased Int;  -- joystick.h:44
  --end record;
  --pragma Convention (C_Pass_By_Copy, Wwvi_Js_Event);  -- joystick.h:39


--extern int open_joystick();
  procedure Open;
  Open_Failure : exception;
--extern int read_joystick_event(struct js_event *jse);
  procedure Read_Event(Reading_Ok : out Boolean; Jse : out Js_Event) ;
--extern void close_joystick();
  procedure Close;
  Close_Failure : exception;
--extern int get_joystick_status(struct wwvi_js_event *wjse);

  procedure Test ;

end Joystick;
