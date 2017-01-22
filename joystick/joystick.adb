
with Text_Io; use Text_io;

package body Joystick is
 -- type Mode_T is new  C.Int;
  type Size_T is new  C.Long; --Long_Integer;
  subtype Ssize_T is Size_T;
  type File_Id is new C.Int;

    JOYSTICK_DEVNAME : constant string := "/dev/input/js0";
    JS_EVENT_BUTTON  : constant C.Int  := 16#01#; 
    JS_EVENT_AXIS    : constant C.Int  := 16#02#;
    JS_EVENT_INIT    : constant C.Int  := 16#80#;
    Joy_Fd           : File_Id := -1;


   O_RDONLY     : constant C.Int := 0;
  -- O_NONBLOCK : constant C.Int := 2048;


  procedure Open_Joystick is
    subtype Path_Name_Type is String(1..14);
    My_Path : aliased Path_Name_Type := JOYSTICK_DEVNAME;
    function cOpen(Path  : access Path_Name_Type;
                   Flags : C.Int) return File_Id;
    pragma Import(C, cOpen, "open" );
  begin
    Joy_Fd := cOpen(My_Path'access, O_RDONLY);
    if Joy_Fd < 0 then
        raise Open_Failure with JOYSTICK_DEVNAME;
    end if;
  end Open_Joystick;


  ----------------------------------------------------
  procedure Close_Joystick is
    function cClose( File : File_Id ) return C.Int;
    pragma import( C, cClose ,"close" );
    Result : C.int := -1;
  begin
    Result := cClose(Joy_Fd);
    if Result < 0 then
        raise Close_Failure with JOYSTICK_DEVNAME;
    end if;
  end Close_Joystick;
-------------------------------------------------------

  procedure Read_Joystick_Event(Reading_Ok : out Boolean; Jse : out Js_Event) is
    Message_Buffer : aliased Js_Event;
    function cRead(Fd    : File_Id;
                   Buf   : access Js_Event;
                   Count : Size_T ) return Ssize_T;
    pragma import( C, cRead , "read");
    Num_Bytes : Ssize_T := -1;
  begin
    Reading_Ok := False;
    Num_Bytes := cRead(Joy_Fd, Message_Buffer'access, Size_T(Message_Buffer'Size));
    if Num_Bytes = Message_Buffer'Size then
       Reading_Ok := True; 
       Jse := Message_Buffer;
    end if;
  end Read_Joystick_Event;
  -------------------------------------------------------

  procedure Test is
    Ok  : Boolean := False;
    Jse : Js_Event;
  begin
     Open_Joystick;
     for i in 1 ..30 loop 
       Read_Joystick_Event (Ok, Jse);
       put_line("reading " & i'img  & " " & Ok'Img); 
       if Ok then
         Put_Line("time" & Jse.Time'img & 
                  " value " & Jse.Value'Img &
                  " c_type " & Jse.C_Type'Img &
                  " number " & Jse.Number'Img);
       end if; 
     end loop; 
     Close_Joystick; 
  end Test;
  -------------------------------------------------------

end Joystick;