
with Text_Io; use Text_io;

package body Joystick is
 -- type Mode_T is new  C.Int;
  type Size_T is new  C.Long; --Long_Integer;
  subtype Ssize_T is Size_T;
  type File_Id is new C.Int;

    JOYSTICK_DEVNAME : constant string := "/dev/input/js0" & Ascii.Nul;
    Joy_Fd           : File_Id := -1;


   O_RDONLY     : constant C.Int := 0;
  -- O_NONBLOCK : constant C.Int := 2048;


  procedure Open_Joystick is
    subtype Path_Name_Type is String(1..15);
    My_Path : aliased Path_Name_Type := JOYSTICK_DEVNAME;
    function cOpen(Path  : access Path_Name_Type;
                   Flags : C.Int) return File_Id;
    pragma Import(C, cOpen, "open" );
  begin
    Put_Line("Open - start");
    Joy_Fd := cOpen(My_Path'access, O_RDONLY);
    Put_Line("Open - Joy_Fd" & Joy_Fd'Img);
    if Joy_Fd < 0 then
        raise Open_Failure with JOYSTICK_DEVNAME;
    end if;
    Put_Line("Open - done");
  end Open_Joystick;


  ----------------------------------------------------
  procedure Close_Joystick is
    function cClose( File : File_Id ) return C.Int;
    pragma import( C, cClose ,"close" );
    Result : C.int := -1;
  begin
    Put_Line("Close - start");
    Result := cClose(Joy_Fd);
    Put_Line("Close - Result" & Result'Img);
    if Result < 0 then
        raise Close_Failure with JOYSTICK_DEVNAME;
    end if;
    Put_Line("Close - done");
  end Close_Joystick;
------------------------------------------------------

  procedure Read_Joystick_Event(Reading_Ok : out Boolean; Jse : out Js_Event) is
    Message_Buffer : aliased Js_Event;
    function cRead(Fd    : File_Id;
                   Buf   : access Js_Event;
                   Count : Size_T ) return Ssize_T;
    pragma import( C, cRead , "read");
    Num_Bytes : Ssize_T := -1;
    Num_Bytes_To_Read : constant Size_T := Size_T(Message_Buffer'Size/8);
  begin
    Reading_Ok := False;
    Put_Line("read - start");

    Num_Bytes := cRead(Joy_Fd, Message_Buffer'access, Num_Bytes_To_Read);
    Put_Line("read - Num_Bytes" & Num_Bytes'Img);
    if Num_Bytes = Num_Bytes_To_Read then
       Reading_Ok := True;
       Jse := Message_Buffer;
    end if;
    Put_Line("read - done");
  end Read_Joystick_Event;
  -------------------------------------------------------

  procedure Test is
    Ok  : Boolean := False;
    Jse : Js_Event;
  begin
     Open_Joystick;
     loop
       Read_Joystick_Event (Ok, Jse);
       Put("reading " & Ok'Img & " ");
       if Ok then
         Put_Line("time" & Jse.Time'img &
                  " value " & Jse.Value'Img &
                  " c_type " & Jse.C_Type'Img &
                  " number " & Jse.Number'Img);
       else
         New_Line;
       end if;
       exit when Jse.Value=0 and Jse.Number=8 and Jse.C_Type=1;
     end loop;
     Close_Joystick;
  end Test;
  -------------------------------------------------------

end Joystick;
