
with Text_Io; use Text_io;

package body Joystick is
 -- type Mode_T is new  C.Int;
  type Size_T is new  C.Long; --Long_Integer;
  subtype Ssize_T is Size_T;
  type File_Id is new C.Int;

  Joystick_Devname : constant string := "/dev/input/js0" & Ascii.Nul;
  Joy_Fd           : File_Id := -1;


  O_RDONLY     : constant C.Int := 0;
  -- O_NONBLOCK : constant C.Int := 2048;


  procedure Open is
    subtype Path_Name_Type is String(1..15);
    My_Path : aliased Path_Name_Type := Joystick_Devname;
    function cOpen(Path  : access Path_Name_Type;
                   Flags : C.Int) return File_Id;
    pragma Import(C, cOpen, "open" );
  begin
    Joy_Fd := cOpen(My_Path'access, O_RDONLY);
    if Joy_Fd < 0 then
        raise Open_Failure with Joystick_Devname;
    end if;
  end Open;
  ----------------------------------------------------
  procedure Close is
    function cClose( File : File_Id ) return C.Int;
    pragma import( C, cClose ,"close" );
    Result : C.int := -1;
  begin
    Result := cClose(Joy_Fd);
    if Result < 0 then
        raise Close_Failure with Joystick_Devname;
    end if;
  end Close;
------------------------------------------------------

  procedure Read_Event(Reading_Ok : out Boolean; Jse : out Js_Event) is
    Message_Buffer : aliased Js_Event;
    function cRead(Fd    : File_Id;
                   Buf   : access Js_Event;
                   Count : Size_T ) return Ssize_T;
    pragma import( C, cRead , "read");
    Num_Bytes : Ssize_T := -1;
    Num_Bytes_To_Read : constant Size_T := Size_T(Message_Buffer'Size/8);
  begin
    Reading_Ok := False;
    Num_Bytes := cRead(Joy_Fd, Message_Buffer'access, Num_Bytes_To_Read);
    if Num_Bytes = Num_Bytes_To_Read then
       Reading_Ok := True;
       Jse := Message_Buffer;
    end if;
  end Read_Event;
  -------------------------------------------------------
  procedure Test is
    Ok  : Boolean := False;
    Jse : Js_Event;
  begin
     Open;
     loop
       Read_Event (Ok, Jse);
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
     Close;
  end Test;
  -------------------------------------------------------

end Joystick;
