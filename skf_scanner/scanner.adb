with Interfaces; use Interfaces;
with Interfaces.C;

with Simple_List_Class;
with Ada.Strings           ; use Ada.Strings;
--with Ada.Strings.Fixed     ; use Ada.Strings.Fixed;
with Ada.Strings.Unbounded ; use Ada.Strings.Unbounded;
with Text_Io;
with Ada.Directories;
with Ada.Calendar;

with GNAT.Lock_Files;

with Utils;
with Log;
with Thin_C_Bindings;
with Config;

--with Settings;

pragma Elaborate(Simple_List_Class);

package body Scanner is

  use type C.Int;
  String_Buffer_Last : constant Integer := 256;
--  subtype String_Buffer is String(1..String_Buffer_Last);
--  type String_Buffer_Pointer is access all String_Buffer;

  subtype Transaction_Type is String(1..4);
  subtype Release_Number_Type is String(1..4);
  subtype Transaction_Identity_Type is String(1..9);
  subtype Date_Type is String(1..8);
  subtype Time_Type is String(1..6);
  type Load_Id_Type is new Integer range -1 .. Integer'Last;
  subtype Destination_Type is String(1..10);

  type Conveyor_Type is range -1 .. 4;
  for  Conveyor_Type'Size use 8;

  Parallel_Port_Value : array (Conveyor_Type'Range) of Thin_C_Bindings.Byte
                                                            := (-1 => 2#00000001#,  -- Something is wrong
                                                                 0 => 2#00000000#,  -- Setting all bits low
                                                                 1 => 2#00000011#,  -- Conveyor 1
                                                                 2 => 2#00000101#,  -- Conveyor 2
                                                                 3 => 2#00001001#,  -- Conveyor 3
                                                                 4 => 2#00010001#); -- Conveyor 4

  type Scanner_Data_Type is record
    Transaction          : Transaction_Type          := (others => ' ');
    Release_Number       : Release_Number_Type       := (others => ' ');
    Transaction_Identity : Transaction_Identity_Type := (others => ' ');
    Date                 : Date_Type                 := (others => ' ');
    Time                 : Time_Type                 := (others => ' ');
    Load_Id              : Load_Id_Type              := Load_Id_Type'First;
    Destination          : Destination_Type          := (others => ' ');
  end record;

--  subtype Header_Type is String(1..10);
  type Route_Data_Type is record
--    Header              : Header_Type      := (others => ' ');
    Destination         : Destination_Type := (others => ' ');
    Conveyor            : Conveyor_Type    := Conveyor_Type'First;
  end record;

--  function Restore_Port(Port : C.Int) return C.Int;
--  pragma Import (C,Restore_Port, "Restore_Port");

  package Scan_List_Package is new Simple_List_Class(Scanner_Data_Type);
  package Route_List_Package is new Simple_List_Class(Route_Data_Type);
  The_List : Route_List_Package.List_Type := Route_List_Package.Create;

  --------------------------------------------------------------------------------

  task IO_Task is
    entry Startup;
  end IO_Task;

  --------------------------------------------------------------------------------
  task File_Task is
    entry Startup;
  end File_Task;

  --------------------------------------------------------------------------------
  task Route_Task is
    entry Startup(Route_File, The_Lock_File : Unbounded_String);
  end Route_Task;

  --------------------------------------------------------------------------------

  protected Scan_Handler is
    --------------------------------------------------------------------------------
    procedure Put_Load_Route_Connection(Scan_Data : in Scanner_Data_Type);
    --------------------------------------------------------------------------------
    entry Get(Load_Id     :  in Load_Id_Type;
              Item        : out Scanner_Data_Type;
              Found       : out Boolean);
    --------------------------------------------------------------------------------
    private 
      Number_Of_List_Entries : Natural := 0;
      Scan_List : Scan_List_Package.List_Type := Scan_List_Package.Create;
  end Scan_Handler;

  --------------------------------------------------------------------------------

  protected Route_Handler is
    --------------------------------------------------------------------------------
    function Is_Locked return Boolean;
    --------------------------------------------------------------------------------
    entry Lock;
    --------------------------------------------------------------------------------
    procedure Release;
    --------------------------------------------------------------------------------
    entry Get_Conveyor(Destination         :  in Destination_Type;
                       Conveyor            : out Conveyor_Type;
                       Found               : out Boolean);
    --------------------------------------------------------------------------------
    private
      Locked : Boolean := False;
  end Route_Handler;


 --------------------------------------------------------------------------------
 -----------------------Declarations Stop----------------------------------------
 --------------------------------------------------------------------------------


  --------------------------------------------------------------------------------
  -- scanner handling
  procedure Split(S :  in String;
                  L : out Load_Id_Type) is
    type Index_Type is (Start, Stop);
    Index : array (Index_Type'Range) of Integer := (others => Integer'First); 
  begin
    for i in S'Range loop -- find first digit and use it's index as start
      case S(i) is
        when '0' .. '9' =>
          Index(Start) := i;
          exit;
        when others => null;
      end case;
    end loop;

    for i in reverse S'Range loop -- find last digit and use it's index as stop
      case S(i) is
        when '0' .. '9' =>
          Index(Stop) := i;
          exit;
        when others => null;
      end case;
    end loop;

    begin 
      L := Load_Id_Type'Value(S (Index(Start) .. Index(Stop) ) );
    exception
      when E: others =>
        Log.Put(Utils.Tracebackinfo(E));
        L := -1;
    end;
  end Split;

  --------------------------------------------------------------------------------

  procedure Split(S :  in String;
                  R : out Route_Data_Type) is
  begin
    -- Trim Destination first?
    if S'Length < 13 then return; end if;
  --  R.Header              := S(1..10);
    R.Destination         := S(1..10);
    R.Conveyor := Conveyor_Type'Value(S(11..13));
  end Split;

  --------------------------------------------------------------------------------
  procedure Split(S :  in String;
                  R : out Scanner_Data_Type) is
  begin
    if S'Length < 51 then return; end if;
    -- Trim Destination first?
    R.Transaction          := S(1..4);
    R.Release_Number       := S(5..8);
    R.Transaction_Identity := S(9..17);
    R.Date                 := S(18..25);
    R.Time                 := S(26..31);
    R.Load_ID              := Load_Id_Type'Value(S(32..40));
    R.Destination          := S(41..50);
  end Split;

  --------------------------------------------------------------------------------

  protected body Scan_Handler is
    --------------------------------------------------------------------------------
    procedure Put_Load_Route_Connection(Scan_Data : in Scanner_Data_Type) is
    begin
      Scan_List_Package.Insert_At_Tail(Scan_List,Scan_Data);
      Number_Of_List_Entries := Number_Of_List_Entries +1;
    end Put_Load_Route_Connection;
    --------------------------------------------------------------------------------
    entry Get(Load_Id     :  in Load_Id_Type;
              Item        : out Scanner_Data_Type;
              Found       : out Boolean)
           when Number_Of_List_Entries > 0 is
      End_Of_List     : Boolean            := False;
      Local_Scan_Item : Scanner_Data_Type;
    begin
      Found := False;
      -- End_Of_List cannot happen here, due to Number_Of_List_Entries > 0 
      Scan_List_Package.Get_First(Scan_List, Local_Scan_Item, End_Of_List);
      loop
        exit when Found or else End_Of_List;
        if Local_Scan_Item.Load_Id = Load_Id then
          Item := Local_Scan_Item;
          Scan_List_Package.Delete(Scan_List);
          Number_Of_List_Entries := Number_Of_List_Entries -1;
          Found := True;
        end if;
        Scan_List_Package.Get_Next(Scan_List, Local_Scan_Item, End_Of_List);
      end loop;
    end Get;
  end Scan_Handler;

  --------------------------------------------------------------------------------

  protected body Route_Handler is
    --------------------------------------------------------------------------------
    function Is_Locked return Boolean  is
    begin
      return Locked;
    end Is_Locked;
    --------------------------------------------------------------------------------

    entry Lock when not Locked is
    begin
      Locked := True;
    end Lock;

    --------------------------------------------------------------------------------
    procedure Release is
    begin
      Locked := False;
    end Release;
    --------------------------------------------------------------------------------
    entry Get_Conveyor(Destination         :  in Destination_Type;
                       Conveyor            : out Conveyor_Type;
                       Found               : out Boolean)
           when not Locked is
      End_Of_List      : Boolean            := False;
      Local_Route_Item : Route_Data_Type;
    begin
      Locked := True;
      Found := False;
      Conveyor := Conveyor_Type'First;
      Route_List_Package.Get_First(The_List, Local_Route_Item, End_Of_List);
      -- End_Of_List cannot happen here, due to Number_Of_List_Entries > 0
      loop
        exit when Found or else End_Of_List;
        if Local_Route_Item.Destination = Destination then
          Conveyor := Local_Route_Item.Conveyor;
          Found := True;
        end if;
        Route_List_Package.Get_Next(The_List, Local_Route_Item, End_Of_List);
      end loop;
      Locked := False;
    end Get_Conveyor;
    --------------------------------------------------------------------------------
  end Route_Handler;

  --------------------------------------------------------------------------------

  function Check_Route(Scan_Item : in Scanner_Data_Type) return boolean is
    pragma Warnings(Off, Scan_Item);
    pragma Compile_Time_Warning(True, "Check_route");
  begin
    return True;
  end Check_Route;
  -----------------------------------------------------------------------------------

  task body IO_Task is
    subtype String_Buffer_Type is String(1..String_Buffer_Last);
    type String_Buffer_Pointer_Type is access all String_Buffer_Type;

    Fd, Num_Bytes : C.Int := 0;
    Buf : aliased String_Buffer_Type;
    Buf_Ptr : String_Buffer_Pointer_Type := Buf'Access;
    Scan_Item : Scanner_Data_Type;
    Conveyor  : Conveyor_Type;
    Local_Load : Load_Id_Type := Load_Id_Type'First;

    type Handler is (Scan, Route);
    Found : array (Handler'Range) of Boolean := (others => False);

    type Error_Flag_Type is (NO_ERROR, E_TRX, E_NOBAND, E_ROUTE);
    Error_Flag : Error_Flag_Type := Error_Flag_Type'First;

    function Do_Read(Port : C.Int; Buffer : String_Buffer_Pointer_Type) return C.Int;
    pragma import(C, Do_Read, "Do_Read");

    function Initialize_Port(Baudrate : C.Int; Device : Config.Serial_Port_Pointer_Type) return C.Int;
    pragma import(C, Initialize_Port, "Initialize_Port");

    use Ada.Directories;

  begin 
  -- Watch the serial port,
  -- and set correct value to the parallel port
    accept Startup do -- 8bit,no parity,1 stopbit is set in c-code
      Fd := Initialize_Port(Baudrate => Config.Baudrate, --9_600,
                            Device   => Config.Serial_Port_Pointer );
      Thin_C_Bindings.Get_Parallel_Port_Access;
    end Startup;

    loop
      Error_Flag := NO_ERROR;
      Buf := (Buf'Last => Ascii.Nul,
              others => ' ');
      Num_Bytes := Do_Read(Fd,Buf_Ptr);  --read blocking from serial port, hangs until CR or LF
      Split( Buf(1..Integer(Num_Bytes)) , Local_Load); --fix input
      Scan_Handler.Get(Local_Load, Scan_Item, Found(Scan)); -- find the load int the load/route connection list
      if Found(Scan) then
        Route_Handler.Get_Conveyor(Scan_Item.Destination,        -- get the conveyor for the route
                                   Conveyor,
                                   Found(Route));
      else 
        Log.Put("E_TRX Did not find route for load:" & Load_Id_Type'Image(Scan_Item.Load_Id));
        Error_Flag := E_TRX;
      end if;

      if Found(Route) then
          if not Check_Route(Scan_Item) then 
            Error_Flag := E_ROUTE;
            Log.Put("E_ROUTE Did not find route in setup:" & Scan_Item.Destination);
          end if;
      end if;

      if Found(Scan) and then not Found(Route) then
          Error_Flag := E_NOBAND;
        Log.Put("E_NOBAND Did not find conveyor for route:" & Scan_Item.Destination);
      end if;

      case Error_Flag is
        when NO_ERROR =>
          Thin_C_Bindings.Write_Parallel_Port(Value => Parallel_Port_Value(Conveyor));

       when E_TRX | E_NOBAND | E_ROUTE =>
          Thin_C_Bindings.Write_Parallel_Port(Value => Parallel_Port_Value(-1));
      end case;

      delay Config.Reset_PLC_Delay; -- reset port after a second
      Thin_C_Bindings.Write_Parallel_Port(Value => Parallel_Port_Value(0));

      if Found(Scan) then
        declare
          Single_Line_File : String := Config.Work_Directory & "/" & Scan_Item.Transaction_Identity & Config.Single_Line_File_Extension;
        begin 
          Delete_File(Single_Line_File);
        exception
          when E: others => 
            Log.Put("Got exeption when deleting: '" & Single_Line_File & "'"); 
            Log.Put(Utils.Tracebackinfo(E));
        end;
      end if;
    end loop;
  exception
    when E: others =>
      Log.Put(Utils.Tracebackinfo(E));
  end IO_Task;

  --------------------------------------------------------------------------------

  procedure Loop_Over_Files_With_Action (Wk_Dir                  : in String := "";
                                         To_Dir                  : in String := "";
                                         Pattern                 : in String := Config.Ftp_File_Pattern;
                                         Read_Line_Into_List     : in Boolean := False;
                                         Create_Single_Line_File : in Boolean := False;
                                         Move                    : in Boolean := False;
                                         Check_For_Growth        : in Boolean := False;
                                         Delete                  : in Boolean := False;
                                         Days                    : in Integer := 0) is
    use Ada.Directories;
    use Ada.Calendar;
    subtype String_Buffer_Type is String(1..String_Buffer_Last);

    Cwd                 : String                    := Current_Directory;
    Search_Files        : Search_Type;
    File_Entry          : Directory_Entry_Type;
    File                : Text_Io.File_Type; 
    Buf                 : String_Buffer_Type;
    Scan_Data           : Scanner_Data_Type;
    Len                 : Positive                  := 1;
    Error_In_File       : Boolean                   := False;
    Last_MF_Time        : Time;
    Ok_To_Move          : Boolean                   := False;
    Now                 : Time                      := Clock;

    ------------------------------------------------------------------------
    procedure Create_Single_Line(SD : in Scanner_Data_Type; Line : in String) is
      Single_Line_File : Text_Io.File_Type;
      Filename : String := Config.Work_Directory & "/" & SD.Transaction_Identity & Config.Single_Line_File_Extension;
    begin
      begin
        Text_Io.Create(File => Single_Line_File,
                       Name => Filename);
      exception
        when E: others =>
          Log.Put("Failed to create file: '" & Filename & "'");
          Log.Put(Utils.Tracebackinfo(E));
      end;
      begin
        Text_Io.Put_Line(Single_Line_File, Line);
      exception
        when E: others =>
          Log.Put("Failed to write to file: '" & Filename & "'");
          Log.Put(Utils.Tracebackinfo(E));
      end;
      begin
        Text_Io.Close(File => Single_Line_File);
      exception
        when E: others =>
          Log.Put("Failed to close file: '" & Filename & "'");
          Log.Put(Utils.Tracebackinfo(E));
      end;
    end Create_Single_Line;
    ------------------------------------------------------------------------
  begin
    Set_Directory(Wk_Dir);
    Start_Search(Search    => Search_Files,
                 Directory => Current_Directory,
                 Pattern   => Pattern,
                 Filter    => Filter_Type'(Directory     => False,
                                           Ordinary_File => True,
                                           Special_File  => False));
    loop
      exit when not More_Entries(Search_Files);
      Get_Next_Entry(Search_Files, File_Entry);
      ---------------------------------------------------------
      if Read_Line_Into_List then
        begin
          Error_In_File := False;
          begin
            Text_Io.Open(File => File,
                         Mode => Text_Io.In_File,
                         Name => Full_Name(File_Entry));
            Read_File_Loop :
            loop
              Buf := (Buf'Last => Ascii.Nul,
                      others => ' ');
              Text_Io.Get_Line(File,Buf,Len);
              Split(Buf(1..Len),Scan_Data);
              Scan_Handler.Put_Load_Route_Connection(Scan_Data);
              if Create_Single_Line_File then
                Create_Single_Line(Scan_Data, Buf(1..Len));
              end if;
            end loop Read_File_Loop;
          exception
            when Text_Io.End_Error  =>
              Text_Io.Close(File => File);
            when others  =>
              Error_In_File := True;
              Text_Io.Close(File => File);
          end;

          if Error_In_File then
            Rename(Full_Name(File_Entry), Config.Error_Directory & "/" & Simple_Name(File_Entry));
          else
            Rename(Full_Name(File_Entry), Config.Log_Directory & "/" & Simple_Name(File_Entry));
          end if;
        exception
          when E: others  =>
            Log.Put(Utils.Tracebackinfo(E));
        end;
      ---------------------------------------------------------
      elsif Move then
        begin
           Ok_To_Move := True;
           if Check_For_Growth then
             Last_MF_Time := Modification_Time(File_Entry);
             delay Config.File_Growth_Delay;
             Ok_To_Move := Last_MF_Time = Modification_Time(File_Entry); -- file has not grown in Config.File_Growth_Delay secs -> ok
           end if;
           if Ok_To_Move then
             Rename(Full_Name(File_Entry), To_Dir & "/" & Simple_Name(File_Entry));
           end if;
        exception
          when E: others =>
            Log.Put("Failed to move: '" & Full_Name(File_Entry) & "' to '" & To_Dir & "'");
            Log.Put(Utils.Tracebackinfo(E));
        end;
      ---------------------------------------------------------
      elsif Delete then
        begin
          if (Now - Modification_Time(File_Entry) > Days * 86_400.0) then
            Delete_File(Simple_Name(File_Entry));
          end if;
        exception
          when E: others =>
            Log.Put("Delete failed: '" & Simple_Name(File_Entry) & "'");
            Log.Put(Utils.Tracebackinfo(E));
        end;
      ---------------------------------------------------------
      end if;
    end loop;
    End_Search(Search_Files);
    Set_Directory(Cwd);
  end Loop_Over_Files_With_Action;

  ---------------------------------------------------------------------------------------

  procedure Create_Non_Existing_Directories is
    use Ada.Directories;
  begin
    if not Exists(Config.Work_Directory) then
      Create_Directory(Config.Work_Directory);
    end if;

    if not Exists(Config.Log_Directory) then
      Create_Directory(Config.Log_Directory);
    end if;

    if not Exists(Config.Error_Directory) then
      Create_Directory(Config.Error_Directory);
    end if;

    if not Exists(Config.Hourly_Log_File_Directory) then
      Create_Directory(Config.Hourly_Log_File_Directory);
    end if;
  end Create_Non_Existing_Directories;

  --------------------------------------------------------------------------------

  task body File_Task is
  -- once a minute, watch a given directory for files from host.
  -- Make sure the file is fully transfered, by watching its size,
  -- move it to Working_Dir.
  -- Parse it and split/put anything into the Scan_Handler list
  -- move the file to log dir/error dir
  -- Create the directories if they do not exist
  -- Also delete files older than 14 days  in log directory
  --  and delete files older than 90 days in error directory
  begin 
    accept Startup do
      begin
        Create_Non_Existing_Directories;
      exception
        when E: others =>
          Log.Put(Utils.Tracebackinfo(E));
      end; 

      begin
      --Read_Files_From_Work; The single liners already created (*.wrk)
        Loop_Over_Files_With_Action(Wk_Dir              => Config.Work_Directory,
                                    Read_Line_Into_List => True,
                                    Pattern             => Config.Single_Line_File_Pattern);
      exception
        when E: others =>
          Log.Put(Utils.Tracebackinfo(E));
      end; 
    end Startup;
    loop
      begin
--      Check_For_New_Files an move them from ftp -> work; Multiline files (*.dat)
        Loop_Over_Files_With_Action(Wk_Dir           => Config.Incoming_FTP_Directory,
                                    To_Dir           => Config.Work_Directory,
                                    Pattern          => Config.Ftp_File_Pattern,
                                    Move             => True,
                                    Check_For_Growth => True);
--      Treat_New_Files Read into lost, and create single line files; (*.dat multiline -> *.wrk singelline)
        Loop_Over_Files_With_Action(Wk_Dir                  => Config.Work_Directory,
                                    Read_Line_Into_List     => True,
                                    Pattern                 => Config.Ftp_File_Pattern,
                                    Create_Single_Line_File => True);

--      Cleanup(Config.Log_Directory, 14);
        Loop_Over_Files_With_Action(Wk_Dir  => Config.Log_Directory,
                                    Pattern => Config.Ftp_File_Pattern,
                                    Delete  => True,
                                    Days    => 14);

 --     Cleanup(Config.Error_Directory, 90);
        Loop_Over_Files_With_Action(Wk_Dir  => Config.Error_Directory,
                                    Pattern => Config.Ftp_File_Pattern,
                                    Delete  => True,
                                    Days    => 90);
      exception
        when E: others =>
          Log.Put(Utils.Tracebackinfo(E));
      end;

      delay Config.Check_Files_Delay;
    end loop;
  exception
    when E: others =>
      Log.Put(Utils.Tracebackinfo(E));
  end File_Task;

  --------------------------------------------------------------------------------

  task body Route_Task is
  -- Watch a given file for route/parallellport values. 
  -- Administer a list with connections between routes and what value
  -- to put to Parallel port
  -- Empty the list every 5 minutes, an fill with new values
    Local_Lock_File, Local_Route_File : Unbounded_String;
    Data_File                         : Text_Io.File_Type;
    subtype String_Buffer_Type is String(1..String_Buffer_Last);
    Buf : String_Buffer_Type := (others => ' ');
    Len : Positive := 1;
    Route_Data : Route_Data_Type;
    Locked : Boolean := False;
  begin
    accept Startup(Route_File, The_Lock_File : Unbounded_String) do
      Local_Route_File := Route_File;
      Local_Lock_File  := The_Lock_File;
    end Startup;

    Route_Loop :
    loop
      Locked := False;
      begin  --if lockfile exists, cgi is updating, -> wait and try again
        GNAT.Lock_Files.Lock_File(To_String(Local_Lock_File)); -- will hang enad retry every 2 sec, if file exists
        Locked := True;
      exception
        when GNAT.Lock_Files.Lock_Error => --lockfile cannot be locked! 
          Locked := False;
          Log.Put("Cannot lock the lockfile '" & To_String(Local_Lock_File) & "'");
      end;

      if Locked then
        begin
          Text_Io.Open(File => Data_File,
                       Mode => Text_Io.In_File,
                       Name => To_String(Local_Route_File));
          --Empty and refill the list
          Route_Handler.Lock;
          Route_List_Package.Remove_All(The_List);
          begin
            Read_File_Loop :
            loop
              Text_Io.Get_Line(Data_File,Buf,Len);
              Split(Buf(1..Len),Route_Data);
              Route_List_Package.Insert_At_Tail(The_List, Route_Data);
            end loop Read_File_Loop;
          exception
            when Text_Io.End_Error =>
              begin
                Text_Io.Close(File => Data_File);
              exception
                when E: others =>
                  Log.Put(Utils.Tracebackinfo(E));
              end;
          end;
          Route_Handler.Release;
          GNAT.Lock_Files.Unlock_File(To_String(Local_Lock_File));
        exception
          when E: others =>
            Log.Put(Utils.Tracebackinfo(E));
            if Route_Handler.Is_Locked then
              Route_Handler.Release;
            end if;
            begin
              if Text_Io.Is_Open(Data_File) then
                Text_Io.Close(File => Data_File);
              end if;
            exception
              when E: others =>
                Log.Put(Utils.Tracebackinfo(E));
            end;
        end;
      end if;
      delay Config.Route_File_Check_Delay;
    end loop Route_Loop;
  exception
    when E: others =>
      Log.Put(Utils.Tracebackinfo(E));
  end Route_Task;

  --------------------------------------------------------------------------------

  procedure Startup is
    function Pid return C.Int;
    pragma import (C, Pid, "getpid");
    Pid_File : Text_Io.File_Type;
    procedure Daemon;
    pragma import (C, Daemon, "become_daemon");
  begin
	Daemon; -- fork/exit first
    begin
      Text_Io.Create(File => Pid_File,
                     Mode => Text_Io.Out_File,
                     Name => Config.Pid_File);
    exception
      when E: others =>
        Log.Put("Failed to write pidfile at startup. Permissions perhaps?");
        Log.Put("Pidfile: '" & Config.Pid_File & "'");
        Log.Put(Utils.Tracebackinfo(E));
    end;
    Text_Io.Put_Line(Pid_File,C.Int'Image(Pid));
    Text_Io.Close(Pid_File);

    File_Task.Startup;
    Route_Task.Startup(To_Unbounded_String(Config.Data_File),
                       To_Unbounded_String(Config.Lock_File));
    IO_Task.Startup;
  exception
    when E: others =>
      Log.Put(Utils.Tracebackinfo(E));
  end Startup;

  --------------------------------------------------------------------------------

end Scanner;


