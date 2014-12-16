
with Ada.Command_Line.Environment;
with Text_io;

package body Sv_m.Utils is
-- change this to something suitable to your system!!!!!!!!!!!!
--  "/home/bnl/processes/";
-- this is found in environment variable 'PID_FILE_DIRECTORY';
  gProcess_directory : String(1..255) := (others => ' ');
  gProcess_directory_is_set : Boolean := False;
  gProcess_directory_length : Natural := 0;

  --  Global_debug        : Boolean :=  True;
  Global_debug          : Boolean :=  False;

-----------------------------------
  function Getpid return Pid_t;
  pragma Import (C, getpid);

  function Pid return Pid_t is
  begin
    return Getpid;
  end pid;

-----------------------------------
  procedure C_perror(Text : in String);
  pragma    Import(C,C_perror,"perror");

  procedure Perror(Text : in String) is
  begin
    C_perror(Text & Ascii.NUL);
  end Perror;

----------------------------------------------------

  function Environment(Str : in String; Raise_Exception : Boolean := True) return String is
    use Ada.Command_Line.Environment;
    Error : constant String :=  "NO_SUCH_ENV: " & str;
    No_Of_Evironment_Variables : Natural := Environment_Count;
  begin

    if No_Of_Evironment_Variables > 0 then
      for i in 1..No_Of_Evironment_Variables loop
        declare
          Temp : constant String := Environment_Value(i);
        begin
          for j in Temp'Range loop
            if Temp(j) = '=' then
              if Temp(Temp'First ..j-1) = Str then
                return Temp(j+1 .. Temp'Last);
              else
                exit;
              end if;
            end if;
          end loop;
        end;
      end loop;
    end if;
    Log("Environment-" & Error, Always);
    if Raise_Exception then
      raise No_Such_Environment_Variable;
    else
      return "";
    end if;  
  end Environment;
----------------------------------------------------------
  function Process_Directory return String is
  begin
    if not gProcess_directory_is_set then
      declare
        Pd : constant String := Environment("PID_FILE_DIRECTORY");
      begin
        if Pd(Pd'Last) /= '/' then
          gProcess_directory_length := Pd'Length + 1;
          gProcess_directory(1..gProcess_directory_length) := Pd & '/';
          gProcess_directory_is_set := True;
        else
          gProcess_directory_length := Pd'Length;
          gProcess_directory(1..gProcess_directory_length) := Pd;
          gProcess_directory_is_set := True;
        end if;
      end;
    end if;
    return gProcess_directory(1..gProcess_directory_length);
  end Process_Directory;

-----------------------------------------------------

  procedure Log(To_log : in String; When_to_log : in Debug_level_type := Global) is
  begin
    if Global_debug or (When_to_log = Always) then
      Text_io.Put_line(To_log);
    end if;
  end Log;
---------------------------------------------------
  procedure Create_Pid_File(File_Name : in String) is
    Pid_File : Text_io.File_Type;
  begin
    Text_io.Create(File => Pid_File,
                   Name => Process_Directory & File_Name);
    Text_io.Close (File => Pid_File);
  end Create_Pid_File;

---------------------------------------------------

--int msgctl ( int msqid, int cmd, struct msqid_ds *buf )
  function Check_Queue(Q_Id : in C.Int; Cmd : in C.Int) return C.Int is
    type Message_Queue_Identity_Ds_Pointer_Type is access all Message_Queue_Identity_Ds_Type;

    Buf : aliased Message_Queue_Identity_Ds_Type;
    Buf_Ptr : Message_Queue_Identity_Ds_Pointer_Type := Buf'Access;

    function C_Msgctl(Q_Id : in C.Int; Cmd : in C.Int;
                      Buffert : in Message_Queue_Identity_Ds_Pointer_Type ) return C.Int;
    pragma import(C, C_Msgctl, "msgctl");

    Result : C.Int := C.Int'First;
  begin

    Result := C_Msgctl(Q_Id, Cmd, Buf_Ptr);
    Log("Check_Queue-Result:" & Result'Img,Always);
    if Result = C_ERROR then
      Perror("Check_Queue");
    else
      Log("Check_Queue-Msg_Stime: " & Buf_Ptr.all.Msg_Stime'Img, Always);
      Log("Check_Queue-Unused1:   " & Buf_Ptr.all.Unused1'Img, Always);
      Log("Check_Queue-Msg_Rtime: " & Buf_Ptr.all.Msg_Rtime'Img, Always);
      Log("Check_Queue-Unused2:   " & Buf_Ptr.all.Unused2'Img, Always);
      Log("Check_Queue-Msg_Ctime: " & Buf_Ptr.all.Msg_Ctime'Img, Always);
      Log("Check_Queue-Unused3:   " & Buf_Ptr.all.Unused3'Img, Always);
      Log("Check_Queue-Msg_Cbytes:" & Buf_Ptr.all.Msg_Cbytes'Img, Always);
      Log("Check_Queue-Msg_Qnum:  " & Buf_Ptr.all.Msg_Qnum'Img, Always);
      Log("Check_Queue-Msg_Qbytes:" & Buf_Ptr.all.Msg_Qbytes'Img, Always);
      Log("Check_Queue-Msg_Lspid: " & Buf_Ptr.all.Msg_Lspid'Img, Always);
      Log("Check_Queue-Msg_Lrpid: " & Buf_Ptr.all.Msg_Lrpid'Img, Always);
      Log("Check_Queue-Unused4:   " & Buf_Ptr.all.Unused4'Img, Always);
      Log("Check_Queue-Unused5:   " & Buf_Ptr.all.Unused5'Img, Always);
    end if;
    return Result;
  end Check_Queue;


--     Msg_Stime  : Time_t := Time_t'First;                   --time of last msgsnd command
--     Unused1    : C.Unsigned_Long := C.Unsigned_Long'First;
--     Msg_Rtime  : Time_t := Time_t'First;                   --time of last msgrcv command
--     Unused2    : C.Unsigned_Long := C.Unsigned_Long'First;
--     Msg_Ctime  : Time_t := Time_t'First;                   --time of last change
--     Unused3    : C.Unsigned_Long := C.Unsigned_Long'First;
--     Msg_Cbytes : C.Unsigned_Long := C.Unsigned_Long'First; --current number of bytes on queue
--     Msg_Qnum   : C.Unsigned_Long := C.Unsigned_Long'First; --number of messages currently on queue
--     Msg_Qbytes : C.Unsigned_Long := C.Unsigned_Long'First; --max number of bytes allowed on queue
--     Msg_Lspid  : Pid_t := Pid_t'First;                     --pid of last msgsnd()
--     Msg_Lrpid  : Pid_t := Pid_t'First;                     --pid of last msgrcv()
--     Unused4    : C.Unsigned_Long := C.Unsigned_Long'First;
--     Unused5    : C.Unsigned_Long := C.Unsigned_Long'First;

  function Error_Number return C.Int is
    errno : C.Int;
    pragma import(C,errno);
  begin
    return Errno;
  end Error_Number;



end sv_m.utils;
