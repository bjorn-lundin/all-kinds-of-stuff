with Unchecked_Conversion;
with Unchecked_Deallocation;
with Sv_m.Utils;     use Sv_m.Utils;
with Sv_m.Constants; use Sv_m.Constants;
with Sv_m.C_Import;  use Sv_m.C_Import;
with System;
with Text_io;

package body Sv_m is
  use C; -- declared in spec

  gQueue_Identity      : C.Int := C.Int'First;

  gThis_Process        : Process_Type;
  gThis_Process_Is_Set : Boolean := False;


  --For caching which Queue that is associated with which process
  type Process_Info_Type is record
    Process        : Process_Type;
    Queue_Identity : C.Int;
  end record;
  type Node;
  type Tree is access Node;
  type Node is record
    Process_Info : Process_Info_Type;
    Left,Right   : Tree;
  end record;
  List : Tree := null;
  Type Inserts_Type is mod 500;  -- should suffice??
  gCounter : Inserts_Type := Inserts_Type'First;
------------------------------------------------------------------
--start list handling of cached entries
------------------------------------------------------------------
  procedure Free is new Unchecked_Deallocation(Object => Node,
                                               Name   => Tree);

  --Empty list every Inserts_Type'Last time. There should rarely be more than
  --Inserts_Type'Last clients and fellow processes! Proberly old and dead
  --that just makes a mess of the list.
  procedure Delete_List(The_List : in out Tree) is
  begin
    if The_list /= null then
      Delete_List(The_List.Left);
      Delete_List(The_List.Right);
      Free(The_List);
    end if;
  end Delete_List;
-----------------------------------------------------
  procedure Insert_Into_List(The_List    : in out Tree;
                             The_Process : in Process_Type;
                             The_Queue   : in C.Int) is
  begin
    if The_List = null then
      The_List := new Node'((The_Process,The_Queue), null, null);
    elsif The_Process.Process_Name < The_List.Process_Info.Process.Process_Name then
      Insert_Into_List(The_List.Left, The_Process, The_Queue);
    else
      Insert_Into_List(The_List.Right, The_Process, The_Queue);
    end if;
  end Insert_Into_List;

-----------------------------------------------------

  function Get_Cached_Message_Queue_For_Process(The_List    : in Tree;
                                                The_Process : in Process_Type) return C.int is
  begin
    if The_List = null then
      return C.Int'First;                                              -- No list or Process not in list
    elsif The_Process.Process_Name = The_List.Process_Info.Process.Process_Name then
      return The_List.Process_Info.Queue_Identity;                     --This is what we came for
    elsif The_Process.Process_Name < The_List.Process_Info.Process.Process_Name then
      return Get_Cached_Message_Queue_For_Process(The_List.Left, The_Process); -- try again with left tree
    else
      return Get_Cached_Message_Queue_For_Process(The_List.Right, The_Process); -- try again with right tree
    end if;
  end Get_Cached_Message_Queue_For_Process;
-----------------------------------------------------
--stop list handling of cached entries
-----------------------------------------------------

-----------------------------------------------------
--start message queue handling procs
-----------------------------------------------------
-- The timer task used in C_Receive
  task type Timer_Task is
    entry Start(Timeout : Duration);
    entry Stop;
  end Timer_Task;

  task body Timer_Task is
    LocalTimeout    : Duration      := 0.0;
    TimeOut_Message : Message_Type;
    Result          : C.Int         := C.Int'First;
  begin
    accept Start(Timeout : Duration) do
      LocalTimeout := Timeout;
--      Log("Timer Started:" & Timeout'Img, Always);
    end Start;

    select
      accept Stop do
--        Log("Timer Stop nice ",Always);
        null;
      end Stop;
    or
      delay LocalTimeout;
 --       Log("Timer Delay",Always);
        Timeout_Message.Message_Head.Identity := MESSAGE_TIMEOUT_IDENTITY;
        TimeOut_Message.Message_Head.Sender   := This_Process;
        TimeOut_Message.Message_Head.Receiver := This_Process;
	--setting Type_Of_Message to abs(MESSAGE_TIMEOUT_IDENTITY) makes it possible to
	--get all time outs in receive, without disturbing other messages
	TimeOut_Message.Type_Of_Message := C.Long(abs(MESSAGE_TIMEOUT_IDENTITY));
        Result := C_Send(gQueue_Identity, TimeOut_Message);
        case Result is
          when 0..C.Int'Last => null;
          when C_ERROR       =>
            Perror("Timer_task C_Send = C_ERROR");
            raise Send_Failure;
          when others        =>
            Perror("Timer_task C_Senf < C_ERROR");
            raise Unknown_Send_Failure;
        end case;
    end select;
  end Timer_Task;
-----------------------------------------------------------------
--stop message queue handling procs
-----------------------------------------------------
-----------------------------------------------------
--start helper functions
-----------------------------------------------------

  function Get_Message_Queue_Id_For_Process(Process : in Process_Type) return C.Int is
    Key  : Key_t := Key_t'First;
    Queue_Identity : C.Int := C.Int'First;
  begin
--    Queue_Identity := Get_Cached_Message_Queue_For_Process(List, Process); --Look in list first
--    if Queue_Identity = C.Int'First then --Not in list, look at disk
      Key := FileToKey(Process_Directory & Process.Process_Name, PROJECT);
      Queue_Identity := C_Create_Message_Queue(Key);
--       if (Queue_Identity >= 0) then --Q created OK, insert into list for future use
--         gCounter := gCounter +1;
--         if gCounter = Inserts_Type'Last then
--           Delete_List(List);  -- create new list when list is too big; might contain dead processes
--         end if;
--         Insert_Into_List(List, Process, Queue_Identity);
--       end if;
--    end if;
    if (Queue_Identity < 0) then
      Log("Results from FileToKey and C_create", Always);
      Log("Key " & Key'Img, Always);
      Log("Queue_Identity: " & Queue_Identity'Img, Always);
      Perror("GetQueueIdForProcess");
      raise No_Such_Message_Queue;
    end if;

    return Queue_Identity;
  end Get_Message_Queue_Id_For_Process;


-----------------------------------------------------
--stop helper functions
-----------------------------------------------------


-----------------------------------------------------
--the generic packages
--Just do conversion to byte array. Convert back when unpacking
---------------------------------------------------
  package body Generic_io is
    Data_Length    : constant Natural := Natural (Data_Type'Size/System.Storage_Unit);
    subtype Byte_Array_Message is Byte_Array (1..Data_Length);
    subtype Byte_Array_2 is Byte_Array (1..2);
    ----------------------------------------------------
    function  Pack (Data : in  Data_Type) return Message_Type is
      function Data_To_Byte_Array is new
             Unchecked_Conversion (Data_Type, Byte_Array_Message);
      Message : Message_Type;
    begin
      if (Data'Size > (MAX_DATA_LENGHT * System.Storage_unit))  then
        Log ("Message size is: " & Natural'Image(Data'size/System.Storage_Unit), Always);
        Log("MAX_DATA_LENGHT is: " & Positive'Image(MAX_DATA_LENGHT), Always);
        raise Too_Big_Message;
      end if;
      --sender and receiver are set in SEND
      Message.Message_Head.Identity := Identity;
      Message.Message_Body.Data (1..Integer (Data_Length)) := Data_To_Byte_Array(Data);
      Message.Message_Body.Data_Length := Data_Length;
      return Message;
    end Pack;
    ----------------------------------------------------
    function  Unpack (Message : Message_Type) return Data_Type is
      function Message_To_Data is new Unchecked_Conversion(Byte_Array_Message, Data_Type);
    begin
      return Message_To_Data (Message.Message_Body.Data (1..Data_Length));
    end Unpack;
    ----------------------------------------------------

    procedure Send (Receiver   : in  Process_Type;
                    Data       : in  Data_Type) is
    begin
      Send(Receiver,Pack(Data));  --add no of elements
    end Send;
  end Generic_io;

---------------------------------------------------
-- Functions and Procedures in the spec below
---------------------------------------------------
  procedure Create_Message_Queue(Key : in Key_t) is
    Result : C.Int := 0;
  begin
    Result := C_Create_Message_Queue(Key);
    if (Result >= 0) then
      gQueue_Identity := Result;
    else
      Log("Result from Create: " & Result'Img, Always);
      Perror("Create");
      raise Create_Message_Queue_Failure;
    end if;
  end Create_Message_Queue;

---------------------------------------------------

  procedure Send(Process : in Process_Type;
                 Message : in Message_Type) is
    Result,Queue_Identity : C.Int := C.Int'First;
    Local_Message         : Message_type := Message;
  begin
    Queue_Identity := Get_Message_Queue_Id_For_Process(Process);

    Local_Message.Message_Head.Sender   := This_Process;
    Local_Message.Message_Head.Receiver := Process;
    Result := C_Send(Queue_Identity, Local_Message);
    case Result is
      when 0..C.Int'Last => null;
      when C_ERROR       =>
        Perror("Send = C_ERROR");
        raise Send_Failure;
      when others        =>
        Perror("Send < C_ERROR");
        raise Unknown_Send_Failure;
    end case;
  end Send;

---------------------------------------------------

  procedure Receive(Message : out Message_Type;
                    Timeout : in Duration := 0.0) is
    Key             : Key_t := Key_t'First;
    Timeout_Message : Message_Type;
  begin
    if gQueue_Identity < 0 then
      Key := FileToKey(Process_directory & This_Process.Process_name, PROJECT) ;
      Create_Message_Queue(Key);
    end if;

    if Timeout > 0.0 then
      declare
        Timer : Timer_Task;
      begin
        Timer.Start(Timeout);                                       --start Timer
        Message := C_Receive(gQueue_Identity, 0);   -- wait until message arrives, or timeout-msg
        Log("Receive Timeout-Got a message:" & Identity(Message)'Img, Always);
        begin
          Timer.Stop;
        exception --Task might be dead...
          when Tasking_Error => null;
        end;
      end;

      if Identity(Message) = MESSAGE_TIMEOUT_IDENTITY then
        raise Sv_m.Timeout;
      end if;

      --remove timeouts the task sent AFTER real message arrived
      --but BEFORE task was terminated AND timer went out
      --By using IPC_NOWAIT, the call will return at once. By specifying the message type as
      --MESSAGE_TIMEOUT_IDENTITY we only look for timeout messages.
      --When there are no more timeouts, the call will fail, causing C_Receive to
      --raise Receive_Failure, and set errno to ENOMSG
      loop
        begin
          Timeout_Message := C_Receive(gQueue_Identity, C.Long(MESSAGE_TIMEOUT_IDENTITY),IPC_NOWAIT);
        exception
          when Receive_Failure  =>
            Log("Receive-Clearing timeouts got Receive_Failure, Error_Number=" & Error_Number'Img);
            exit when Error_Number = ENOMSG;
--          when Storage_Error => --Storage_Error????????????????????
--            Log("Receive-Clearing timeouts got Storage_Error, Error_Number=" & Error_Number'Img);
--            exit when Error_Number = ENOMSG;
--          when Others =>
--            Log("Receive-Clearing timeouts got Others, Error_Number=" & Error_Number'Img);
        end;
      end loop;
    else
      Message := C_Receive(gQueue_Identity, Timeout_Message.Type_Of_Message);   -- wait forever
      Log("Receive No-Timeout-Got a message");
    end if;

  end Receive;

---------------------------------------------------

  function Identity(Message : in Message_Type) return Identity_Type is
  begin
    return Message.Message_Head.Identity;
  end Identity;

------------------------------------------------------

  function Sender(Message : in Message_Type) return Process_Type is
  begin
    return Message.Message_Head.Sender;
  end Sender;

------------------------------------------------------

  function This_Process return Process_Type is
  begin
    if not gThis_Process_Is_Set then
      declare
        This_Process_Name : constant String := Environment("PROCESS_NAME",False);
      begin
        if This_Process_Name'Length > 0 then
          gThis_Process.Process_Name(1..This_Process_Name'Length) := This_Process_Name;
          gThis_Process_Is_Set := True;
          Log("Process name: " & gThis_Process.Process_Name, Always);
        else
          declare
            Pid_String : constant String := Pid_t'Image(Pid);
          begin                                             --image leaves a blank for a possible '-'
            gThis_Process.Process_Name(1..Pid_String'Length-1) := Pid_String(2..Pid_String'Length);
            Create_Pid_File(Pid_String(2..Pid_String'Length));
            gThis_Process_Is_Set := True;
            Log("Pid: " & gThis_Process.Process_Name, Always);
          end;
        end if;
      end;
    end if;
    return gThis_Process;
  end This_Process;

-----------------------------------------------------
end Sv_m;
