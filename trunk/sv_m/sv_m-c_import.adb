with System;
with Unchecked_Conversion;

package body Sv_m.C_Import is


  type I4 is range 0 .. 2**32 - 1;
  for I4'Size use 32;

  U_R : constant I4 := 8#400#;
  U_W : constant I4 := 8#200#;
  U_X : constant I4 := 8#100#;
  G_R : constant I4 := 8#040#;
  G_W : constant I4 := 8#020#;
  G_X : constant I4 := 8#010#;
  O_R : constant I4 := 8#004#;
  O_W : constant I4 := 8#002#;
  O_X : constant I4 := 8#001#;

  PERMISSIONS : constant I4 := U_R + U_W + G_R + G_W + O_R ;
-----------------------------------------------------
  function C_Send(Queue_Identity : in C.Int;
                  Message        : in Message_Type) return C.Int is

    type Message_Pointer_Type is access all Message_Type;

    Tmp_Msg     : aliased Message_Type := Message;
    Tmp_Msg_Ptr : Message_Pointer_Type := Tmp_Msg'Access;
    Local_Size : C.Int := 0;

    function C_Msgsnd(Msqid : C.Int; Msgp   : Message_Pointer_Type;
                      Msgsz : C.Int; Msgflg : C.Int) return C.Int;
    pragma Import (C, C_Msgsnd, "msgsnd");
  begin
-- All 'size are in bits. There are System.Storage_Unit bits in a byte
-- (message_type'size + header'size + Data_Length'size)/System.Storage_Unit + lenght of actual data
    Local_Size := C.Int((C.Long'Size + Message_Head_Type'Size + Integer'Size)/System.Storage_Unit +
                         Message.Message_Body.Data_Length);
    return C_Msgsnd(Queue_Identity,Tmp_Msg_Ptr,Local_Size,0);

  end C_Send;
-----------------------------------------------------

  function C_Create_Message_Queue(Key    : in Key_t;
                                  Create : in Boolean := True)  return C.Int is

    function C_Msgget(Key : Key_t; Msgflg : C.Int) return C.Int;
    pragma Import (C, C_Msgget, "msgget");

    type Bit_Array_32 is array (0..31) of Boolean;
    pragma pack(Bit_Array_32);

    function To_Bit_array_32 is new Unchecked_Conversion(I4, Bit_Array_32);
    function To_I4           is new Unchecked_Conversion(Bit_Array_32, I4);

    Ba_Permissions, Ba_IPC_CREATE  : Bit_Array_32;
    I4_Permissions                 : I4 := 0;
  begin
-- by or-ing the permissions set on the Queue with IPC_CREAT, we
-- create the Queue if it doesn't exist, rather than getting a trappable error

    if Create then
      Ba_Permissions   := To_Bit_Array_32(PERMISSIONS);
      Ba_IPC_CREATE    := To_Bit_Array_32(I4(IPC_CREAT));
      I4_Permissions   := To_I4(Ba_Permissions or Ba_IPC_CREATE);
    else -- just get the queue, fail if not exists
      I4_Permissions := I4(PERMISSIONS);
    end if;
    return C_Msgget(Key, C.Int(I4_Permissions));
  end C_Create_Message_Queue;

---------------------------------------------------

  function C_Receive(Queue_Identity : in C.Int;
                     Msg_Type       : in C.Long;
                     Msg_Flag       : in C.Int := 0) return Message_Type is

    type Message_Pointer_Type is access all Message_Type;

    function C_Msgrcv(Msqid   : C.Int; Msgpointer : Message_Pointer_Type;
                      Msgsize : C.Int; Msgtype    : C.Long;
                      Msgflag : C.Int)                 return C.Int;
    pragma Import (C, C_Msgrcv, "msgrcv");

    Tmp_Msg     : aliased Message_Type;
    Tmp_Msg_Ptr : Message_Pointer_Type := Tmp_Msg'Access;
    Result      : C.Int                := C.Int'First;
  begin
    Result := C_Msgrcv(Queue_Identity, Tmp_Msg_Ptr,
                       C.Int((Message_type'Size + C.Long'Size)/System.Storage_Unit),
                       Msg_Type, Msg_Flag);

    if (Result >= 0) then
--      Log("Identity: " & Identity(Tmp_Msg_Ptr.all)'Img,Always);
      return Tmp_Msg_Ptr.all;
    else
      if Msg_Type /= C.Long(MESSAGE_TIMEOUT_IDENTITY) then
        Log("Result from C_Receive: " & Result'Img, Always);
        Log("Msg_Type: " & Msg_Type'Img, Always);
        Perror("Receive");
      end if;
      raise Receive_Failure;
    end if;
  end C_Receive;

------------------------------------------------------
-- int msgctl ( int msqid, int cmd, struct msqid_ds *buf )
--   tmpbuf : aliased Message_Queue_Identity_Ds_Type;
--   buf : Message_Queue_Identity_Ds_Type_Pointer_Type := tmpbuf'Access;
--  function C_Msgctl(Queue_Identity : in C.Int; Command : in C.Int;
--                    Buf : Message_Queue_Identity_Ds_Type_Pointer_Type) return C.Int is
--  begin
--  end C_Msgctl;
------------------------------------------------------
  function FileToKey(Pathname     : in String;
                     Proj         : in C.Int;
                     Debug_Errors : in Boolean := True) return Key_t is
    Result    : Key_t := Key_t'First;
    LocalPath : String(1..Pathname'Length + 1) := (others => ' ');
-- key_t ftok ( char *pathname, int proj )
    function C_Ftok(Pathname : in String; Proj : in C.Int) return Key_t;
    pragma Import(C,C_Ftok,"ftok");

  begin
    LocalPath(1..Pathname'Length) := Pathname ;
    for i in reverse LocalPath'Range loop --remove trailing blanks
      if LocalPath(i) /= ' ' then
        LocalPath(i + 1) := Ascii.NUL; --null-terminate!!
        exit;
      end if;
    end loop;

    Result := C_Ftok(LocalPath, Proj);

    if Result = Key_t(C_ERROR) then
      if Debug_Errors then
        Log("Error when getting mailbox for: " & LocalPath, Always);
        Log("Project: " & Proj'Img & ", ftok result: " & Result'Img, Always);
        Perror("FileToKey");
      end if;
      raise Ftok_Failure;
    end if;
    return Result;
  end FileToKey;

-----------------------------------------------------



end Sv_m.C_Import;