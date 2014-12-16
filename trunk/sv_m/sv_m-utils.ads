

with Sv_m.Constants ; use Sv_m.Constants;

package Sv_m.Utils is
  use C;

  type Debug_Level_Type is (Global, Always);
  No_Such_Environment_Variable : exception;
------------------------------------------------------
  Type Pid_t is new C.Long;
  function Pid return Pid_t;

------------------------------------------------------
  function Environment(Str  : in String; Raise_Exception : Boolean := True) return String ;

----------------------------------------------------------
  procedure Perror(Text : in String) ;

------------------------------------------------------

  procedure Log(To_log : in String; When_to_log : in Debug_level_type := Global) ;
------------------------------------------------------

  function Process_Directory return String ;
------------------------------------------------------

  procedure Create_Pid_File(File_Name : in String) ;
------------------------------------------------------

  function Error_Number return C.Int;

  type Time_t is new C.Long;

-- struct ipc_perm
--   {
--     __key_t __key;			/* Key.  */
--     __uid_t uid;			/* Owner's user ID.  */
--     __gid_t gid;			/* Owner's group ID.  */
--     __uid_t cuid;			/* Creator's user ID.  */
--     __gid_t cgid;			/* Creator's group ID.  */
--     unsigned short int mode;		/* Read/write permission.  */
--     unsigned short int __pad1;
--     unsigned short int __seq;		/* Sequence number.  */
--     unsigned short int __pad2;
--     unsigned long int __unused1;
--     unsigned long int __unused2;
--   };
--
--file:/usr/include/linux/msg.h
-- struct msqid_ds {
-- 	struct ipc_perm msg_perm;
-- 	struct msg *msg_first;		/* first message on queue,unused  */
-- 	struct msg *msg_last;		/* last message in queue,unused */
-- 	__kernel_time_t msg_stime;	/* last msgsnd time */
-- 	__kernel_time_t msg_rtime;	/* last msgrcv time */
-- 	__kernel_time_t msg_ctime;	/* last change time */
-- 	unsigned long  msg_lcbytes;	/* Reuse junk fields for 32 bit */
-- 	unsigned long  msg_lqbytes;	/* ditto */
-- 	unsigned short msg_cbytes;	/* current number of bytes on queue */
-- 	unsigned short msg_qnum;	/* number of messages in queue */
-- 	unsigned short msg_qbytes;	/* max number of bytes on queue */
-- 	__kernel_ipc_pid_t msg_lspid;	/* pid of last msgsnd */
-- 	__kernel_ipc_pid_t msg_lrpid;	/* last receive pid */
-- };

--Linux 2.4 version include/bits/ipc.h
  type Ipc_Permissions_Type is record
    Key     : Key_t           := Key_t'First;
    Uid     : C.Unsigned      := C.Unsigned'First;
    Gid     : C.Unsigned      := C.Unsigned'First;
    Cuid    : C.Unsigned      := C.Unsigned'First;
    Cgid    : C.Unsigned      := C.Unsigned'First;
    Mode    : C.Unsigned      := C.Unsigned'First;
    Pad1    : C.Unsigned      := C.Unsigned'First;
    Seq     : C.Unsigned      := C.Unsigned'First;
    Pad2    : C.Unsigned      := C.Unsigned'First;
    Unused1 : C.Unsigned_Long := C.Unsigned_Long'First;
    Unused2 : C.Unsigned_Long := C.Unsigned_Long'First;
  end record;

  type Message_Queue_Identity_Ds_Type is record
    Msg_Perm   : Ipc_Permissions_Type;             --structure describing operation permission
    Msg_Stime  : Time_t          := Time_t'First;          --time of last msgsnd command
    Unused1    : C.Unsigned_Long := C.Unsigned_Long'First;
    Msg_Rtime  : Time_t          := Time_t'First;          --time of last msgrcv command
    Unused2    : C.Unsigned_Long := C.Unsigned_Long'First;
    Msg_Ctime  : Time_t          := Time_t'First;          --time of last change
    Unused3    : C.Unsigned_Long := C.Unsigned_Long'First;
    Msg_Cbytes : C.Unsigned_Long := C.Unsigned_Long'First; --current number of bytes on queue
    Msg_Qnum   : C.Unsigned_Long := C.Unsigned_Long'First; --number of messages currently on queue
    Msg_Qbytes : C.Unsigned_Long := C.Unsigned_Long'First; --max number of bytes allowed on queue
    Msg_Lspid  : Pid_t           := Pid_t'First;           --pid of last msgsnd()
    Msg_Lrpid  : Pid_t           := Pid_t'First;           --pid of last msgrcv()
    Unused4    : C.Unsigned_Long := C.Unsigned_Long'First;
    Unused5    : C.Unsigned_Long := C.Unsigned_Long'First;
  end record;



--typedef unsigned short __u_short;
--typedef unsigned int __u_int;
--typedef unsigned long __u_long;
--
--typedef __u_int __uid_t;		/* Type of user identifications.  */
--typedef __u_int __gid_t;		/* Type of group identifications.  */
--
--typedef long int __time_t;
--typedef unsigned long int msgqnum_t;
--typedef unsigned long int msglen_t;

  function Check_Queue(Q_Id : in C.Int; Cmd : in C.Int) return C.Int ;


end Sv_m.Utils;
