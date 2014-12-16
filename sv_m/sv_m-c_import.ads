
with Sv_m.Constants ; use Sv_m.Constants;
with Sv_m.Utils     ; use Sv_m.Utils;

package Sv_m.C_Import is
  use C;
  Ftok_Failure : exception;

-----------------------------------------------------
  function FileToKey(Pathname     : in String;
                     Proj         : in C.Int;
                     Debug_Errors : in Boolean := True) return Key_t ;
-----------------------------------------------------
  function C_Receive(Queue_Identity : in C.Int;
                     Msg_Type       : in C.Long;
                     Msg_Flag       : in C.Int := 0) return Message_Type ;
-----------------------------------------------------
  function C_Create_Message_Queue(Key    : in Key_t;
                                  Create : in Boolean := True)  return C.Int ;
-----------------------------------------------------
  function C_Send(Queue_Identity : in C.Int;
                  Message        : in Message_Type) return C.Int ;
-----------------------------------------------------

end Sv_m.C_Import;
