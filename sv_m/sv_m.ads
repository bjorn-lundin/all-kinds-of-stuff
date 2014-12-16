with Interfaces.C;

package Sv_m is
  type Byte is range 0 .. 2**8-1;
  for Byte'Size use 8;

  type Word is range 0..2**16-1;
  for Word'Size use 16;

  type Byte_Array is array (Positive  range  <>) of Byte;
  MAX_DATA_LENGHT : constant Positive := 1024;

  package C renames Interfaces.C;

  type Key_t is new C.Int;
  pragma Convention (C, Key_t);

  subtype Name_Type is String(1..15);

  type Process_Type is record
    Process_name : Name_Type := (others => ' ');
    Process_Node : Name_Type := (others => ' ');
  end record;

  type Identity_Type is range -10 .. 2**15-1;
  for  Identity_Type'Size use 16;

  Create_Message_Queue_Failure     : exception;
  Send_Failure                     : exception;
  Receive_Failure                  : exception;
  Timeout                          : exception;
  No_Such_Message_Queue            : exception;
  Unknown_Send_Failure             : exception;
  Too_Big_Message                  : exception;

  MESSAGE_TIMEOUT_IDENTITY : constant Identity_Type := -10;
  PROJECT      : constant C.Int := 234;  -- could be any number


  type Message_Type is private;
------------------------------------------------------
  procedure Create_Message_Queue(Key : in Key_t);
------------------------------------------------------
  procedure Send(Process : in Process_Type; Message : in Message_Type);
------------------------------------------------------
  procedure Receive(Message : out Message_Type; Timeout : in Duration := 0.0 );
------------------------------------------------------
--  function FileToKey(Pathname : in String; Proj : in C.Int) return Key_t ;
------------------------------------------------------
  function Identity(Message : in Message_Type) return Identity_Type;
----------------------------------------------------------
  function Sender  (Message : in Message_Type) return Process_Type;
----------------------------------------------------------
  function This_Process return Process_Type ;
----------------------------------------------------------
  generic
    Identity : Identity_Type;
    type Data_Type is private;
  package Generic_io is

    procedure Send (Receiver   : in  Process_Type;
                    Data       : in  Data_Type);

    function  Unpack (Message  : in  Message_Type) return Data_Type;

    function  Pack (Data       : in  Data_Type) return Message_Type;

  end Generic_io;
-----------------------------------------------------------------

private
  type Message_Body_Type is record
    Data_Length      : Integer := 0;
    Data             : Byte_Array(1..MAX_DATA_LENGHT);
  end record;

  type Message_Head_Type is record
    Receiver : Process_Type  := ((others => ' '),(others => ' '));
    Sender   : Process_Type  := ((others => ' '),(others => ' '));
    Identity : Identity_Type := Identity_Type'First;
  end record;

  type Message_Type is record
    Type_Of_Message : C.Long := 100;   -- must be greater than abs(TIMEOUT_MESSAGE_IDENTITY)
    Message_Head    : Message_Head_Type;
    Message_Body    : Message_Body_Type;
  end record;

end Sv_m;
