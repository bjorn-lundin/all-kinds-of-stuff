--                                                                          --
--                      SQL                                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--  Copyright (c) Björn Lundin 2001                                         --
--  All rights reserved.                                                    --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions      --
--  are met:                                                                --
--  1. Redistributions of source code must retain the above copyright       --
--     notice, this list of conditions and the following disclaimer.        --
--  2. Redistributions in binary form must reproduce the above copyright    --
--     notice, this list of conditions and the following disclaimer in      --
--     the documentation and/or other materials provided with the           --
--     distribution.                                                        --
--  3. Neither the name of Björn Lundin nor the names of its contributors   --
--     may be used to endorse or promote products derived from this         --
--     software without specific prior written permission.                  --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY BJÖRN LUNDIN AND CONTRIBUTORS ``AS         --
--  IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT          --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS       --
--  FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL BJÖRN       --
--  LUNDIN OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,              --
--  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES                --
--  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR      --
--  SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)      --
--  HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN               --
--  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR            --
--  OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE,          --
--  EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                      --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with PGAda.Database;        use PGAda.Database;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Calendar;

package body Sql is

  subtype Hour_type   is Integer  range  0..23;
  subtype Minute_type is Integer  range  0..59;
  subtype Second_type is Duration range  0.0..60.0;

  type Time_Type is record
    Hour   : Hour_type   := 0;
    Minute : Minute_type := 0;
    Second : Second_type := 0.0;
  end record;

  type Date_Type is record
    Year  : Ada.Calendar.Year_Number   := Ada.Calendar.Year_Number'First ;
    Month : Ada.Calendar.Month_Number  := Ada.Calendar.Month_Number'First ;
    Day   : Ada.Calendar.Day_Number    := Ada.Calendar.Day_Number'First ;
  end record;

  Time_Type_First : Time_Type ;
  Date_Type_First : Date_Type ;

  MAX_CURSORS        : constant Positive := 700;      --20
  MAX_BIND_VARIABLES : constant Integer := 60;       --60

  Cursor_Not_Found          : exception;
  Binder_Variable_Not_Found : exception;

  type Connection_Record  is record
    Is_Connected : Boolean := False;
    Connection   : Connection_Type;
  end record;

  type Bind_Type is record
    Is_Free     : Boolean := True;
    Name        : Unbounded_String;
    Value       : Unbounded_String;
  end record;

  type Binder_Array_Type is array (1..MAX_BIND_VARIABLES) of Bind_Type;

  type Cursor_Type is record
    Is_Free          : Boolean := True;
    Is_Prepared      : Boolean := False;
    Cursor_Name      : String (1..4) := (others => ' ');
    Query            : Unbounded_String;
    Unprepared_Query : Unbounded_String;
    Bind_Array       : Binder_Array_Type ;
  end record;

  Global_Cursors : array (1..MAX_CURSORS) of Cursor_Type;

  Global_Connection   : Connection_Record;  -- the default connection;
  Global_Transaction  : Transaction_Type ;  -- the only allowed transaction;
  Global_Query_Result : Result_Type;        -- the only result Set
  Global_DML_Result   : Result_Type;        -- the for stuff that is not a Query

  Global_Debug_Level  : Constant Integer := 0;

  Transaction_Setting    : Statement_Type;

  Global_Transaction_Identity : Transaction_Identity_Type;
------------------------------------------------------------
-- start local procs
------------------------------------------------------------
  procedure Log(What : in String; Level : in Integer := 1) is
  begin
    if Global_Debug_Level > 0 then
      case Level is
        when 0 => null;
        when others => Put_Line(What);
      end case;
    end if;
  end Log;

------------------------------------------------------------
  function Instr(Instring : in String ; What : in Character) return Integer is
  begin
    for i in Instring'Range loop
      if Instring(i) = What then
        return i;
      end if;
    end loop;
    return 0;
  end Instr;

------------------------------------------------------------

  function Dequote(Instring : in String) return String is
    j : Integer := Instr(Instring,''');
  begin
--    Log("Dequote |" & Instring & "|" );
    if j > 0 then
      return Instring(Instring'First .. j-1) &  "''" & Dequote(Instring(j+1..Instring'Last)) ;
    else
      return Instring;
    end if;
  end Dequote;

------------------------------------------------------------

  function Get_New_Transaction return Transaction_Identity_Type is
  begin
    Global_Transaction_Identity := Transaction_Identity_Type'Succ(Global_Transaction_Identity);
    return Global_Transaction_Identity;
  end Get_New_Transaction;

------------------------------------------------------------
  function Split_Time(t : in Ada.Calendar.Time) return Time_type is
    lct : Ada.Calendar.Day_duration;
    ltt : Time_type := Time_type_first;
    d : Date_type;
  begin
    Ada.Calendar.Split(t, d.year, d.month, d.day, lct);

    ltt.Hour := Hour_Type(Integer(lct) / (60 * 60));
    lct := lct - Ada.Calendar.Day_Duration(ltt.Hour * 60 * 60);

    ltt.Minute := Minute_Type(Integer(lct) / 60);
    lct := lct - Ada.Calendar.Day_Duration(ltt.Minute * 60);

    ltt.Second := Second_type(lct);

    return ltt;
  end Split_Time;
------------------------------------------------------------
  function String_Date(t : in Ada.Calendar.Time) return String is
    d : Date_type;
    lct : Ada.Calendar.Day_duration;
    retval : string(1..10) := (others => ' ');
  begin
    Ada.Calendar.Split(t, d.year, d.month, d.day, lct);
    declare
      Year  : constant String := Ada.Calendar.Year_number'image(d.year);
      Month : constant String := Ada.Calendar.Month_number'image(d.Month);
      Day   : constant String := Ada.Calendar.Day_number'image(d.Day);
    begin
      --Year has always 4 digits
      retval(1..4) := Year(2..5);
      retval(5..5) := "-";
      if Month'length = 2 then  -- image prefixes positives with blank
        retval(6..6) := "0";
        retval(7..7) := Month(2..2);
      else
        retval(6..7) := Month(2..3);
      end if;
      retval(8..8) := "-";

      if Day'length = 2 then
        retval(9..9) := "0";
        retval(10..10) := Day(2..2);
      else
        retval(9..10) := Day(2..3);
      end if;
    end; --declare

    return retval;

  end String_Date;
------------------------------------------------------------
  function String_Time(t : in Ada.Calendar.Time) return String is
    tt : time_type;
    retval : string(1..8) := (others => ' ');
  begin
    tt := Split_Time(t);

    declare
      Hour   : constant string :=  Hour_type'image(tt.Hour);
      Minute : constant string :=  Minute_type'image(tt.Minute);
      Second : constant string :=  Integer'image(Integer(tt.Second));
    begin
      if Hour'Length = 2 then
        retval(1..1) := "0";
        retval(2..2) := Hour(2..2);
      else
        retval(1..2) := Hour(2..3);
      end if;

      retval(3..3) := ":";

      if Minute'Length = 2 then
        retval(4..4) := "0";
        retval(5..5) := Minute(2..2);
      else
        retval(4..5) := Minute(2..3);
      end if;

      retval(6..6) := ":";

      if Second'Length = 2 then
        retval(7..7) := "0";
        retval(8..8) := Second(2..2);
      else
        retval(7..8) := Second(2..3);
      end if;
    end; --declare

    log("String_Time returns: " & retval);

    return retval;
  end String_Time;
------------------------------------------------------------
  function Convert_date_to_Calendar_time(d : in Date_Type ) return Ada.Calendar.time is
  begin
    return Ada.Calendar.Time_Of(d.Year, d.Month, d.Day);
  end Convert_date_to_Calendar_time;
------------------------------------------------------------
  function Convert_time_to_calendar_time(t : in Time_Type ) return Ada.Calendar.time is
  begin
     return Ada.Calendar.Time_Of(Date_Type_First.Year,
                             Date_Type_First.Month,
                             Date_Type_First.Day,
                             Ada.Calendar.Day_Duration(t.Hour * 60 * 60) +
                             Ada.Calendar.Day_Duration(t.Minute * 60.0) +
                             t.Second);
  end Convert_time_to_calendar_time;
------------------------------------------------------------
------------------------------------------------------------

  procedure Print_Errors(MyUnit : in String;
                         MyStatus : in Exec_Status_Type) is
  begin
    if ((MyStatus /= Command_Ok) and
        (MyStatus /= Tuples_Ok)) then
      Put_Line(MyUnit);
      Put_Line(Error_Message(Global_Connection.Connection));
      Put_Line(Exec_Status_Type'Image(MyStatus));
    end if;
  end Print_Errors;

------------------------------------------------------------

  function PGerror(Local_Status : Exec_Status_Type) return Boolean is
    Failure : Boolean := True;
  begin
    Failure := ((Local_Status /= Command_Ok) and (Local_Status /= Tuples_Ok));
    if Failure then
      Put_Line("PGerror: " & Exec_Status_Type'Image(Local_Status));
    end if;
    return Failure;
  end PGerror;

------------------------------------------------------------

  procedure Free_Binder_Array(Cursor_Index : in Integer) is
  begin
    for i in 1..MAX_BIND_VARIABLES loop
      Global_Cursors(Cursor_Index).Bind_Array(i).Is_Free := True;
    end loop;
  end Free_Binder_Array;

--------------------------------------------------------------

  function Get_Next_Free_Bindno(Statement : in Statement_Type;
                                Parameter : in String) return Natural is
     Next_Free_Bindno : Natural := 0;
  begin
--make sure Set can be used several times on same Bind variables on Statement
--first look for not free Bind varables with thesame Bindname.
  -- if not found, continue as usual, but if found, return that index so it can be overwritten.

    for j in 1..MAX_BIND_VARIABLES loop
      if Global_Cursors(Statement.Index).Bind_Array(j).Name = To_Unbounded_String(Parameter) then
        Global_Cursors(Statement.Index).Bind_Array(j).Is_Free := False;
        Next_Free_Bindno :=  j;
        exit;
      end if;

      if Global_Cursors(Statement.Index).Bind_Array(j).Is_Free then
        Global_Cursors(Statement.Index).Bind_Array(j).Is_Free := False;
        Next_Free_Bindno :=  j;
        exit;
      end if;
    end loop;

    if Next_Free_Bindno > MAX_BIND_VARIABLES then
      Put_Line("Get_Next_Free_Bindno: Too_Many_Input_Parameters");
      raise Too_Many_Input_Parameters;
    else
      return Next_Free_Bindno;
    end if;
  end Get_Next_Free_Bindno;

------------------------------------------------------------

  function Get_Binder_Variable(Cursor_Index : in Natural;
                               Bind : in Unbounded_String) return Unbounded_String is
  begin
    for j in 1..MAX_BIND_VARIABLES loop
      if To_String(Global_Cursors(Cursor_Index).Bind_Array(j).Name) = To_String(Bind) then
        return Global_Cursors(Cursor_Index).Bind_Array(j).Value;
      end if;
    end loop;
    Put_Line("Get_Binder_Variable: '" & To_String(Bind) & "' NOT found!! Is it SET ?");
    raise Binder_Variable_Not_Found;
  end Get_Binder_Variable;

------------------------------------------------------------

  function No_Of_Binders(Command : in String) return Natural is
    No: Natural := 0; -- no of parameters
  begin
    for i in Command'Range loop
      case Command(i) is
        when ':' => No := No + 1;
        when others => null;
      end case;
    end loop;
    if (No > MAX_BIND_VARIABLES) then
      Put_Line("No_Of_Binders: Too_Many_Input_Parameters");
      raise Too_Many_Input_Parameters;
    end if;
--    Log("No_Of_Binders:no of variables: " & Natural'Image(no));
    return No;
  end No_Of_Binders;

------------------------------------------------------------

  function Edit_Marker (Command : in String;
                        Cursor_index : in Natural) return Unbounded_string is
    Query : Unbounded_string;
    Local_Command : String(1..command'Last +1) ;
  begin
    Local_Command(1..command'Last) := Command(1.. Command'Last);
    Local_Command(Command'Last + 1..Command'Last+1) := ".";

    Command_Loop: for i in Local_Command'range loop
      if Local_Command(i) = ':' then
        for j in i + 1..Local_Command'Last loop
          case Local_Command(j) is
            when ' ' | ')' | ',' | '.' =>
              Query := To_Unbounded_String(Local_Command(1..i-1) & " ") &
                       Get_Binder_Variable(Cursor_Index,To_Unbounded_String(Local_Command(i+1..j-1))) &
                        To_Unbounded_String(Local_Command(j..Local_Command'Last));
              exit Command_Loop;
            when others => null;
          end case;
        end loop ;
      end if;
    end loop Command_Loop;
    declare
      s2 : constant String := To_String(Query);
    begin
      Query := To_Unbounded_String(s2(1..s2'Last -1));
      return Query;
    end;
  end Edit_Marker;

------------------------------------------------------------

  function Bind_All(Statement : in Statement_Type) return Unbounded_String is
    Binders_No : Natural;
  begin
    --can be replaced with Recursive fuction!!
    -- Bind them here and raise Sequence_Error if not all have values!
    Binders_No := No_Of_Binders(To_String(Global_Cursors(Statement.Index).Query));
    Log("BIND_ALL: transforming: " & To_String(Global_Cursors(Statement.Index).Query));
    for i in 1..Binders_No loop
        Global_Cursors(Statement.Index).Query :=
                 Edit_Marker(To_String(Global_Cursors(Statement.Index).Query),Statement.Index);
    end loop;
    Log("BIND_ALL: returning: " & To_String(Global_Cursors(Statement.Index).Query));

-- check if all are bound
-- it will currently crash in 'Get_Binder_Variable'
-- so this test is not needed
--    declare
--      bound_Query : constant string := To_String(Global_Cursors(Statement.Index).Query);
--    begin
--      for j in bound_Query'range loop
--        if bound_Query(j) = ':' then
--          Log("BIND_ALL: All Bind variables not bound!!");
--          raise Sequence_Error;
--        end if;
--      end loop;
--    end;

    Global_Cursors(Statement.Index).Is_Prepared := True; --move this to a real check in Edit_Marker!!
    return Global_Cursors(Statement.Index).Query ;
  end Bind_All;

--------------------------------------------------------------------------------

  function Convert_To_Time(MyTime : String) return Time_Type is
    Local_Time : Time_Type := Time_Type_First;
  begin
    Local_Time.Hour   := Hour_Type'Value(MyTime(1 .. 2));
    Local_Time.Minute := Minute_Type'Value(MyTime(4 .. 5));
    Local_Time.Second := Second_Type'Value(MyTime(7 .. 8));
    return Local_Time;
  end Convert_To_Time;

--------------------------------------------------------------------------------

  function Convert_To_Date(MyDate : String) return Date_Type is
    Local_Date : Date_Type := Date_Type_First;
  begin
    Local_Date.Year  := Ada.Calendar.Year_Number'Value(MyDate(1 .. 4));
    Local_Date.Month := Ada.Calendar.Month_Number'Value(MyDate(6 .. 7));
    Local_Date.Day   := Ada.Calendar.Day_Number'Value(MyDate(9 ..10));
    return Local_Date;
  end Convert_To_Date;

------------------------------------------------------------
-- end local procs
------------------------------------------------------------

------------------------------------------------------------
-- start connection related proces
--------------------------------------------------------------

  procedure Connect(Host       : in String  := "";
                    Port       : in Natural := 0;
                    Options    : in String  := "";
                    TTY        : in String  := "";
                    DB_Name    : in String  := "";
                    Login      : in String := "";
                    Password   : in String := "")     is
    Local_Status : Connection_Status_Type;
  begin
    Set_DB_Login (Global_Connection.Connection,
                  Host      => Host,
                  Port      => Port,
                  Options   => Options,
                  TTY       => TTY,
                  Db_Name   => DB_Name,
                  Login     => Login,
                  Password  => Password);

    Local_Status := Status (Global_Connection.Connection);
    case Local_Status is
      when Connection_Ok =>
        Global_Connection.Is_Connected := True;
      when Connection_Bad =>
        Global_Connection.Is_Connected := False;
        Put_Line("Connect: Not_Connected");
        raise Not_Connected;
    end case;
  end Connect;

---------------------------------------------------------------

  procedure Open_Oracle_Session(d1 : String; d2 : String) is
  begin
   -- Connect(DB_Name => "");
    null;
  end Open_Oracle_Session;

---------------------------------------------------------------

  procedure Open_ODBC_Session(d1 : String; d2 : String; d3 : String) is
  begin
    null;
  end Open_ODBC_Session;

--------------------------------------------------------------

  procedure Close_Session is
  begin
    if Is_Session_Open then
      Finish (Global_Connection.Connection);
      Global_Connection.Is_Connected := False;
    end if;
    Log("Session closed");
  end Close_Session;

--------------------------------------------------------------

  function Database return Database_Type is
  begin
    return PostgreSQL;
  end Database;

----------------------------------------------------------------
  function Is_Session_Open return Boolean is
  begin
    return Global_Connection.Is_Connected;
  end Is_Session_Open;

--------------------------------------------------------------
-- end connection related procs
--------------------------------------------------------------

--------------------------------------------------------------
-- start transaction handling procs
--------------------------------------------------------------

  function Transaction_In_Progress return Boolean is
  begin
    return Global_Transaction.Counter > Transaction_Identity_Type'First;
  end Transaction_In_Progress;

--------------------------------------------------------------
  procedure Check_Is_Connected is
  begin
    if not Global_Connection.Is_Connected then
      raise Not_Connected;
    end if;
  end Check_Is_Connected;

------------------------------------------------------------

  procedure Check_Transaction_In_Progress is
  begin
    if not Transaction_In_Progress then
      raise No_Transaction;
    end if;
  end Check_Transaction_In_Progress;

------------------------------------------------------------

  procedure Check_Is_Prepared(Cursor_Index : in Integer) is
  begin
    if not Global_Cursors(Cursor_Index).Is_Prepared then
      raise Sequence_Error;
    end if;
  end Check_Is_Prepared;

------------------------------------------------------------

  procedure Start_Transaction(t  : in out Transaction_Type;
                              ts : in Transaction_Status_Type) is
    Status : Exec_Status_Type;
  begin
    Check_Is_Connected;
    -- check if transaction already in progress
    case Global_Transaction.Status is
      when None =>
        t.Counter := Get_New_Transaction;
        Global_Transaction.Counter := t.Counter;
      when Read_Only =>
        if ts = Read_Write then
          Put_Line("Start_Transaction: Transaction_Error");
          raise Transaction_Error;
        end if;
        t.Counter := Get_New_Transaction;
        return;  -- do nothing

      when Read_Write =>
        if ts = Read_Only then
          Put_Line("Start_Transaction: Transaction_Error");
          raise Transaction_Error;
        end if;
        t.Counter := Get_New_Transaction;
        return;  -- do nothing
    end case;

    Exec (Global_Connection.Connection, "Begin", Global_DML_Result);
    Status := Result_Status(Global_DML_Result);
    Clear(Global_DML_Result);
    if PGerror(Status) then
      Print_Errors("Start_Read_Only_Transaction",Status);
      raise PostgreSQL_Error;
    end if;
    Global_Transaction.Status := ts;
  end Start_Transaction;

---------------------------------------------------------------

  procedure Start_Read_Only_Transaction(t : in out Transaction_Type) is
  begin
    Start_Transaction(t,Read_Only);
  end Start_Read_Only_Transaction;

---------------------------------------------------------------

  procedure Start_Read_Write_Transaction(t : in out Transaction_Type) is
  begin
    Start_Transaction(t,Read_Write);
  end Start_Read_Write_Transaction;

------------------------------------------------------------

  procedure Commit(t : in Transaction_Type) is
    Status : Exec_Status_Type;
  begin
    Check_Is_Connected;
    Check_Transaction_In_Progress;
  -- check if transaction already in progress
    case Global_Transaction.Status is
      when None =>
        Put_Line("Commit: No_Transaction");
        raise No_Transaction;

      when Read_Only | Read_Write =>
        -- check for ownership
        if t.Counter /= Global_Transaction.Counter then
        -- not the owner, do nothing
          Log("not the owner tries to commit");
          return;
        end if;
    end case;

    Exec (Global_Connection.Connection, "Commit", Global_DML_Result);
    Status := Result_Status(Global_DML_Result);
    Clear(Global_DML_Result);                                           ------------------------!!!!!!!!!!!!!!!!!!!1
    Clear(Global_Query_Result);
    if PGerror(Status) then
      Print_Errors("commit",Status);
      raise PostgreSQL_Error;
    end if;
    Global_Transaction.Counter := Transaction_Identity_Type'First;
    Global_Transaction.Status := None;
    Log("the owner commits");
  end Commit;

------------------------------------------------------------

  procedure Rollback(t : in Transaction_Type) is
    Status : Exec_Status_Type;
  begin
    Check_Is_Connected;
    Check_Transaction_In_Progress;

    case Global_Transaction.Status is
      when None =>
        Put_Line("Rollback: No_Transaction");
        raise No_Transaction;

      when Read_Only | Read_Write =>
        -- check for ownership
        if t.Counter /= Global_Transaction.Counter then
        -- not the owner
          Put_Line("not the owner tries to rollback");
          raise Transaction_Error;
--          return;
        end if;
    end case;

    Exec (Global_Connection.Connection, "Rollback", Global_DML_Result);
    Status := Result_Status(Global_DML_Result);
    Clear(Global_DML_Result);
    Clear(Global_Query_Result);
    if PGerror(Status) then
      Print_Errors("rollback",Status);
      raise PostgreSQL_Error;
    end if;
    Global_Transaction.Counter := Transaction_Identity_Type'First;
    Global_Transaction.Status := None;
  end Rollback;

--------------------------------------------------------------

  function Transaction_Status return Transaction_Status_Type is
  begin
    return Global_Transaction.Status;
  end Transaction_Status;

--------------------------------------------------------------
  procedure Set_Transaction_Isolation_Level(Level : in Transaction_isolation_level_Type;
                                            Scope : in Transaction_isolation_level_scope_Type) is
    Local_Transaction : Transaction_Type;
  begin
    Start_Read_Write_Transaction(Local_Transaction);
    case Level is
      when Read_Commited =>
        case Scope is
          when Transaction =>
            Prepare(Transaction_Setting,"SET TRANSACTION ISOLATION LEVEL READ COMMITTED");
          when Session =>
            Prepare(Transaction_Setting,"SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL READ COMMITTED");
        end case;
      when Serializable =>
        case Scope is
          when Transaction =>
            Prepare(Transaction_Setting,"SET TRANSACTION ISOLATION LEVEL SERIALIZABLE");
          when Session =>
            Prepare(Transaction_Setting,"SET SESSION CHARACTERISTICS AS TRANSACTION ISOLATION LEVEL SERIALIZABLE");
        end case;
    end case;
    Execute(Transaction_Setting);
    Commit(Local_Transaction);
  end Set_Transaction_Isolation_Level;


--------------------------------------------------------------
-- end transaction handling procs
--------------------------------------------------------------

--------------------------------------------------------------
-- start cursor handling procs
--------------------------------------------------------------

  procedure Prepare(Statement : in out Statement_Type;
                    Command   : in String) is
    Free_Cursor_Found : Boolean := False;
  begin
-- associate cursor with Statement in and mark taken in global array
-- if it is not already assigned an index

    if Statement.Index > 0 then  -- reprepare
      Free_Cursor_Found := True;
    else                         -- first time
      for i in 1..MAX_CURSORS loop
        if Global_Cursors(i).Is_Free then
          Log("PREPARE: Allocating cursor: " & Integer'Image(i) );
          Global_Cursors(i).Is_Free := False;
          Free_Cursor_Found := True;
          Statement.Index := i;
          case i is
            when 1..9     =>
              Global_Cursors(Statement.Index).Cursor_Name(1..2) := "C" & Integer'Image(i)(2..2);
            when 10..99   =>
              Global_Cursors(Statement.Index).Cursor_Name(1..3) := "C" & Integer'Image(i)(2..3);
            when 100..MAX_CURSORS =>
              Global_Cursors(Statement.Index).Cursor_Name(1..4) := "C" & Integer'Image(i)(2..4);
            when others   =>
                raise Too_Many_Cursors;
          end case;
          exit;
        end if;
      end loop;
    end if;

    if not Free_Cursor_Found then
      Put_Line("PREPARE: Could not find a free cursor!" );
      raise Too_Many_Cursors;
    end if;
    Global_Cursors(Statement.Index).Is_Prepared := False ;
    Global_Cursors(Statement.Index).Query := To_Unbounded_String(Command) ;
    Global_Cursors(Statement.Index).Unprepared_Query :=
                              Global_Cursors(Statement.Index).Query;

end Prepare;

------------------------------------------------------------

  procedure Open_Cursor(Statement : in Statement_Type) is
    Status : Exec_Status_Type;
  begin
  -- check for open database and prepared Statement too!!
  -- declare/open the cursor and execute the Statement
    Check_Is_Connected;
    Check_Transaction_In_Progress;

    -- too allow a Statement to be prepared several times
    if not Global_Cursors(Statement.Index).Is_Prepared then
      Global_Cursors(Statement.Index).Query :=
        Global_Cursors(Statement.Index).Unprepared_Query;
    end if;

    Log("SQL.OPEN_CURSOR: " & To_String(Global_Cursors(Statement.Index).Query));
    Global_Cursors(Statement.Index).Query := Bind_All(Statement);

    Exec (Global_Connection.Connection,
          "Declare " & Global_Cursors(Statement.Index).Cursor_Name & " cursor for " &
            To_String(Global_Cursors(Statement.Index).Query),
           Global_DML_Result);
    Status := Result_Status(Global_DML_Result);
    Clear(Global_DML_Result);
    if PGerror(Status) then
      Print_Errors("Open_Cursor",Status);
      raise PostgreSQL_Error;
    end if;
  end Open_Cursor;

------------------------------------------------------------

  procedure Fetch(Statement  : in Statement_Type;
                  End_Of_Set : out Boolean) is
    Status : Exec_Status_Type;
  begin
    Check_Is_Connected;
    Check_Transaction_In_Progress;
    Check_Is_Prepared(Statement.Index);


    Log("Fetch - fetching in: '" & Global_Cursors(Statement.Index).Cursor_Name & "'");
    Clear(Global_Query_Result); --clear old result
    Exec (Global_Connection.Connection, "Fetch next in " & Global_Cursors(Statement.Index).Cursor_Name, Global_Query_Result);
    Status := Result_Status(Global_Query_Result);

    if PGerror(Status) then
        Print_Errors("Fetch",Status);
        raise PostgreSQL_Error;
    end if;

    End_Of_Set := (Nbr_Tuples (Global_Query_Result) = 0);
  end Fetch;

------------------------------------------------------------

  procedure Close_Cursor(Statement : in Statement_Type) is
    Status : Exec_Status_Type;
  begin
  -- remove cursor and association?
    Check_Is_Connected;
    Check_Transaction_In_Progress;
    Check_Is_Prepared(Statement.Index);

    Exec (Global_Connection.Connection, "Close " & Global_Cursors(Statement.Index).Cursor_Name, Global_DML_Result);
    Status := Result_Status(Global_DML_Result);
    Clear(Global_DML_Result);
    if PGerror(Status) then
      Print_Errors("Close_Cursor",Status);
      raise PostgreSQL_Error;
    end if;
  end Close_Cursor;

-------------------------------------------------------------

  procedure Execute(Statement           : in Statement_Type;
                    No_Of_Affected_Rows : out Natural) is
    Status : Exec_Status_Type;
  begin

  -- check for open database and prepared Statement too!!
  -- declare/open the cursor and execute the Statement
    Check_Is_Connected;
    Check_Transaction_In_Progress;

    if Transaction_Status /= Read_Write then
      Put_Line("Exceute: current transaction type is: " &
              Transaction_Status_Type'Image(Transaction_Status));
      raise Sequence_Error;
    end if;

    -- extract cursor.Query to its Bindvarables here!
    -- raise sequence error if not all are bound!
    Global_Cursors(Statement.Index).Query := Bind_All(Statement);

    Check_Is_Prepared(Statement.Index);

    Exec (Global_Connection.Connection,
          To_String(Global_Cursors(Statement.Index).Query),
          Global_Query_Result);
    Status := Result_Status(Global_Query_Result);

    if PGerror(Status) then
      Print_Errors("Execute",Status);
      raise PostgreSQL_Error;
    end if;

-- to allow statements like 'set transaction isolation level read comitted'
-- which returns no rows
   begin
     No_Of_Affected_Rows := Rows_Affected(Global_Query_Result);
   exception
     when Constraint_Error => No_Of_Affected_Rows := 1;
   end;
   Clear(Global_Query_Result);
  end Execute;

------------------------------------------------------------

  procedure Execute(Statement : in Statement_Type) is
    Rows : Natural := 0;
  begin
    Execute(Statement,Rows);
    if Rows = 0 then
      raise No_Such_Row;
    end if;
  end Execute;

------------------------------------------------------------

  function Is_Null (Statement: Statement_Type;
                    Parameter: Positive) return Boolean is
  begin
    return Pgada.Database.Is_null(Global_Query_Result,1,Parameter);
  end Is_Null;

------------------------------------------------------------

  function Is_Null (Statement: Statement_Type;
                    Parameter: String) return Boolean is
  begin
    return Is_Null(Statement,Get_Field_Number(Global_Query_Result,Parameter));
  end Is_Null;

--------------------------------------------------------------
-- end cursor handling procs
------------------------------------------------------------

--------------------------------------------------------------
-- start Set handling procs
------------------------------------------------------------

  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in String) is

    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
    Local_Value : constant String := Dequote(Value);
  begin
--     Log("Set |" & Local_Value & "|");

   -- Associate Query Bind variable(Bindno) with Value
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String("'" & Local_Value & "'");
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set;

------------------------------------------------------------

  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in Integer) is

    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
  begin
   -- Associate Query Bind variable(Bindno) with Value
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String(Integer'Image(Value));
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set;

------------------------------------------------------------

  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in Float) is
    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
  begin
   -- Associate Query Bind variable(Bindno) with Value
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String(Float'Image(Float(Value)));
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set;
---------------------------------------------------------
  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in Character) is
    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
    Local_Value : String(1..1) := (others => ' ');
  begin
   -- Associate Query Bind variable(Bindno) with Value
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Local_Value(1) := Value;
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String(Local_Value);
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set;
-------------------------------------------------------------

  procedure Set_Date(Statement : in out Statement_Type;
                     Parameter : in String;
                     Value     : in Ada.Calendar.Time) is
    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
  begin
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String("'" & String_Date(Value) & "'");
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set_Date;
------------------------------------------------------------

  procedure Set_Time(Statement : in out Statement_Type;
                     Parameter : in String;
                     Value     : in Ada.Calendar.Time) is
    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
    Local_Time_1 : constant String := String_Time(Value);
    Local_Time_2 : String(1..6) := (others => ' ');
  begin
    Local_Time_2(1..2) := Local_Time_1(1..2);  -- remove ':' from time
    Local_Time_2(3..4) := Local_Time_1(4..5);
    Local_Time_2(5..6) := Local_Time_1(7..8);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String("'" & Local_Time_2 & "'");
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set_Time;
------------------------------------------------------------

  procedure Set_Null (Statement: Statement_Type;
                      Parameter: String) is
    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
  begin
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String("NULL");
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set_null;

------------------------------------------------------------

  procedure Set_Null_Date (Statement: Statement_Type;
                           Parameter: String) is
    Bindno : Natural := Get_Next_Free_Bindno(Statement,Parameter);
  begin
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Name := To_Unbounded_String(Parameter);
    Global_Cursors(Statement.Index).Bind_Array(Bindno).Value := To_Unbounded_String("NULL");
    Global_Cursors(Statement.Index).Is_Prepared := False ;
  end Set_Null_Date;

--------------------------------------------------------------
-- end Set handling procs
------------------------------------------------------------

--------------------------------------------------------------
-- start Get handling procs
------------------------------------------------------------

  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out Integer) is
  begin
    declare                   -- always return the 1st row in the latest result!!!!;
      Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
    begin
      Value := Integer'Value(Local_String);
    end;

  exception
    when Constraint_Error =>
      Put_Line("No such column: " & Positive'Image(Parameter));
      raise No_Such_Column;
  end Get;

------------------------------------------------------------

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out Integer) is
  begin
    declare -- always return the 1st row in the latest result!!!!;
      Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
    begin
      Value := Integer'Value(Local_String);
    end;
  exception
    when Constraint_Error =>
      Put_Line("No such column: " & Parameter);
      raise No_Such_Column;
  end Get;

------------------------------------------------------------

  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out String) is
-- always return the 1st row in the latest result!!!!;
    Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
  begin
    Value(1..Local_String'Length) := Local_String;
  exception
    when Constraint_Error =>
      Put_Line("No such column: " & Positive'Image(Parameter));
      raise No_Such_Column;
  end Get;

-----------------------------------------------------------

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out String) is
-- always return the 1st row in the latest result!!!!;
    Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
  begin
    Value(1..Local_String'Length) := Local_String;
  exception
    when Constraint_Error =>
      Put_Line("No such column: " & Parameter);
      raise No_Such_Column;
  end Get;

----------------------------------------------------------

  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out Character) is
  begin
    declare  -- always return the 1st row in the latest result!!!!;
      Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
    begin
      Value := Local_String(1);
    end;

  exception
    when Constraint_Error =>
      Put_Line("No such column: " & Positive'Image(Parameter));
      raise No_Such_Column;
  end Get;

----------------------------------------------------------

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out Character) is
  begin
    declare  -- always return the 1st row in the latest result!!!!;
      Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
    begin
      Value := Local_String(1);
    end;

  exception
    when Constraint_Error =>
      Put_Line("No such column: " & Parameter);
      raise No_Such_Column;
  end Get;

----------------------------------------------------------


  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out Float) is
  begin
    declare	  -- always return the 1st row in the latest result!!!!;
      Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
    begin
      Value := Float(Float'Value(Local_String));
    end;
  exception
    when Constraint_Error =>
      Put_Line("No such column: " & Positive'Image(Parameter));
      raise No_Such_Column;
  end Get;

------------------------------------------------------------

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out Float) is
  begin
    declare   -- always return the 1st row in the latest result!!!!;
      Local_String : constant String := Get_Value(Global_Query_Result,1,Parameter);
    begin
      Value := Float(Float'Value(Local_String));
    end;
  exception
    when Constraint_Error =>
    Put_Line("No such column: " & Parameter);
    raise No_Such_Column;
  end Get;

------------------------------------------------------------

  procedure Get_Date(Statement : in Statement_Type;
                     Parameter : in String;
                     Value     : out Ada.Calendar.Time) is
    fn : natural := get_field_number(Global_Query_result, Parameter);
  begin
    Get_Date(Statement,fn,Value);
  exception
    when constraint_error =>
    put_line("No such column: " & Parameter);
    raise no_such_column;
  end Get_Date;

------------------------------------------------------------

  procedure Get_Date(Statement : in Statement_Type;
                     Parameter : in Positive;
                     Value     : out Ada.Calendar.Time) is
  begin
    declare   -- always return the 1st row in the latest result!!!!;
      locstr : constant string := Get_Value(Global_Query_Result,1,Parameter);
    begin
--      put_line("Get_date: '" & locstr & "'");
      if locstr'length = 0 then
        Value := Ada.Calendar.Time_of(Date_Type_First.Year, Date_Type_First.Month, Date_Type_First.Day);
      else
	    Value := Convert_date_to_calendar_time(Convert_to_date(locstr));
      end if;
    end;
  exception
    when constraint_error =>
    put_line("No such column number: " & Parameter'img);
    raise no_such_column;
  end Get_Date;
------------------------------------------------------------

  procedure Get_time(Statement : in Statement_Type;
                     Parameter : in String;
                     Value     : out Ada.Calendar.Time) is
    fn : natural := get_field_number(Global_Query_result, Parameter);
  begin
    Get_Time(Statement,fn,Value);
  exception
    when constraint_error =>
    put_line("No such column: " & Parameter);
    raise no_such_column;
  end Get_time;

------------------------------------------------------------

 procedure Get_time(Statement : in Statement_Type;
                     Parameter : in Positive;
                     Value     : out Ada.Calendar.Time) is
  begin
    declare   -- always return the 1st row in the latest result!!!!;
      locstr : constant string := Get_Value(Global_Query_Result,1,Parameter);
    begin
      if locstr'length = 0 then
        Value := Ada.Calendar.Time_of(Date_Type_First.Year, Date_Type_First.Month, Date_Type_First.Day);
      else
        Value := Convert_time_to_calendar_time(Convert_to_time(locstr));
      end if;
--      put_line("Get_date: '" & locstr & "'");
    end;
  exception
    when constraint_error =>
    put_line("No such column number: " & Parameter'img);
    raise no_such_column;
  end Get_time;


--------------------------------------------------------------
-- end Get handling procs
------------------------------------------------------------


end Sql;





