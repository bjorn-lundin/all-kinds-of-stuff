--                                                                          --
--                      SQL                                                 --
--                                                                          --
--                                 Spec                                     --
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
with PGAda.Database; use PGAda.Database;
with Ada.Calendar;

package Sql is

  Not_Connected             : exception;
  No_Transaction            : exception;
  Duplicate_Index           : exception;
  No_Such_Row               : exception;
  Transaction_Conflict      : exception;
  PostgreSQL_Error          : exception;
  Too_Many_Cursors          : exception;
  Sequence_Error            : exception;
  Too_Many_Input_Parameters : exception;
  No_Such_Column            : exception;
  Null_Value                : exception;
  Transaction_Error         : exception;
  Sql_Error                 : exception;


 type Database_Type is (Rdb,         -- Not currently supported
                        Oracle,
		        Mimer,       -- Not currently supported
			Sql_Server,  -- Not currently supported
			Ms_Access,    -- Not currently supported
			PostgreSQL);  --bnl

  -- Function Database returns the name of the current database.
  function Database return Database_Type;

  type Transaction_Status_Type is (None, Read_Write, Read_Only);
  type Transaction_Isolation_Level_Type is (Read_Commited, Serializable);
  type Transaction_Isolation_Level_Scope_Type is (Transaction, Session);

  type Transaction_Type is limited private;
  type Statement_Type   is limited private;



-----------------------------------------------------------
  procedure Open_Oracle_Session(d1 : String; d2 : String);

  procedure Open_ODBC_Session(d1 : String; d2 : String; d3 : String);

  procedure Close_Session;

  procedure Connect(Host       : in String  := "";
                    Port       : in Natural := 0;
                    Options    : in String  := "";
                    TTY        : in String  := "";
                    DB_Name    : in String  := "";
                    Login      : in String := "";
                    Password   : in String := "");

  function  Is_Session_Open return Boolean;

-----------------------------------------------------------

  procedure Set_Transaction_Isolation_Level(Level : in Transaction_Isolation_Level_Type;
                                            Scope : in Transaction_Isolation_Level_Scope_Type);

  procedure Start_Read_Write_Transaction(T : in out Transaction_Type);

  procedure Start_Read_Only_Transaction(T : in out Transaction_Type);

  procedure Commit(T : in Transaction_Type) ;

  procedure Rollback(T : in Transaction_Type) ;

  function  Transaction_Status return Transaction_Status_Type;
-----------------------------------------------------------

  procedure Prepare(Statement : in out Statement_Type;
                    Command   : in String) ;

  procedure Open_Cursor(Statement : in Statement_Type);

  procedure Fetch(Statement  : in Statement_Type;
                  End_Of_Set : out Boolean) ;

  procedure Close_Cursor(Statement : in Statement_Type);


-----------------------------------------------------------
  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out Integer) ;

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out Integer);

  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out String);

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out String);

  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out Float);

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out Float);

  procedure Get(Statement : in Statement_Type;
                Parameter : in Positive;
                Value     : out Character);

  procedure Get(Statement : in Statement_Type;
                Parameter : in String;
                Value     : out Character);


  procedure Get_Date(Statement : in Statement_Type;
                     Parameter : in String;
                     Value     : out Ada.Calendar.Time) ;

  procedure Get_Time(Statement : in Statement_Type;
                     Parameter : in Positive;
                     Value     : out Ada.Calendar.Time) ;


  procedure Get_Time(Statement : in Statement_Type;
                     Parameter : in String;
                     Value     : out Ada.Calendar.Time) ;

  procedure Get_Date(Statement : in Statement_Type;
                     Parameter : in Positive;
                     Value     : out Ada.Calendar.Time) ;

-----------------------------------------------------------

  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in Integer);

  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in String);

  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in Character);

  procedure Set(Statement : in out Statement_Type;
                Parameter : in String;
                Value     : in Float);

  procedure Set_Date(Statement : in out Statement_Type;
                     Parameter : in String;
                     Value     : in Ada.Calendar.Time);

  procedure Set_Time(Statement : in out Statement_Type;
                     Parameter : in String;
                     Value     : in Ada.Calendar.Time);
-----------------------------------------------------------

  procedure Execute(Statement           : in Statement_Type;
                    No_Of_Affected_Rows : out Natural) ;


  procedure Execute(Statement : in  Statement_Type) ;

--------------------------------------------------------------
  function Is_Null (Statement: Statement_Type;
                    Parameter: Positive) return Boolean ;

  function Is_Null (Statement: Statement_Type;
                    Parameter: String) return Boolean ;

------------------------------------------------------------

  procedure Set_Null (Statement: Statement_Type;
                      Parameter: String);


  procedure Set_Null_Date (Statement: Statement_Type;
                           Parameter: String);

------------------------------------------------------------
  procedure Print_Errors(MyUnit   : in String;
                         MyStatus : in Exec_Status_Type);


------------------------------------------------------------
  function String_Date(t : in Ada.Calendar.Time) return String ;

------------------------------------------------------------
  function String_Time(t : in Ada.Calendar.Time) return String ;

------------------------------------------------------------

private
  type Transaction_Identity_Type is mod Integer'Last;

  type Statement_Type is record
    Index : Integer := 0;
  end record;

  type Transaction_Type is record
    Status      : Transaction_Status_Type := None;
    Counter     : Transaction_Identity_Type := 0;
  end record;
end Sql;




