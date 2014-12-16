
with Sql;
with Ada.Text_Io;
with Ada.Strings;       use  Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Calendar2;
with Types; use Types;

procedure Sql_Test is
   T            : Sql.Transaction_Type;
   Insert,
   Show,
   Update,
   Delete,
   Drop,
   Create,
   Select_All   : Sql.Statement_Type;
   Eos          : Boolean := False;
   F_Name       : String(1..100)      := (others => ' ');
   F_ID         : String(1..15)       := (others => ' ');
   Supported    : String(1..3)        := (others => ' ');
   Int          : Integer_4           := 0;
   Fl           : Float_8             := 0.0;
   Txt          : string(1..10)       := (others => ' ');
   Ts           : Calendar2.Time_Type := Calendar2.Time_Type_First;
   Rows_Affected : Natural := 0;
   
   
   procedure Do_Show_All(Lead : String) is
     T : Sql.Transaction_Type;
   begin
     Ada.Text_IO.Put_Line(Lead);
     T.Start;
     Show.Prepare ("select * from TEST order by INT");
     Show.Open_Cursor;
     loop
       Show.Fetch(Eos);
       exit when Eos;
       F_Name    := (others => ' ');
       F_ID      := (others => ' ');
       Supported := (others => ' ');
       Show.Get("INT",Int );
       Show.Get("FL",Fl );
       Show.Get("TXT",Txt );
       Show.Get("TS", Ts );
       Ada.Text_IO.Put_Line(int'Img & " " & Fl'Img & " " & Trim(Txt, Both) & " " & Ts.To_String);
     end loop;
     Show.Close_Cursor;
     T.Commit;
   end Do_Show_All;
   
   
begin

   -- ("Connect db");
   Sql.Connect
     (Host     => "sebjlun-deb64",
      Port     => 5432,
      Db_Name  => "dry",
      Login    => "bnl",
      Password => "bnl");

   T.Start;
   Select_All.Prepare (
     "select FEATURE_NAME, IS_SUPPORTED, FEATURE_ID " &
     "from INFORMATION_SCHEMA.SQL_FEATURES " &
     "where FEATURE_NAME like :LANGUAGE");

   Select_All.Set("LANGUAGE", "%Ada%");
   Select_All.Open_Cursor;
   loop
     Select_All.Fetch(Eos);
     exit when Eos;
     F_Name    := (others => ' ');
     F_ID      := (others => ' ');
     Supported := (others => ' ');
     Select_All.Get("FEATURE_ID",F_ID );
     Select_All.Get("FEATURE_NAME",F_Name );
     Select_All.Get("IS_SUPPORTED", Supported );
     Ada.Text_IO.Put_Line(Trim(F_ID, Both) & " " & Trim(F_Name, Both) & " " & Trim(Supported, Both));
   end loop;
   Select_All.Close_Cursor;
   T.Commit;
   
   
   Drop.Prepare ("drop table TEST");
   begin
     T.Start;
     Drop.Execute;
     T.Commit;
   exception
     when Sql.No_Such_Object =>
     T.Rollback;
   end;
   
   Create.Prepare (
     "create table TEST (" &
     "INT integer not null primary key, " &
     "FL float not null, " &
     "TXT varchar(10) not null, " & 
     "TS timestamp(3) without time zone not null)");
     
   T.Start;
   Create.Execute;
   T.Commit;
   
   T.Start;
   Insert.Prepare("insert into TEST values (:INT, :FL, :TXT, :TS)");
   Insert.Set("INT", Integer_4(1));
   Insert.Set("FL", 1.0);
   Insert.Set("TXT", "1234567890");
   Insert.Set_Timestamp("TS", Calendar2.Clock);
   Insert.Execute;
   
   Insert.Set("INT", Integer_4(2));
   Insert.Set("FL", 2.0);
   Insert.Execute;  -- reuse values from last set for TXT and TS
   T.Commit;
   
   Do_Show_All("Newly inserted");

   T.Start;
   Update.Prepare("update TEST set FL = :FL where INT = :INT");
   Update.Set("INT", Integer_4(1));
   Update.Set("FL", 5.0);
   Update.Execute;
   T.Commit;
   Do_Show_All("Newly updated");

   T.Start;
   Update.Set("INT", Integer_4(1));
   Update.Set("FL", 15.0);
   Update.Execute;
   T.Rollback;
   
   Do_Show_All("Newly updated - but with rollback");

   T.Start;
   Delete.Prepare("delete from TEST where INT = :INT");
   Delete.Set("INT", Integer_4(1));
   Delete.Execute;
   T.Commit;
   Do_Show_All("Newly deleted 1");

   T.Start;
   Delete.Set("INT", Integer_4(1));
   Delete.Execute(Rows_Affected);
   T.Commit;
   Do_Show_All("Newly deleted" & Rows_Affected'Img);

   begin
     T.Start;
     Drop.Execute;
     T.Commit;
   exception
     when Sql.No_Such_Object =>
     T.Rollback;
   end;
   
   
   Sql.Close_Session;

end Sql_Test;
