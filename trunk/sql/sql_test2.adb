with Calendar;
with sql;
with text_io;
with gnat.traceback.symbolic;
with ada.exceptions; 

procedure sql_test2 is

  stm,stm2        : sql.statement_type;
  trans           : sql.transaction_type;
  end_of_set      : boolean := false;
  s1              : String(1..10) := (others => ' ');
  i1              : integer := 0;
  f1              : float := 0.0;
  dt,c1,c2        : Calendar.Time := Calendar.clock;
begin

--Simple sample program that inserts a row i the database,
-- and then read and prints all rows in the table.
--This is run on a table like this

--create table TEST (
-- i integer,
-- f float,
-- s character(10),
-- d date,
-- t time);



-- change to something suitable
    sql.Connect(db_name => "bnl");

    sql.start_read_write_transaction(trans);
 
    sql.prepare(stm,"insert into TEST values (:INT,:FLOAT, :STRING, :DATE, :TIME)");   
    sql.set(stm,"INT",1);
    sql.set(stm,"FLOAT",1.0);
    sql.set(stm,"STRING","Hi there");
    sql.set_date(stm,"DATE",dt);
    sql.set_time(stm,"TIME",dt);

    sql.execute(stm); 
    sql.commit(trans);

    sql.start_read_write_transaction(trans);

    sql.prepare(stm2,"select * from TEST");

    sql.open_cursor(stm2);
    loop
      sql.fetch(stm2,end_of_set);
      exit when end_of_set;  
 
      if not sql.is_null(stm2,"S") then                     --should test for null values!!
        sql.get(stm2,"S",s1);
      end if; 

      sql.get(stm2,"F",f1);
      sql.get(stm2,"I",i1);
      sql.get_time(stm2,"T",c1);
      sql.get_date(stm2,"D",c2) ;
      Text_io.put("F" & " - " & f1'img & " | ");
      Text_io.put("I" & " - " & i1'img & " | ");
      Text_io.put("S" & " - " & s1 & " | ");
      Text_io.put("T" & " - " & sql.string_time(c1) & " | ");
      Text_io.put_line("T" & " - " & sql.string_date(c2) );
    end loop;
    sql.close_cursor(stm2);
  
    sql.commit(trans); 

  sql.Close_Session;
  
exception
  when Event: others =>
  text_io.put_line(gnat.traceback.symbolic.symbolic_traceback(event));  
  text_io.put_line(ada.exceptions.Exception_Name(Event));
end sql_test2;