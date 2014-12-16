with Calendar;
with sql;
with text_io;
with gnat.traceback.symbolic;
with ada.exceptions; 

procedure sql_test3 is

  stm,stm2,stm3   : sql.statement_type;
  trans : sql.transaction_type;
  end_of_set : boolean := false;
  s1 : String(1..10) := (others => ' ');
  i1 : integer := 0;
  f1 : float := 0.0;
  dt,c1,c2 : Calendar.Time := Calendar.clock;
begin


--compiled and linked like this to get tracebackinfo on x86:
--gnatmake -gnatv sql_test3.adb -g -v -cargs -gnatfo -funwind-tables -bargs -E

--This is run on a table like this

--create table TEST2 (
-- i integer,
-- j integer,
-- PRIMARY KEY (i));

-- change to something suitable
  sql.Connect(db_name => "bnl");

    sql.start_read_write_transaction(trans);
    sql.prepare(stm,"insert into TEST2 values (:i,:j)");   
    sql.set(stm,"i",1);
    sql.set(stm,"j",2);
    sql.execute(stm); 
    sql.commit(trans);
 
  sql.Close_Session;
  
exception
  when Event: others =>
    text_io.put_line(ada.exceptions.Exception_Name(Event));
    text_io.put_line(gnat.traceback.symbolic.symbolic_traceback(event));  
end sql_test3;