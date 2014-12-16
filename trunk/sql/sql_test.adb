
with sql;
with text_io;
with gnat.traceback.symbolic;


procedure sql_test is

  stm   : sql.statement_type;
  trans : sql.transaction_type;
  end_of_set : boolean := false;
  Title : String(1..50) := (others => ' ');
  Lastname : String(1..20) := (others => ' ');
begin


  sql.Connect(db_name => "bnl");

    sql.start_read_write_transaction(trans);
    sql.prepare(stm,"select AUTHORS.LASTNAME, BOOKS.TITLE "        &
                 " from BOOKS, BOOKS_AUTHORS, AUTHORS "            &
                 " where BOOKS.ISBN_NO = BOOKS_AUTHORS.ISBN_NO "   & 
                 " and AUTHORS.AUTHORNO = BOOKS_AUTHORS.AUTHORNO " & 
                 " and AUTHORS.LASTNAME in (:A1,:A2)");
   
    sql.set(stm,"A1","Lang");
    sql.set(stm,"A2","Christie");
    sql.open_cursor(stm);
    loop
      sql.fetch(stm,end_of_set);
      exit when end_of_set;  
      sql.get(stm,"LASTNAME",Lastname);
      sql.get(stm,"Title",Title);
      Text_io.put(Lastname & " - " );
      Text_io.put_line(Title);
    end loop;


    sql.close_cursor(stm);
    sql.commit(trans); 

  sql.Close_Session;
  
exception
  when Event: others =>
  text_io.put_line(gnat.traceback.symbolic.symbolic_traceback(event));  
  
end sql_test;