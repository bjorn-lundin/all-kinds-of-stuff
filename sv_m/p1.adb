with text_io;
with sv_m; use sv_m;
with tst;
with sattmate_calendar;

procedure p1 is



  package bnl2_Package is new sv_m.Generic_io
          (Identity        => 3000,
           Data_Type       => tst.bnl);

  rec : tst.bnl;
  m: sv_m.message_type;
begin
  begin
    loop
      text_io.put_line("Emptying mailbox ");
      sv_m.receive(m,0.1);
    end loop;
  exception
    when others =>
      text_io.put_line("mailbox Empty");
  end;
  rec.a := 0;
--  rec.d(1005..1006) := "AB";
  text_io.put_line(sattmate_calendar.STRING_TIME  (MILLISECONDS => TRUE));
  loop
    rec.a := rec.a + 1;
    text_io.put_line ("Before send");
    bnl2_Package.send(("p2             ",(others => ' ')),rec);
    text_io.put_line ("After send");
    text_io.put_line ("Before receive");
    sv_m.receive(m);
    text_io.put_line ("After receive");
--    sv_m.receive(m,10.0);
    text_io.put_line ("Before unpack");
    rec := bnl2_Package.unpack(m);
    text_io.put_line ("After unpack");
    text_io.put_line ("a'Img b c'Img' " & rec.a'Img & rec.b & rec.c'Img);-- & rec.d);
    exit when rec.a >= 1_0;
  end loop;
  text_io.put_line(sattmate_calendar.STRING_TIME  (MILLISECONDS => TRUE));
  text_io.put_line ("Done a'" &  rec.a'Img & "' b: '" & rec.b & "' c'" & rec.c'Img);-- & "' d '" & rec.d & "'");
  text_io.put_line ("rec size" & rec'Size'Img);
end p1;
