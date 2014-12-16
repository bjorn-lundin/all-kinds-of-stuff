
with text_io;
with sv_m; use sv_m;
with tst;
procedure p2 is

  package bnl2_Package is new sv_m.Generic_io
          (Identity        => 3000,
           Data_Type       => tst.bnl);

  rec : tst.bnl;
  m: sv_m.message_type;
begin
  begin
    loop
      text_io.put_line("Emptying mailbox");
      sv_m.receive(m,0.1);
    end loop;
  exception
    when others =>
    text_io.put_line("mailbox Empty");
  end;
  loop
    sv_m.receive(m,10.0);
--    sv_m.receive(m);
    rec := bnl2_Package.unpack(m);
    text_io.put_line ("a'Img b c'Img" & rec.a'Img & rec.b & rec.c'Img ); --& rec.d);
    rec.a :=  rec.a + 1 ;
    bnl2_Package.send(Sender(m),rec);
  end loop;
exception
  when sv_m.timeout =>
  text_io.put_line ("Done a'Img b c'Img" & rec.a'Img & rec.b & rec.c'Img);-- & rec.d);
end p2;
