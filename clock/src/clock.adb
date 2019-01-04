with Text_Io; use Text_Io;
with Steppers;


procedure Clock is

  procedure Log(Who,What : in String) is
  begin
    --Put_Line(Calendar2.Clock.To_String & " " & Who & " " & What);
    Put_Line( Who & " " & What);
  end Log;


begin
  Log("main","init steppers");
  Steppers.Init;
-- Log("main","test start");
--  Steppers.Test;
 Log("main","Do_Clock");
  Steppers.Do_Clock;
  Log("main","test stop");
  Steppers.Stop;
exception
  when others =>
    Steppers.Stop;

end Clock;
