

with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;
with Display;
with Text_io;
procedure Test_Lcd is
  Lcd : Display.Lcd_Type;
  Tmp : String (1..4) := (others => ' ');
begin

  Lcd.Open;
  Lcd.Display_On;
  Lcd.Clear;

  for i in 1 .. 20 loop
    Move(I'Img,Tmp);
    Lcd.Write(" " & Trim(Tmp,Both));
  end loop;
  Lcd.Clear;
  
  for i in 1 .. 20 loop
    Move(I'Img,Tmp);
    Lcd.Home;
    Lcd.Write(" " & Trim(Tmp,Both));
  end loop;

  Lcd.Clear;

  for Col in Display.Column_Type range 1..14 loop
    for Row in Display.Row_Type'range loop    
     -- Text_Io.Put_Line("Row/Col" & Row'Img & "/" & Col'Img);
     -- Move(Trim(Col'Img,Both),Tmp);
      Move("10",Tmp);
      Lcd.GotoRC(Row => Row, Col => Col);
      Lcd.Write(Trim(Tmp,Both));
      --delay 1.0;
    end loop;
  end loop;

  
  Lcd.Display_Off;
  Lcd.Close;



end Test_Lcd;