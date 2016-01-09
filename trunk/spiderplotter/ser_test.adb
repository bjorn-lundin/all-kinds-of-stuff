
with Ser_Com ;
with Text_Io; Use Text_Io;

procedure Ser_Test is

  Serial : aliased Ser_Com.Serial_Port;
  Cnt : Integer := 0;
  Buffer : String(1..10);
  Last   : Natural;
  Hello : String := "Hello World";

begin
  Ser_Com.Open(Port => Serial, Name => Ser_Com.Name (1));  
  Serial.Set(Timeout => 1.0, Flow => Ser_Com.None);

  Main : loop
    Cnt := Cnt +1;
    for i in Hello'range loop
      Put(Hello(i));
      New_Line;
      Serial.Write(Hello(i));
    end loop;
    Serial.Write(Ascii.LF);

    Serial.Read(Buffer,Last);
    if Last /= 0 then
      Put_Line("did read " & Last'Img) ;
      Put_Line(Buffer(1..last)) ;
    else
      Put_Line("No reply");
    end if;
    exit Main when Cnt >= 10;   
  end loop Main;
  Serial.Write('Q');
  Serial.Close;

end Ser_Test;
