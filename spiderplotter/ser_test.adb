

with Ser_Com ;
with Text_Io; Use Text_Io;
with Ada.Streams;

with Ada.Unchecked_Conversion;


procedure Ser_Test is

  Serial : aliased Ser_Com.Serial_Port;
  ---C : Character;

  Cnt : Integer := 0;
  Buffer : Ada.Streams.Stream_Element_Array(1..10);
  Last   : Ada.Streams.Stream_Element_Offset;

  subtype Chars is Character range '!' .. 'Z';
  use type Ada.Streams.Stream_Element_Offset;
 -- use type Ada.Streams.Stream_Element_Array;

  Hello : String := "Hello World";

 -----------------------------
   -- To_Stream_Element_Array --
   -----------------------------

   function To_Stream_Element_Array (This : in String)
                                     return    Ada.Streams.Stream_Element_Array
   is
      use Ada.Streams;
      subtype StringX is String (1 .. This'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. This'Length);
      function To_Arr is new Ada.Unchecked_Conversion (StringX, ArrayX);
   begin
      return To_Arr (This);
   end To_Stream_Element_Array;

   ---------------
   -- To_string --
   ---------------

   function To_string (This : in Ada.Streams.Stream_element_array)
      return String
   is
      use Ada.Streams;
      subtype StringX is String (1 .. This'Length);
      subtype ArrayX  is Stream_Element_Array (1 .. This'Length);
      function To_Str is new Ada.Unchecked_Conversion (ArrayX, StringX);
   begin
      return To_Str (This);
   end To_string;


begin

  Ser_Com.Open (Port => Serial, Name => Ser_Com.Name (1));
  Serial.Set
     (Rate      => Ser_Com.B9600,
      Bits      => Ser_Com.CS8,
      Stop_Bits => Ser_Com.One,
      Parity    => Ser_Com.None,
      Block     => True,
      Timeout   => 1.0,
      Flow      => Ser_Com.None);

--  loop
--   -- Put_Line("will read") ;
--    Serial.Read(Buffer,Last);
--    if Last /= 0 then
--      Put_Line("did read " & Last'Img) ;
--      Put_Line(To_string(Buffer(1..last))) ;
--    end if;
--    exit when To_string(Buffer(1..1)) = "Q";   
--  end loop;


  loop
    Cnt := Cnt +1;
    for i in Hello'range loop
      Put(Hello(i));
      New_Line;
      Character'Write (Serial'access, Hello(i));
    end loop;
    Character'Write (Serial'access, Ascii.LF);


    Serial.Read(Buffer,Last);
    if Last /= 0 then
      Put_Line("did read " & Last'Img) ;
      Put_Line(To_string(Buffer(1..last))) ;
    else
      Put_Line("No reply");
    end if;
    exit when Cnt >= 10;
   
  end loop;
  Character'Write (Serial'access, 'Q');


  Serial.Close;

end Ser_Test;
