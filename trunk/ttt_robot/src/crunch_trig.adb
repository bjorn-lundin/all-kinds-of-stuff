with Stacktrace;
with Text_Io; use Text_Io;
with Ada.Numerics.Generic_Elementary_Functions;


procedure crunch_trig is

  package Math is new Ada.Numerics.Generic_Elementary_Functions(Float);
  use Math;

  Cycle : constant Float := 360.0;

  L1 : constant Float := 94.0;
  L2 : constant Float := 103.0;

  Fi1 : constant Float := 1.8 * 20.0/72.0;
  Fi2 : constant Float := 1.8 * 660.0/3844.0;

  Alfa1 : Float := 0.0;
  Alfa2 : Float := 0.0;

 -- Tic1 : Integer := 0;
 -- Tic2 : Integer := 0;

  Maxtic : constant Integer := 200*1000;  --200 tics/rev

  Epsilon : constant Float := 0.001;

  X12 : Float := -4.5;
  Y12 : Float := 170.6;

  X22 : Float := 81.1;
  Y22 : Float := 150.0;

  X : Float := 0.0;
  Y : Float := 0.0;

  M1 : Float := Sqrt(X12*X12 + Y12*Y12);
  M2 : Float := Sqrt(X22*X22 + Y22*Y22);

  D1 : Float := 0.0;

begin


  One :
  for T1 in 1 .. Maxtic loop
    for T2 in 1 .. Maxtic loop
      Alfa1 := Float(T1) * Fi1;
      Alfa2 := Float(T2) * Fi2;

      X := L1 * Cos(X => Alfa1, Cycle => Cycle) + L2 * Cos(X => Alfa2, Cycle => Cycle);
      Y := L1 * Sin(X => Alfa1, Cycle => Cycle) + L2 * Sin(X => Alfa2, Cycle => Cycle);

      D1 := abs(Sqrt(X*X + Y*Y) -  M1);
      if D1 < Epsilon then

        Put_Line("-------case 1 -------");
        Put_Line("D1 " & D1'Img);
        Put_Line("X12 " & X12'Img);
        Put_Line("Y12 " & Y12'Img);
        Put_Line("A1 " & Alfa1'Img & " " & T1'img);
        Put_Line("A2 " & Alfa2'Img & " " & T2'img);
        Put_Line("--------------------");
        Put_Line("");
        exit One;
      end if;


    end loop;
  end loop One;


  Two:
  for T1 in 1 .. Maxtic loop
    for T2 in 1 .. Maxtic loop
      Alfa1 := Float(T1) * Fi1;
      Alfa2 := Float(T2) * Fi2;

      X := L1 * Cos(X => Alfa1, Cycle => Cycle) + L2 * Cos(X => Alfa2, Cycle => Cycle);
      Y := L1 * Sin(X => Alfa1, Cycle => Cycle) + L2 * Sin(X => Alfa2, Cycle => Cycle);

      if Sqrt(X*X + Y*Y) -  M2 < Epsilon then
        Put_Line("-------case 2 -------");
        Put_Line("X12 " & X22'Img);
        Put_Line("Y12 " & Y22'Img);
        Put_Line("A1 " & Alfa1'Img & " " & T1'img);
        Put_Line("A2 " & Alfa2'Img & " " & T2'img);
        Put_Line("--------------------");
        Put_Line("");
        exit Two;
      end if;


    end loop;
  end loop Two;


exception
  when E: others =>
    Stacktrace.Tracebackinfo(E);
end crunch_trig;
