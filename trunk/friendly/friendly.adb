--nothing changed, just test
with Text_Io;
with Ada.Command_Line;

procedure Friendly is
  Min_Friend            : Positive := 1;
  Max_Friend            : constant Positive := 1_000_000_000;
  Debug_Print_Out       : constant Positive := 100_000;
  Sum : array (1..2) of Natural := (others => 0);
  ---------------------------------------------------------
  function Sum_Of_Divisors(Num : in Integer) return Natural is
    Local_Sum : Natural := 0;
  begin
    for j in 1 .. Num -1 loop
      if Num mod j = 0 then -- is divisor
        Local_Sum := Local_Sum + j;
      end if;      
    end loop;
    return Local_Sum;    
  end Sum_Of_Divisors;
  ---------------------------------------------------------
begin
  if Ada.Command_Line.Argument_Count > 0 then
    Min_Friend := Positive'Value(Ada.Command_Line.Argument(1));
  end if;
  
  for i in Min_Friend .. Max_Friend loop  
    if i mod Debug_Print_Out = 0 then
      Text_Io.Put_Line ("I'm still working, i " & Positive'Image(i));
    end if;
    
    Sum(1) := Sum_Of_Divisors(i);
    Sum(2) := Sum_Of_Divisors(Sum(1));
    
    if i = Sum(2) and then i /= Sum(1) then
      Text_Io.Put_Line ("i,sum " & Positive'Image(i) & ", " & Positive'Image(Sum(1)) );
    end if;    
  end loop;  
end Friendly;
