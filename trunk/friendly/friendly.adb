--nothing changed, just test
with Logging; use Logging;
with Calendar2; use Calendar2;

procedure Friendly is
  Min_Friend            : Positive := 1;
  Max_Friend            : constant Positive := 1_000_000_000;
  Debug_Print_Out       : constant Positive := 100_000;
  ---------------------------------------------------------
  Start : constant Calendar2.Time_Type := Calendar2.Clock;
  Stop  :          Calendar2.Time_Type := Calendar2.Time_Type_Last;
  
  task type Calculate_Type is
    entry Start(Id, From, To : Positive);
    entry Get_Time( T : in out Calendar2.Time_Type);
  end Calculate_Type;
  
  
  task body Calculate_Type is
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
    Local_Id, Local_From, Local_To : Positive := 1;
    Sum : array (1..2) of Natural := (others => 0);
   
  begin
    accept Start(Id, From, To : Positive) do
      Local_From := From;
      Local_To   := To;
    end ;
    for i in Local_From .. Local_To loop  
      if i mod Debug_Print_Out = 0 then
        Log ("I'm" & Local_id'Img & " still working, i" & i'Img);
      end if;
      
      Sum(1) := Sum_Of_Divisors(i);
      Sum(2) := Sum_Of_Divisors(Sum(1));
      
      if i = Sum(2) and then i /= Sum(1) then
        Log ("i,sum " & i'Img & "," & Sum(1)'Img );
      end if;    
    end loop;  
    
    accept  Get_Time( T : in out Calendar2.Time_Type) do
      T := Calendar2.Clock;
    end ;
  end Calculate_Type;
  
  subtype Calc_Index is Positive range 1 ..4 ;
  Calculators : array(Calc_Index'range) of Calculate_Type;
begin
  for i in Calc_Index'range loop
    Calculators(i).Start(i,1,50_000);
  end loop;
  
  for i in Calc_Index'range loop
    Calculators(i).Get_Time(Stop);
    Log ("Time elapsed for i :" & i'Img & " " & Calendar2.String_Interval(Stop - Start));
  end loop;
  
  
end Friendly;
