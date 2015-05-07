
with Brickpi;
with Interfaces.C; use Interfaces.C;
with Text_Io; use Text_Io;

procedure Test_All is
  Speed : Int := 0;
  Last_Encoder_Value, 
  This_Encoder_Value : Long := 0;
begin

  Brickpi.Clear_Tick;
  Brickpi.Setup;
  

  Brickpi.Motor_Enable(Brickpi.PORT_A);
  
  Brickpi.Setup_Sensors;

  Brickpi.Update_Values;
  
  Brickpi.Reset_Encoder(BrickPi.PORT_A);
  Brickpi.Update_Values;
  
  Speed := 100;
  
  loop
    Last_Encoder_Value := This_Encoder_Value;
    Brickpi.Motor_Speed(BrickPi.PORT_A, Speed);
    loop 
      Brickpi.Update_Values;  
      This_Encoder_Value := Brickpi.Encoder(BrickPi.PORT_A);
      exit when This_Encoder_Value /= 0;
    end loop;  
    
    Put_Line(Speed'Img & " This_Encoder_Value/Last_Encoder_Value " & This_Encoder_Value'Img & "/" & Last_Encoder_Value'Img);
    
    --delay 0.01;
    --if This_Encoder_Value = Last_Encoder_Value then
    --  if Speed > 0 then
    --    Speed := Speed +1;
    --  elsif Speed < 0 then
    --    Speed := Speed -1;
    --  end if;
    --  
    --  if Speed > 255 then
    --    Speed := 255;
    --  end if;
    --  if Speed < -255 then
    --    Speed := -255;
    --  end if;
    --end if;
    
  
  end loop;
  
  
end Test_All;

