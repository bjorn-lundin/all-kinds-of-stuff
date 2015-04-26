

with Interfaces.C; use Interfaces.C;
with Brickpi;
with Text_Io; use Text_Io;

procedure Motor_Test is
  V,F,R : Int := -2;
  Brick : Brickpi.Brick_Pi_Record_Pointer := Brickpi.Get_Pointer_To_Brick_Pi;
begin

  Brickpi.Clear_Tick;
  R := Brickpi.Setup;
  Put_Line("Setup result " & R'Img);
  if R > 0 then 
    return;
  end if;
  
  Brick.Address(1) := 1;
  Brick.Address(2) := 2;

  Brick.Motor_Enable(Brickpi.PORT_A) := 1;
  Brick.Motor_Enable(Brickpi.PORT_B) := 1;
  R := Brickpi.Setup_Sensors;
  Put_Line("Setup_Sensors result " & R'Img);
  if R > 0 then 
    return;
  end if;

  loop
    if F = 1  then
      Brick.Motor_Speed(BrickPi.PORT_A):= 80;
      Brick.Motor_Speed(BrickPi.PORT_B):= 100;
      if v > 30 then
        F := 0;
        V := 0;
      end if;
      v := v+1;
    else
      Brick.Motor_Speed(BrickPi.PORT_A):= -80;
      Brick.Motor_Speed(BrickPi.PORT_B):= -100;
      if v > 30 then
        F := 1;
        V := 0;
      end if;
      v := v+1;      
    end if;
    
    R := BrickPi.Update_Values;
    Put_Line("Update_Values result " & R'Img);
    if R > 0 then 
      return;
    end if;
    
    Put_Line("Encoder A " & Brick.Encoder(BrickPi.PORT_A)'Img);
    Put_Line("Encoder B " & Brick.Encoder(BrickPi.PORT_B)'Img);

    Put_Line("Encoder_Offset A " & Brick.Encoder_Offset(BrickPi.PORT_A)'Img);
    Put_Line("Encoder_Offset B " & Brick.Encoder_Offset(BrickPi.PORT_B)'Img);
    
    delay 0.1;
  
  end loop;
  
  
end Motor_Test;

