
with Ada.Strings;         use Ada.Strings;
with Ada.Strings.Fixed;   use Ada.Strings.Fixed;


with Interfaces.C; use Interfaces.C;
with Brickpi.Thin;
with Text_Io; use Text_Io;
with Display;

procedure Test_Motor is
  V,F,R : Int := -2;
  Brick : Brickpi.Thin.Brick_Pi_Record_Pointer := Brickpi.Thin.Get_Pointer_To_Brick_Pi;
  
  Lcd : Display.Lcd_Type;
  tmp : string (1..4) := (others => ' ');
  
begin
  Lcd.Open;
  Lcd.Clear;
  
  for i in 1 .. 200 loop
    Move(I'Img,Tmp);
    Lcd.Home;
    Lcd.Write(" " & Trim(Tmp,Both));
    delay 0.1;
  end loop;
  
  Lcd.Close;
  return;

  Brickpi.Thin.Clear_Tick;
  R := Brickpi.Thin.Setup;
  Put_Line("Setup result " & R'Img);
  if R > 0 then 
    return;
  end if;
  
  Brick.Address(1) := 1;
  Brick.Address(2) := 2;

  Brick.Motor_Enable(Brickpi.PORT_A) := 1;
  Brick.Motor_Enable(Brickpi.PORT_B) := 1;
  R := Brickpi.Thin.Setup_Sensors;
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
    
    R := Brickpi.Thin.Update_Values;
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
  
  
end Test_Motor;

