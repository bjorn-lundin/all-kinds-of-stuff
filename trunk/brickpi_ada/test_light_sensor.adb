

with Interfaces.C; use Interfaces.C;
with Brickpi;
with Text_Io; use Text_Io;

procedure Test_Light_Sensor is
  Brick : Brickpi.Brick_Pi_Record_Pointer := Brickpi.Get_Pointer_To_Brick_Pi;
  R : Int := -2;
begin

  Brickpi.Clear_Tick;
  R := Brickpi.Setup;
  Put_Line("Setup result " & R'Img);
  if R > 0 then 
    return;
  end if;
  
  Brick.Address(1) := 1;
  Brick.Address(2) := 2;

  
  Brick.Sensor_Type(BrickPi.PORT_4) := Brickpi.TYPE_SENSOR_LIGHT_ON;
  Brick.Sensor_Type(BrickPi.PORT_3) := Brickpi.TYPE_SENSOR_LIGHT_OFF;
  R := Brickpi.Setup_Sensors;
  
  Put_Line("Setup_Sensors result " & R'Img);
  if R > 0 then 
    return;
  end if;

  loop
    
    R := BrickPi.Update_Values;
   -- Put_Line("Update_Values result " & R'Img);
    if R > 0 then 
      return;
    end if;
    
    Put("Sensor 3 " &  Brick.Sensor_Value(BrickPi.PORT_3)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_3,1)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_3,2)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_3,3)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_3,4)'Img);
    Put(" Sensor 4 " &  Brick.Sensor_Value(BrickPi.PORT_4)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_4,1)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_4,2)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_4,3)'Img);
    Put(Brick.Sensor_Array(BrickPi.PORT_4,4)'Img);
    New_Line;
    
    delay 0.1;
  
  end loop;
  
  
end Test_Light_Sensor;

