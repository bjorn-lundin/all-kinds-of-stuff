

with Interfaces.C; use Interfaces.C;
with Brickpi.Thin;
with Brickpi.Constants;
with Text_Io; use Text_Io;

procedure Test_Light_Sensor is
  Brick : Brickpi.Thin.Brick_Pi_Record_Pointer := Brickpi.Thin.Get_Pointer_To_Brick_Pi;
  R : Int := -2;
begin

 -- Brickpi.Thin.Clear_Tick;
  R := Brickpi.Thin.Setup;
  Put_Line("Setup result " & R'Img);
  if R > 0 then 
    return;
  end if;
  
--  Brick.Address(1) := 1;
--  Brick.Address(2) := 2;

  
-- Brick.Sensor_Type(BrickPi.Thin.PORT_4) := Brickpi.Constants.TYPE_SENSOR_LIGHT_ON;
--  Brick.Sensor_Type(BrickPi.Thin.PORT_3) := Brickpi.Constants.TYPE_SENSOR_LIGHT_OFF;
  Brick.Sensor_Type(BrickPi.Thin.PORT_4) := Brickpi.Constants.RETURN_VERSION;
  R := Brickpi.Thin.Setup_Sensors;
  
  Put_Line("Setup_Sensors result " & R'Img);
  if R > 0 then 
    return;
  end if;

  loop
    
    R := Brickpi.Thin.Update_Values;
   -- Put_Line("Update_Values result " & R'Img);
    if R > 0 then 
      return;
    end if;
    
    Put("version= " &  Brick.Sensor_Value(BrickPi.Thin.PORT_4)'Img);
    exit;
    
    
    Put("Sensor 3 " &  Brick.Sensor_Value(BrickPi.Thin.PORT_3)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_3,1)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_3,2)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_3,3)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_3,4)'Img);
    Put(" Sensor 4 " &  Brick.Sensor_Value(BrickPi.Thin.PORT_4)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_4,1)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_4,2)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_4,3)'Img);
    Put(Brick.Sensor_Array(BrickPi.Thin.PORT_4,4)'Img);
    New_Line;
    
    delay 0.1;
  
  end loop;
  
  
end Test_Light_Sensor;

