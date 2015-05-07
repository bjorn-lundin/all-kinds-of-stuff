
with Brickpi.Thin;
with Text_Io; use Text_Io;
with Interfaces.C; use Interfaces.C;

package body Brickpi is
  Retry_Time : Duration := 0.01;  
  Brick : Brickpi.Thin.Brick_Pi_Record_Pointer := Brickpi.Thin.Get_Pointer_To_Brick_Pi;

  procedure Setup is
    R : Int := 0;
  begin  
    Brick.Address(1) := 1;
    Brick.Address(2) := 2;
  
    loop
      R := Brickpi.Thin.Setup;
      Put_Line("Setup result " & R'Img);
      exit when R = 0;
      delay Retry_Time;
    end loop;
  end Setup;
  
  
  procedure Setup_Sensors is
    R : Int := 0;
  begin  
    loop 
      R := Brickpi.Thin.Setup_Sensors;
      Put_Line("Setup_Sensors result " & R'Img);
      exit when R = 0;
      delay Retry_Time;
    end loop;  
  end Setup_Sensors;
  
  
  
  procedure Update_Values is
    R : Int := 0;
  begin  
    loop
      R := Brickpi.Thin.Update_Values;
      Put_Line("Update_Values result " & R'Img);
      exit when R = 0;
      delay Retry_Time;
   end loop;    
  end Update_Values;
  
  
  procedure Clear_Tick is
  begin
    Brickpi.Thin.Clear_Tick;
  end Clear_Tick;

  
  
  procedure Motor_Enable(Port : Motor_Port_Type) is
  begin
    Brick.Motor_Enable(Port) := 1;
  end Motor_Enable;
  
  procedure Motor_Disable(Port : Motor_Port_Type) is
  begin
    Brick.Motor_Enable(Port) := 0;
  end Motor_Disable;


  procedure Reset_Encoder(Port : Motor_Port_Type) is
  begin
    Brick.Encoder_Offset(Port) := Brick.Encoder(Port);
  end Reset_Encoder;
 

  procedure Motor_Speed(Port : Motor_Port_Type; Speed : Speed_Type) is
  begin
    Brick.Motor_Speed(Port):= Speed;
  end Motor_Speed;

  function Encoder(Port : Motor_Port_Type) return Interfaces.C.Long is
  begin
    return  Brick.Encoder(Port);
  end Encoder;

 
end Brickpi;
