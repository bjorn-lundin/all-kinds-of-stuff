
with Interfaces.C;
with Unchecked_Conversion;

package Brickpi is
  use type Interfaces.C.Int;
  
  type Motor_Port_Type is (Port_A, Port_B, Port_C, Port_D);
  for Motor_Port_Type'size use Interfaces.C.Unsigned_Char'Size;
  for Motor_Port_Type use (Port_A => 0, Port_B => 1, Port_C => 2, Port_D => 3);
  function Motor_Port is new Unchecked_Conversion(Motor_Port_Type, Interfaces.C.Unsigned_Char);
  function Motor_Port is new Unchecked_Conversion(Interfaces.C.Unsigned_Char, Motor_Port_Type);
  
  
  Bad_Call : exception;
  procedure Setup ;
  procedure Setup_Sensors ; 
  procedure Update_Values;
  
  procedure Clear_Tick;


  procedure Motor_Enable(Port : Motor_Port_Type);
  
  procedure Motor_Disable(Port : Motor_Port_Type);
  
  procedure Reset_Encoder(Port : Motor_Port_Type);
  
  subtype Speed_Type is Interfaces.C.Int range -255 .. 255;
  procedure Motor_Speed(Port : Motor_Port_Type; Speed : Speed_Type) ;
   
  function Encoder(Port : Motor_Port_Type) return Interfaces.C.Long;
  
end Brickpi;
