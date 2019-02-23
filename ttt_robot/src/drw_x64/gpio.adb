

package body Gpio is
  
  ---------------------------------------------------------
  procedure Setup is
  begin
      null;
      raise Bad_GPIO_Call with "Wiring_Pi_Setup_Gpio";
  end Setup;
  ---------------------------------------------------------
  procedure Digital_Write(Pin : Interfaces.C.Int; Value : Boolean) is
  begin
      null;
      raise Bad_GPIO_Call with "Digital_Write";
  end Digital_Write;
  ------------------------------------------------------
  procedure Digital_Read(Pin : Interfaces.C.Int; Value : out Boolean) is
  begin
      null;
      raise Bad_GPIO_Call with "Digital_Read";
  end Digital_Write;
  ------------------------------------------------------
  procedure Pin_Mode(Pin : Interfaces.C.Int ; Mode : Interfaces.C.Int) is
  begin
      null;
      raise Bad_GPIO_Call with "Pin_Mode";
  end Pin_Mode;
  
  
end Gpio;