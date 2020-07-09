
package body Gpio is
  
  ---------------------------------------------------------
  procedure Setup is
    pragma Warnings(Off);
  begin
      null;
      raise Bad_GPIO_Call with "Wiring_Pi_Setup_Gpio";
  end Setup;
  ---------------------------------------------------------
  procedure Digital_Write(Pin : Interfaces.C.Int; Value : Boolean) is
    pragma Warnings(Off);
  begin
      null;
      raise Bad_GPIO_Call with "Digital_Write";
  end Digital_Write;
  ------------------------------------------------------
  function Digital_Read(Pin : Interfaces.C.Int) return Boolean is 
    pragma Warnings(Off);
  begin
      return True;
      raise Bad_GPIO_Call with "Digital_Read";
  end Digital_Read;
  ------------------------------------------------------
  procedure Pin_Mode(Pin : Interfaces.C.Int ; Mode : Interfaces.C.Int) is
    pragma Warnings(Off);
  begin
      null;
      raise Bad_GPIO_Call with "Pin_Mode";
  end Pin_Mode;
  
  
end Gpio;
