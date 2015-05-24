
with Brickpi.Thin;
with Ada.Command_line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;

procedure Create_Constants is
begin

  if Argument_Count = 1 then
    if Argument(1) = "--brick" then
      Brickpi.Thin.Print_Constants;
    elsif Argument(1) = "--io" then
      Brickpi.Thin.Print_Io_Constants;
    else
      Put_Line (Standard_Error,"arg can only be 'brick' or 'io'");    
    end if;  
  else
    Put_Line (Standard_Error,"arg can only be 'brick' or 'io' and exactly 1 arg");    
  end if;
end Create_Constants;

