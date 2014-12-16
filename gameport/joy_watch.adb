with Gameport_Io, Ada.Text_Io ; 
use  Gameport_Io, Ada.Text_Io;
with Ada.Command_Line;

procedure Joy_Watch is
  Gp      : Byte := 0;
  subtype Constrained_String is string (1..256);     
  type Txt_Pointer_Type is access all Constrained_String; 
  function C_System(Txt_Pointer : Txt_Pointer_Type) return Integer;
  pragma Import (C, C_System, "system");
  Cmd_A : aliased Constrained_String := (others => Ascii.Nul);
  Cmd_B : aliased Constrained_String := (others => Ascii.Nul);
  Poll_Delay, Action_Delay : Duration := 0.0;
  Result : Integer := 0;
  pragma Warnings (Off, Result);
begin

  if Ada.Command_Line.Argument_Count /= 4 then
    Put_Line(Standard_Error," Usage:");
    Put_Line(Standard_Error," joy_watch cmd_a cmd_b poll_delay action_delay");
    Put_Line(Standard_Error,"   cmd_a - What to execute when button A is pressed");
    Put_Line(Standard_Error,"   cmd_b - What to execute when button B is pressed");
    Put_Line(Standard_Error,"   poll_delay   - Time in seconds between each poll delay");
    Put_Line(Standard_Error,"   action_delay - Time in seconds to delay after executing a command due to button pressed");
    Put_Line(Standard_Error,"");
    Put_Line(Standard_Error,"No spaces are allowed in commands. Wrap in shell script if needed");
    Put_Line(Standard_Error,"WILL NEED ROOT ACCESS!");
    return;
  end if;

  declare
    Tmp : String := Ada.Command_Line.Argument(1);
  begin
    Cmd_A(Tmp'range) := Tmp;
  end;  
  declare
    Tmp : String := Ada.Command_Line.Argument(2);
  begin
    Cmd_B(Tmp'range) := Tmp;
  end;  
  Poll_Delay   := Duration'Value(Ada.Command_Line.Argument(3));
  Action_Delay := Duration'Value(Ada.Command_Line.Argument(4));


-- we need access to the gameport
  Get_Gameport_Access;
--   Read from the gameport and display the result
  Set_Gameport(255);
  loop
    Gp := Get_Gameport;
    if    Is_Pressed(Joystick_1, A, Gp) then         
      Result := C_System(Cmd_A'Access);  
      delay Action_Delay;
    elsif Is_Pressed(Joystick_1, B, Gp) then
      Result := C_System(Cmd_B'Access);  
      delay Action_Delay;
    end if; 
    delay Poll_Delay;
  end loop;

-- We don't need the port anymore, but we don't get here, since we do not exit the loop
-- We have to be killed...
  Release_Gameport_Access;

exception
  when Event : others => 
  -- if we are killed, we get here, I think...
    Release_Gameport_Access;
end Joy_Watch;
