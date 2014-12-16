
with Gameport_Io, Ada.Text_Io ; --,Gnat.Traceback.Symbolic;
use  Gameport_Io, Ada.Text_Io;


procedure Joy is
 Gp      : Byte := 0;
 Pressed : Boolean := False;
 pos     : Position_Type;
begin
-- we need access to the gameport
  Get_Gameport_Access;
--   Read from the gameport and display the result
--  loop
    Set_Gameport(255);
    Gp := Get_Gameport;
--    Put_line("status: 201 " & byte'image(gp));
    for j in Joystick_Type'range loop
      for b in Bit_Type'range loop
        Put_Line("Is_pressed : " & Joystick_Type'image(j) & " " &
                                Bit_Type'image(b) & " " &
                                Boolean'image(Is_Pressed(j,b,Gp)));
      end loop;
    end loop;
    Pos := Get_Position(Joystick_1,10);
    Put_Line("Direction: " & Direction_type'image(Get_Direction(Pos)));
--    put_line("X: " & pos.X'img & " Y: " & pos.Y'img);
--    new_line;
--  end loop;

-- We don't need the port anymore
  Release_Gameport_Access;

exception
  when Event : others => 
    null; --Put_Line (Gnat.Traceback.Symbolic.Symbolic_Traceback (Event));
end Joy;
