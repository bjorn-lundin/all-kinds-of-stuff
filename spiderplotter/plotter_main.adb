
--with Ada.Command_line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
--with Interfaces.C;

with Plotter.Definitions; use Plotter.Definitions;
with Gpio;

procedure Plotter_Main is


   Start_X,X : X_Coordinate_Type := 0;
   Start_Y,Y : Y_Coordinate_Type := 0;

  -- L1,L2 : Float := 0.0;

  Buffer : String(1..100) := (others => 'w');
  Len : Natural := 0;
  File : File_Type;

begin

  Put_Line("setup");
  Gpio.Setup;

  Put_Line("Motor_Controller.Init");
  Motor_Controller.Init;

  Put_Line("Pen_Controller.Init");
  Pen_Controller.Init;

  Pen_Controller.Pen_Down;


  Start_X := 120 ; Start_Y := 150;
  Y := Start_Y - 2;
  X := Start_X - 2;

  --Open(File, In_File, "dat/bird_positive_1.dat");
  --begin
  --  loop
  --    Y:= Y + 2;
  --    Get_Line(File, Buffer, Len);
  --    for i in 1 .. Len loop
  --      X:= X + 2;
  --      if Buffer(i) = 'X' then
  --        Put_Line("Pen_Controller.Pen_Up");
  --        Pen_Controller.Pen_Up;
  --        Motor_Controller.Goto_XY(X,Y);
  --        Put_Line("Pen_Controller.Pen_Down");
  --        Pen_Controller.Pen_Down;
  --      end if;
  --    end loop;
  --    X:= Start_X - 2;
  --  end loop;
  --exception
  --  when End_Error => Close(File);
  --end ;

--  Y_Loop : loop
--    Y:= Y + 2;
--    exit Y_Loop when Y > 300;
--    X_Loop : loop
--      X:= X + 2;
--      exit X_Loop when X > 250;
--      Put_Line("Pen_Controller.Pen_Up");
--      Pen_Controller.Pen_Up;
--      Motor_Controller.Goto_XY(X,Y);
--      Put_Line("Pen_Controller.Pen_Down");
--      Pen_Controller.Pen_Down;
--    end loop X_Loop;
--    X:= Start_X - 2;
--  end loop Y_Loop;

  Put_Line("Pen_Controller.Pen_Up");
  Pen_Controller.Pen_Up;

  Motor_Controller.Goto_XY(200,350);

  Put_Line("wait to quit");
  Motor_Controller.Stop;
  Put_Line("Motor_Controller.Stopped");
  Pen_Controller.Stop;
  Put_Line("Pen_Controller.Stopped");

end Plotter_Main;
