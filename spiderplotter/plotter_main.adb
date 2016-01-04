
--with Ada.Command_line; use Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
--with Interfaces.C;

with Plotter.Definitions; use Plotter.Definitions;
with Gpio;

procedure Plotter_Main is


   X : X_Coordinate_Type := 0;
   Y : Y_Coordinate_Type := 0;


begin
  Put_Line("setup");
  Gpio.Setup;

  Put_Line("Motor_Controller.Init");
  Motor_Controller.Init;

  Put_Line("Pen_Controller.Init");
  Pen_Controller.Init;


  loop
    X := X + 100;
    Y := Y + 100;

    Put_Line("Pen_Controller.Pen_Up");
    Pen_Controller.Pen_Up;

    exit when X >= 300;

    Motor_Controller.Goto_XY(X,Y);
    Put_Line("Pen_Controller.Pen_Down");
    Pen_Controller.Pen_Down;

  end loop;


  Put_Line("wait to quit");


  Motor_Controller.Stop;
  Put_Line("Motor_Controller.Stopped");


  Pen_Controller.Stop;
  Put_Line("Pen_Controller.Stopped");

end Plotter_Main;

