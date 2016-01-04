
with Ada.Text_IO; use Ada.Text_IO;

with Plotter.Definitions; use Plotter.Definitions;
with Gpio;

procedure Close_Ports is


begin
  Put_Line("setup");
  Gpio.Setup;

  Put_Line("Motor_Controller.Init");
  Motor_Controller.Init;

  Put_Line("Pen_Controller.Init");
  Pen_Controller.Init;

  Put_Line("wait to quit");


  Motor_Controller.Stop;
  Put_Line("Motor_Controller.Stopped");

  Pen_Controller.Stop;
  Put_Line("Pen_Controller.Stopped");

end Close_Ports;

