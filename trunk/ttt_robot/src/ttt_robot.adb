with Text_IO; use Text_IO;
with Stacktrace;
with Motors;
with Gpio;

procedure ttt_robot is


  Motor_Z   : Motors.Motor_Task renames Motors.M(1);
  Motor_Fi1 : Motors.Motor_Task renames Motors.M(2);
  Motor_Fi2 : Motors.Motor_Task renames Motors.M(3);

begin


  Put_Line("setup");
  Gpio.Setup;

  --Init The Motors with pins
  Put_Line("Config and init motors");
  declare
    use Motors;
  begin
    Motor_Fi1.Config(Configuration_Pin => (Step => 16, Direction => 12, Enable => 20, Emergency_Stop => 2), Direction_Towards_Emergency_Stop => Cw);
    Motor_Fi2.Config(Configuration_Pin => (Step => 19, Direction => 13, Enable => 26, Emergency_Stop => 3), Direction_Towards_Emergency_Stop => Cw);
    Motor_Z.Config  (Configuration_Pin => (Step =>  5, Direction => 22, Enable =>  6, Emergency_Stop => 4), Direction_Towards_Emergency_Stop => Cw);
  end;

  Put_Line("MotorFi1.Home start");
  Motor_Fi1.Home;
  Put_Line("MotorFi1.Home stop");

  Put_Line("MotorFi1.Goto_Step start");
  Motor_Fi1.Goto_Step(100);
  Put_Line("MotorFi1.Goto_Step stop");

  exception
    when E: others =>
      Stacktrace.Tracebackinfo(E);

end ttt_robot;

