

with Text_IO; use Text_IO;
with Stacktrace;
with Motors;
with Gpio;

--with Ada.Command_Line;

procedure ttt_robot is


  Motor_Z   : Motors.Motor_Task renames Motors.M(1);
  Motor_Fi1 : Motors.Motor_Task renames Motors.M(2); -- hip/weist
  Motor_Fi2 : Motors.Motor_Task renames Motors.M(3); --elbow
begin

  delay 1.0;
  Put_Line("setup");
  Gpio.Setup;



  -- Magnet
  Gpio.Pin_Mode( 9, Gpio.OUTPUT);
  Gpio.Digital_Write( 9, Gpio.LOW);

  --12v power
  Gpio.Pin_Mode(21, Gpio.OUTPUT);
  Gpio.Digital_Write(21, Gpio.LOW);

--  delay 1.0;

--  for i in 1..10 loop
--    Put_Line("Turn on relay for power to the A4988s" & i'img);
--    Gpio.Digital_Write(21,Gpio.HIGH);
--    delay 10.0;
--
--    Put_Line("Turn OFF relay for power to the A4988s" & i'img);
--    Gpio.Digital_Write(21,Gpio.low);
--    delay 10.0;
--  end loop;



--return;

  --Init The Motors with pins
  Put_Line("Config and init motors");
  declare
    use Motors;
  begin
    Motor_Fi1.Config(Configuration_Pin => (Step => 16, Direction => 12, Enable => 20, Emergency_Stop => 1), Direction_Towards_Emergency_Stop => CCw, Name => 1);
    Motor_Fi2.Config(Configuration_Pin => (Step => 19, Direction => 13, Enable => 26, Emergency_Stop => 0), Direction_Towards_Emergency_Stop => Cw,  Name => 2);
    Motor_Z.Config  (Configuration_Pin => (Step => 27, Direction => 22, Enable =>  6, Emergency_Stop => 4), Direction_Towards_Emergency_Stop => CCw, Name => 3);
  end;

--    if Ada.Command_Line.Argument_Count > 0 then
--      sStep := Motors.Step_Type'Value(Ada.Command_Line.Argument(1));
--    end if;

  Motors.Safe_Home;

  for J in 1 ..5 loop
    Put_Line("big loop" & J'Img);

    Motor_Z.Goto_Step(300.0);
    Motor_Fi2.Goto_Step(200.0);

    for I in 2 .. 7 loop
      Put_Line("Motorfi1.stuff normal" & I'Img);
      Motor_Fi1.Goto_Step(Motors.Step_Type(I * 100));
    end loop;

    for I in  reverse 2 .. 6 loop
      Put_Line("Motorfi1.stuff reverse" & I'Img);
      Motor_Fi1.Goto_Step(Motors.Step_Type(I * 100));
    end loop;

    Motor_Z.Goto_Step(200.0);
    Motor_Fi2.Goto_Step(300.0);
  end loop;

exception
    when E: others =>
      Stacktrace.Tracebackinfo(E);

end ttt_robot;
