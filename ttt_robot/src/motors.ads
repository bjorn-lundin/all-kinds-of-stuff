

package Motors is

  type Pin_Type is new Integer range 0 .. 40;
  type Coordinate_Type is new Integer range -10_000 .. 10_000;
  type Step_Type is new Integer range -10_000 .. 10_000;
  type Motor_State_Type is (Starting, Running);
  type Speed_Type is (Slow, Normal);
  subtype Direction_Type is Boolean;
  Cw  : constant Direction_Type := True;
  CCw : constant Direction_Type := False;

  Delay_Time : array (Speed_Type'Range) of Duration := (Slow => 0.01, Normal => 0.005);


  type Pins_Type is (Step,Direction,Enable,Emergency_Stop);
  type Pin_Array_Type is array (Pins_Type'Range) of Pin_Type;

  task type Motor_Task is
    entry Config(Configuration_Pin : Pin_Array_Type; Direction_Towards_Emergency_Stop : Direction_Type; Name : Positive);
    entry Home;
    entry Goto_Step(S : Step_Type);
  end Motor_Task;


  M : array (1..3) of Motor_Task;
  
  
  procedure Safe_Home;
  
  
end Motors;
