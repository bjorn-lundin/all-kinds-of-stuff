
--with Ada.Interrupts; use Ada.Interrupts;
--with Ada.Interrupts.Names; use Ada.Interrupts.Names;


package Motors is

  type Pin_Number_Type is new Integer range 0 .. 40;
--  type Coordinate_Type is new Integer range -10_000 .. 10_000;
  type Motor_Index_Type is new Integer range 2 .. 3;
  type Motor_State_Type is (Starting, Running);
  type Speed_Type is (Slow, Normal);
  type Direction_Type is (None, Up, Down, Ccw, Cw);

  Delay_Time : array (Speed_Type'Range) of Duration := (Slow => 0.005, Normal => 0.001);
  -- 1 us is absolutly min


  type Pins_Type is (Step,Direction,Enable,Emergency_Stop);
  type Pin_Array_Type is array (Pins_Type'Range) of Pin_Number_Type;

  procedure Set_Direction(Direction : Direction_Type);
  procedure Set_Speed(Speed : Speed_Type);

  task type Motor_Task is
    entry Config(Configuration_Pin : Pin_Array_Type; Name : Motor_Index_Type);
    entry Go;
    entry Stop;
  end Motor_Task;


  M : array (Motor_Index_Type'range) of Motor_Task;



--    protected Handler is
--      procedure Set(P : Pin_Type);
--      pragma Unreserve_All_Interrupts;
--      procedure Handle_Sigint;
--      pragma Interrupt_Handler(Handle_Sigint);
--      pragma Attach_Handler(Handle_Sigint, Sigint);
--
--    private
--      A : Pin_Type := 0;
--      B : Pin_Type := 0;
--      C : Pin_Type := 0;
--    end Handler;


end Motors;
