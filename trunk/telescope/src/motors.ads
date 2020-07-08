
--with Ada.Interrupts; use Ada.Interrupts;
--with Ada.Interrupts.Names; use Ada.Interrupts.Names;


package Motors is

  type Pin_Type is new Integer range 0 .. 40;
  type Coordinate_Type is new Integer range -10_000 .. 10_000;
  --type Step_Type is new Float range -10_000.0 .. 10_000.0;
  type Motor_Index_Type is new Integer range 1 .. 3;
  type Motor_State_Type is (Starting, Running);
  type Speed_Type is (Slow, Normal);
  type Direction_Type is (None, up,Down,Ccw,Cw);
--    subtype Direction_Type is Boolean;
--    Up   : constant Direction_Type := True;
--    Down : constant Direction_Type := False;
 -- Epsilon : constant Step_Type := 0.501;

  Delay_Time : array (Speed_Type'Range) of Duration := (Slow => 0.01, Normal => 0.005);

  type Pins_Type is (Step,Direction,Enable,Emergency_Stop);
  type Pin_Array_Type is array (Pins_Type'Range) of Pin_Type;

  procedure Set_Direction(Direction : Direction_Type);

  task type Motor_Task is
    entry Config(Configuration_Pin : Pin_Array_Type; Name : Motor_Index_Type);
    entry Go;
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
