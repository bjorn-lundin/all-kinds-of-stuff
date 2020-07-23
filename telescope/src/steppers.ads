
package Steppers is


  type Speed_Type is (Extremly_Slow, Very_Slow, Slow, Normal);

  procedure Set_Speed(Speed : Speed_Type);
  procedure Set_Slow_Is_Pressed(Val : Boolean) ;


  procedure Focus_Plus;
  procedure Focus_Minus;
  procedure Focus_Plus_Slow;
  procedure Focus_Minus_Slow;
  procedure No_Direction;

  procedure Stop;
  procedure Init;

  procedure Test;

end Steppers;
