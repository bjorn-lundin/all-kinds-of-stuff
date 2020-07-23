
package Steppers is


  type Speed_Type is (Slow, Normal);

  procedure Set_Speed(Speed : Speed_Type);
  procedure Focus_Plus;
  procedure Focus_Minus;
  procedure Focus_Plus_One;
  procedure Focus_Minus_One;
  procedure No_Direction;

  procedure Stop;
  procedure Init;

  procedure Test;

end Steppers;
