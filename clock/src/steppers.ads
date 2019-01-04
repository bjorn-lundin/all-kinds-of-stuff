
package Steppers is


  type Speed_Type is (Normal, Fast);

  procedure Set_Speed(Speed: in Speed_Type);
  procedure Up;
  procedure Down;
  procedure Right;
  procedure Left;
  procedure No_Direction;

  procedure Stop;
  procedure Init;
  procedure Do_Clock;

  procedure Test;

end Steppers;
