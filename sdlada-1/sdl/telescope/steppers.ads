
package Steppers is


  type Speed_Type is (Normal, Fast);

  procedure Set_Speed(Speed: in Speed_Type);
  procedure Up;
  procedure Down;
  procedure Right;
  procedure Left;
  procedure Stop;
  procedure Init;

end Steppers;
