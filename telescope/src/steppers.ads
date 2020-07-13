
--with Motors;
package Steppers is


  --type Speed_Type is (Normal, Fast);

 -- procedure Set_Speed(Speed: in Motors.Speed_Type);
  procedure Focus_Plus;
  procedure Focus_Minus;
  procedure No_Direction;

  procedure Stop;
  procedure Init;

  procedure Test;

end Steppers;
