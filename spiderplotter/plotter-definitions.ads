
package Plotter.Definitions is

  type Id_Type is new Integer range 1 ..3 ;
  type X_Coordinate_Type is new Integer range -10_000 .. 10_000 ;
  type Y_Coordinate_Type is new Integer range -10_000 .. 10_000 ;
  
  
  task Motor_Controller is
    entry Init;
    entry Goto_XY(X : X_Coordinate_Type; Y : Y_Coordinate_Type );
    entry Stop;
  end Motor_Controller;
  -----------------------------------------------
  task Pen_Controller is
    entry Init;
    entry Pen_Up;
    entry Pen_Down;
    entry Stop;
  end Pen_Controller;
  -----------------------------------------------
  
  
 -- procedure Goto_XY(X : X_Coordinate_Type; Y : Y_Coordinate_Type );
 -- procedure Pen_Up;
 -- procedure Pen_Down;
  
  
                                                
end Plotter.Definitions;

