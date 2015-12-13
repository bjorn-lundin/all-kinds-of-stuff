

package SDL2.Video.Colors is


  type Color_Component is range 0 .. 255 ;
  for Color_Component'size use 8;
  pragma Convention(C, Color_Component) ;    

  type Color is record
       Red   : Color_Component;
       Green : Color_Component;
       Blue  : Color_Component;
       Alpha : Color_Component;
  end record ;
  for Color'size use Color_Component'Size * 4;
  pragma Convention(C, Color) ;    

  for Color use record
       Red   at 0 range  0 ..  7;
       Green at 0 range  8 .. 15;
       Blue  at 0 range 16 .. 23;
       Alpha at 0 range 24 .. 31;
  end record;

  type RGB_Color is record
       Red   : Color_Component;
       Green : Color_Component;
       Blue  : Color_Component;
  end record;
  pragma Convention(C, RGB_Color) ;    



end SDL2.Video.Colors;