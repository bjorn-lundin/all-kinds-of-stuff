
with Interfaces.C; use Interfaces.C;


package SDL2.Video.Rectangles is

  type Size is record
       Width  : Int;
       Height : Int;
  end record ;
  pragma Convention(C, Size) ;    

  type Rectangle is record
       X      : Int;
       Y      : Int;
       Width  : Int;
       Height : Int;
  end record;
  pragma Convention(C, Rectangle) ;    
      
  type Rectangle_Pointer is access all Rectangle ;     
  pragma Convention(C, Rectangle_Pointer) ;    


end SDL2.Video.Rectangles ;
