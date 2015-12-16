
with Interfaces.C; use Interfaces.C;


package SDL2.Video.Rectangles is

  package C renames Interfaces.C;


  type Size is record
       Width  : Int;
       Height : Int;
  end record ;
  pragma Convention(C, Size) ;   
  
  type Size_Arrays is array (C.size_t range <>) of aliased Size ;
  pragma Convention(C, Size_Arrays) ;   

  type Point is record
        X : C.int;
        Y : C.int;
  end record ;
  pragma Convention(C, Point) ;   
 
  type Point_Arrays is array (C.size_t range <>) of aliased Point ;
  pragma Convention(C, Point_Arrays) ;   

  type Point_Arrays_Pointer is access all Point_Arrays ;     
  pragma Convention(C, Point_Arrays_Pointer) ; 

  
  type Line_Segment is record
        Start  : Point;
        Finish : Point;
  end record ;
  pragma Convention(C, Line_Segment) ;   
 
  type Line_Arrays is array (C.size_t range <>) of aliased Line_Segment ;
  pragma Convention(C, Line_Arrays) ;   

  type Rectangle is record
       X      : Int;
       Y      : Int;
       Width  : Int;
       Height : Int;
  end record;
  pragma Convention(C, Rectangle) ;    

  Null_Rectangle : constant Rectangle := (others => 0);
  
  type Rectangle_Pointer is access all Rectangle ;     
  pragma Convention(C, Rectangle_Pointer) ; 
  
 -- type Rectangle_Access is access all Rectangle ;
 -- pragma Convention(C, Rectangle_Access) ;    

  type Rectangle_Arrays is array (C.size_t range <>) of aliased Rectangle ;
  pragma Convention(C, Rectangle_Arrays) ;    


  function Enclose (Points : in Point_Arrays; Clip : in Rectangle; Enclosed : out Rectangle) return Boolean;
  procedure Enclose (Points : in Point_Arrays; Enclosed : out Rectangle);
  
  function Has_Intersected (A, B : in Rectangle) return Boolean;
  
  function Intersects (A, B : in Rectangle; Intersection : out Rectangle) return Boolean;
  
  function Clip_To (Clip_Area : in Rectangle; Line : in out Line_Segment) return Boolean;
  
  function Union (A, B : in Rectangle) return Rectangle;
  
  
  

end SDL2.Video.Rectangles ;
