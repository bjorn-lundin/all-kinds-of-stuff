--

--------------------------------------------------------------------------------------------------------------------
with Ada.Unchecked_Conversion;
with SDL2.Error;

package body SDL2.Video.Rectangles is
   use type C.int;

   function Enclose (Points : in Point_Arrays; Clip : in Rectangle; Enclosed : out Rectangle) return Boolean is
      function SDL_Enclose_Points (P    : in Point_Arrays_Pointer;
                                   L    : in C.int;
                                   Clip : in Rectangle_Pointer;
                                   R    : out Rectangle_Pointer) return SDL_Bool ;
      pragma Import(C, SDL_Enclose_Points, "SDL_EnclosePoints");
      Tmp_Points : aliased Point_Arrays := Points;
      Tmp_Clip : aliased Rectangle := Clip;
      Tmp_Enclosed : aliased Rectangle;
      Result : SDL_Bool := SDL_Enclose_Points (Tmp_Points'access, C.int (Points'Length), Tmp_Clip'access, Tmp_Enclosed'access);
   begin
      Enclosed := Tmp_Enclosed;
      return (Result = SDL_True);
   end Enclose;

   procedure Enclose (Points : in Point_Arrays; Enclosed : out Rectangle) is
      function SDL_Enclose_Points (P    : in Point_Arrays_Pointer;
                                   L    : in C.int;
                                   Clip : in Rectangle_Pointer;
                                   R    : out Rectangle_Pointer) return SDL_Bool ;
      pragma Import(C, SDL_Enclose_Points, "SDL_EnclosePoints");
      Tmp_Points : aliased Point_Arrays := Points;
      Tmp_Enclosed : aliased Rectangle;
      Result : SDL_Bool := SDL_Enclose_Points (Tmp_Points'access, C.int (Points'Length), null, Tmp_Enclosed'access);
   begin
      Enclosed := Tmp_Enclosed;
      if Result /= SDL_True then
         raise Rectangle_Error with SDL2.Error.Get;
      end if;
   end Enclose;

   function Has_Intersected (A, B : in Rectangle) return Boolean is
      function SDL_Has_Intersection (A, B : in Rectangle_Pointer) return SDL_Bool ;
      pragma Import(C, SDL_Has_Intersection, "SDL_HasIntersection");
      Tmp_A : aliased Rectangle := A ;
      Tmp_B : aliased Rectangle := B ;
      Result : SDL_Bool := SDL_Has_Intersection (Tmp_A'access, Tmp_B'access);
   begin
      return (Result = SDL_True);
   end Has_Intersected;

   function Intersects (A, B : in Rectangle; Intersection : out Rectangle) return Boolean is
      function SDL_Has_Intersection (A, B : in Rectangle; R : out Rectangle) return SDL_Bool ;
      pragma Import(C, SDL_Has_Intersection, "SDL_IntersectRect");
      Tmp_A : aliased Rectangle := A ;
      Tmp_B : aliased Rectangle := B ;
      Tmp_Intersection : aliased Rectangle ;
      Result : SDL_Bool := SDL_Has_Intersection (Tmp_A'access, Tmp_B'access, R => Tmp_Intersection'access);
   begin
      Intersection := Tmp_Intersection;
      return (Result = SDL_True);
   end Intersects;

   function Clip_To (Clip_Area : in Rectangle; Line : in out Line_Segment) return Boolean is
      function SDL_Intersect_Rect_And_Line (R : in Rectangle; X1, Y1, X2, Y2 : in out C.int) return SDL_Bool with
      pragma Import(C, SDL_Intersect_Rect_And_Line, "SDL_IntersectRectAndLine");
      X1,X2,Y1,Y2 : aliased C.Int;
      Tmp_Clip_Area : aliased Rectangle := Clip_Area;

      Result : SDL_Bool := SDL_Intersect_Rect_And_Line (Tmp_Clip_Area'access,
                                                        X1'access,
                                                        Y1'access,
                                                        X2'access,
                                                        Y2'access);
   begin
      Line.Start.X  := X1;
      Line.Start.Y  := Y1;
      Line.Finish.X := X2;
      Line.Finish.Y := Y2;
      return (Result = SDL_True);
   end Clip_To;

   function Union (A, B : in Rectangle) return Rectangle is
      procedure SDL_Union_Rect (A, B : in Rectangle; R : out Rectangle) ;
      pragma Import(C, SDL_Union_Rect, "SDL_UnionRect");
      Tmp_A  : aliased Rectangle := A ;
      Tmp_B  : aliased Rectangle := B ;
      Result : aliased Rectangle ;
    begin
      SDL_Union_Rect (Tmp_A'access, Tmp_B'access, Result'access);
      return Result;
   end Union;
end SDL2.Video.Rectangles;
