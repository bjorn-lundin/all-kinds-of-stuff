--------------------------------------------------------------------------------------------------------------------
--  Copyright (c) 2014-2015 Luke A. Guest
--
--  This software is provided 'as-is', without any express or implied
--  warranty. In no event will the authors be held liable for any damages
--  arising from the use of this software.
--
--  Permission is granted to anyone to use this software for any purpose,
--  including commercial applications, and to alter it and redistribute it
--  freely, subject to the following restrictions:
--
--     1. The origin of this software must not be misrepresented; you must not
--     claim that you wrote the original software. If you use this software
--     in a product, an acknowledgment in the product documentation would be
--     appreciated but is not required.
--
--     2. Altered source versions must be plainly marked as such, and must not be
--     misrepresented as being the original software.
--
--     3. This notice may not be removed or altered from any source
--     distribution.
--------------------------------------------------------------------------------------------------------------------
with Interfaces.C;
with SDL2.Error;

package body SDL2.Video.Renderers is
   package C renames Interfaces.C;

   use type C.int;
   use type Renderer_Pointer;

   type Internal_Flip is mod 2 ** 32 with
     Convention => C;

   --  type Internal_Flip_Array is array (Renderer_Flip) of Internal_Flip;

   Internal_Flip_None       : constant Internal_Flip := 16#0000_0000#;
   Internal_Flip_Horizontal : constant Internal_Flip := 16#0000_0001#;
   Internal_Flip_Vertical   : constant Internal_Flip := 16#0000_0002#;

   Internal_Flips : constant array (Renderer_Flip) of Internal_Flip :=
     (Internal_Flip_None,
      Internal_Flip_Horizontal,
      Internal_Flip_Vertical,
      Internal_Flip_Horizontal or Internal_Flip_Vertical);


   function Total_Drivers return Positive is
      function SDL_Get_Num_Render_Drivers return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetNumRenderDrivers";

      Result : C.int := SDL_Get_Num_Render_Drivers;
   begin
      if Result < C.int (Positive'First) then
         raise Renderer_Error with SDL2.Error.Get;
      end if;

      return Positive (Result);
   end Total_Drivers;

   overriding
   procedure Finalize (Self : in out Renderer) is
      procedure SDL_Destroy_Renderer (R : in Renderer_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_DestroyRenderer";
   begin
      if Self.Internal /= null and then Self.Owns then
         SDL_Destroy_Renderer (Self.Internal);

         Self.Internal := null;
      end if;
   end Finalize;

   function Get_Blend_Mode (Self : in Renderer) return SDL2.Video.Textures.Blend_Modes is
      function SDL_Get_Render_Draw_Blend_Mode (R : in Renderer_Pointer;
                                               M : out SDL2.Video.Textures.Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderDrawBlendMode";

      Mode   : SDL2.Video.Textures.Blend_Modes;
      Result : C.int := SDL_Get_Render_Draw_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;

      return Mode;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Renderer; Mode : in SDL2.Video.Textures.Blend_Modes) is
      function SDL_Set_Render_Draw_Blend_Mode (R : in Renderer_Pointer;
                                               M : in SDL2.Video.Textures.Blend_Modes) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderDrawBlendMode";

      Result : C.int := SDL_Set_Render_Draw_Blend_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Draw_Colour (Self : in Renderer) return SDL2.Video.Palettes.Colour is
      function SDL_Get_Render_Draw_Color
        (R                       : in Renderer_Pointer;
         Red, Green, Blue, Alpha : out SDL2.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderDrawColor";

      Colour : SDL2.Video.Palettes.Colour;
      Result : C.int := SDL_Get_Render_Draw_Color (Self.Internal, Colour.Red, Colour.Green, Colour.Blue, Colour.Alpha);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
      return Colour;
   end Get_Draw_Colour;

   procedure Set_Draw_Colour (Self : in out Renderer; Colour : in SDL2.Video.Palettes.Colour) is
      function SDL_Set_Render_Draw_Color
        (R                       : in Renderer_Pointer;
         Red, Green, Blue, Alpha : in SDL2.Video.Palettes.Colour_Component) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderDrawColor";

      Result : C.int := SDL_Set_Render_Draw_Color (Self.Internal, Colour.Red, Colour.Green, Colour.Blue, Colour.Alpha);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Set_Draw_Colour;

   procedure Clear (Self : in out Renderer) is
      function SDL_Render_Clear (R : in Renderer_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderClear";
      Result : C.int := SDL_Render_Clear (Self.Internal);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Clear;

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL2.Video.Textures.Texture) is

      function SDL_Render_Copy
        (R         : in Renderer_Pointer;
         T         : in SDL2.Video.Textures.Texture_Pointer;
         Src, Dest : in SDL2.Video.Rectangles.Rectangle_Access) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopy";

      Result : C.int := SDL_Render_Copy (Self.Internal,
                                         Copy_From.Get_Internal,
                                         null,
                                         null);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Copy;

   --  TODO: Check to make sure this works, if it does, apply the same logic to CopyEx, see below.
   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL2.Video.Textures.Texture;
      From      : in SDL2.Video.Rectangles.Rectangle;
      To        : in SDL2.Video.Rectangles.Rectangle) is

      function SDL_Render_Copy
        (R         : in Renderer_Pointer;
         T         : in SDL2.Video.Textures.Texture_Pointer;
         Src, Dest : in SDL2.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopy";
      Tmp_From : aliased SDL2.Video.Rectangles.Rectangle := From;
      Tmp_To : aliased SDL2.Video.Rectangles.Rectangle := To;

      Result : C.int := SDL_Render_Copy (Self.Internal,
                                         Copy_From.Get_Internal,
                                         Tmp_From'unchecked_access,
                                         Tmp_To'unchecked_access);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Copy;
   
   --bnl
   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL2.Video.Textures.Texture;
      To        : in SDL2.Video.Rectangles.Rectangle) is

      function SDL_Render_Copy
        (R         : in Renderer_Pointer;
         T         : in SDL2.Video.Textures.Texture_Pointer;
         Src,Dest  : in SDL2.Video.Rectangles.Rectangle_Access) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopy";
      Tmp_To : aliased SDL2.Video.Rectangles.Rectangle := To;
      Result : C.int := SDL_Render_Copy (Self.Internal,
                                         Copy_From.Get_Internal,
                                         null,
                                         Tmp_To'unchecked_access);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Copy;
   --bnl

   --  TODO: See above, rearrange the params so that the rectangles are the last elements and make
   --  them default to null_rectangle.
   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL2.Video.Textures.Texture;
      From      : in SDL2.Video.Rectangles.Rectangle;
      To        : in SDL2.Video.Rectangles.Rectangle;
      Angle     : in Long_Float;
      Centre    : in SDL2.Video.Rectangles.Point;
      Flip      : in Renderer_Flip) is

      function SDL_Render_Copy_Ex
        (R         : in Renderer_Pointer;
         T         : in SDL2.Video.Textures.Texture_Pointer;
         Src, Dest : in SDL2.Video.Rectangles.Rectangle;
         A         : in C.double;
         Centre    : in SDL2.Video.Rectangles.Point;
         F         : in Internal_Flip) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderCopyEx";

      Result : C.int := SDL_Render_Copy_Ex (Self.Internal,
                                            Copy_From.Get_Internal,
                                            From,
                                            To,
                                            C.double (Angle),
                                            Centre,
                                            Internal_Flips (Flip));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Copy;

   procedure Draw (Self : in out Renderer; Line : in SDL2.Video.Rectangles.Line_Segment) is
      function SDL_Render_Draw_Line (R              : in Renderer_Pointer;
                                     X1, Y1, X2, Y2 : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawLine";

      Result : C.int := SDL_Render_Draw_Line (Self.Internal, Line.Start.X, Line.Start.Y, Line.Finish.X, Line.Finish.Y);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Draw;

   --  TODO: Check this works!
   procedure Draw (Self : in out Renderer; Lines : in SDL2.Video.Rectangles.Line_Arrays) is
      --  As the records and arrays are defined as C types, an array of lines is also an array of points.
      function SDL_Render_Draw_Lines (R     : in Renderer_Pointer;
                                      P     : in SDL2.Video.Rectangles.Line_Arrays;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawLines";

      Result : C.int := SDL_Render_Draw_Lines (Self.Internal, Lines, C.int (Lines'Length * 2));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Point : in SDL2.Video.Rectangles.Point) is
      function SDL_Render_Draw_Point (R : in Renderer_Pointer; X, Y : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoint";

      Result : C.int := SDL_Render_Draw_Point (Self.Internal, Point.X, Point.Y);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Points : in SDL2.Video.Rectangles.Point_Arrays) is
      type Point_Arrays_Ptr is access all SDL2.Video.Rectangles.Point_Arrays;
      function SDL_Render_Draw_Points (R     : in Renderer_Pointer;
                                       P     : in Point_Arrays_Ptr;
                                       Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoints";

        Tmp_P : aliased SDL2.Video.Rectangles.Point_Arrays := Points;
      Result : C.int := SDL_Render_Draw_Points (Self.Internal, Tmp_P'access, C.int (Points'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangle : in SDL2.Video.Rectangles.Rectangle) is
      type Rectangle_Ptr is access all SDL2.Video.Rectangles.Rectangle;
      function SDL_Render_Draw_Rect (R    : in Renderer_Pointer;
                                     Rect : in Rectangle_Ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRect";
      Tmp_R : aliased  SDL2.Video.Rectangles.Rectangle := Rectangle;
      Result : C.int := SDL_Render_Draw_Rect (Self.Internal, Tmp_R'access);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangles : in SDL2.Video.Rectangles.Rectangle_Arrays) is
      type Rectangle_Arrays_Ptr is access all SDL2.Video.Rectangles.Rectangle_Arrays;
      function SDL_Render_Draw_Rects (R     : in Renderer_Pointer;
                                      Rect  : in Rectangle_Arrays_Ptr;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRects";
        
      Tmp_RA : aliased SDL2.Video.Rectangles.Rectangle_Arrays := Rectangles;
      Result : C.int := SDL_Render_Draw_Rects (Self.Internal, Tmp_RA'access, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Fill (Self : in out Renderer; Rectangle : in SDL2.Video.Rectangles.Rectangle) is
      type Rectangle_Ptr is access all SDL2.Video.Rectangles.Rectangle;
      function SDL_Render_Fill_Rect (R    : in Renderer_Pointer;
                                     Rect : in Rectangle_Ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderFillRect";
      Tmp_R : aliased  SDL2.Video.Rectangles.Rectangle := Rectangle;
      Result : C.int := SDL_Render_Fill_Rect (Self.Internal, Tmp_R'access);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Fill;

   procedure Fill (Self : in out Renderer; Rectangles : in SDL2.Video.Rectangles.Rectangle_Arrays) is
      type Rectangle_Arrays_Ptr is access all SDL2.Video.Rectangles.Rectangle_Arrays;
      function SDL_Render_Fill_Rects (R     : in Renderer_Pointer;
                                      Rect  : in SDL2.Video.Rectangles.Rectangle_Arrays;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderFillRects";
      Tmp_RA : aliased SDL2.Video.Rectangles.Rectangle_Arrays := Rectangles;
      Result : C.int := SDL_Render_Fill_Rects (Self.Internal, Tmp_RA'access, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Fill;

   procedure Get_Clip (Self : in Renderer; Rectangle : out SDL2.Video.Rectangles.Rectangle) is
      type Rectangle_Ptr is access all SDL2.Video.Rectangles.Rectangle;
      procedure SDL_Render_Get_Clip_Rect (R    : in Renderer_Pointer;
                                          Rect : out SDL2.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetClipRect";
      Tmp_R : aliased  SDL2.Video.Rectangles.Rectangle := Rectangle;
   begin
      SDL_Render_Get_Clip_Rect (Self.Internal, Tmp_R'access);
   end Get_Clip;

   procedure Set_Clip (Self : in out Renderer; Rectangle : in SDL2.Video.Rectangles.Rectangle) is
      type Rectangle_Ptr is access all SDL2.Video.Rectangles.Rectangle;
      function SDL_Render_Set_Clip_Rect (R    : in Renderer_Pointer;
                                         Rect : in SDL2.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetClipRect";

      Tmp_R : aliased  SDL2.Video.Rectangles.Rectangle := Rectangle;
      Result : C.int := SDL_Render_Set_Clip_Rect (Self.Internal, Tmp_R'access);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Set_Clip;

   procedure Get_Logical_Size (Self : in Renderer; Size : out SDL2.Video.Rectangles.Size) is
      procedure SDL_Render_Get_Logical_Size (R : in Renderer_Pointer;
                                             W : out Interfaces.C.Int;
                                             H : out Interfaces.C.Int ) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetLogicalSize";
      We,He : aliased Interfaces.C.Int ;
   begin
      SDL_Render_Get_Logical_Size (Self.Internal, We'access, He'access);
      Size := SDL2.Video.Rectangles.Size'(We,He);
   end Get_Logical_Size;

   procedure Set_Logical_Size (Self : in out Renderer; Size : in SDL2.Video.Rectangles.Size) is
      function SDL_Render_Set_Logical_Size (R : in Renderer_Pointer;
                                            W : in Interfaces.C.Int;
                                            H : in Interfaces.C.Int ) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetLogicalSize";
      We : aliased Interfaces.C.Int := Size.W;
      He : aliased Interfaces.C.Int := Size.H;
      Result : C.int := SDL_Render_Set_Logical_Size (Self.Internal, We'access, He'access);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Set_Logical_Size;

   procedure Get_Scale (Self : in Renderer; X, Y : out Float) is
      type C_Float_Ptr is access all C.C_Float;
      procedure SDL_Render_Get_Scale (R : in Renderer_Pointer; 
                                      X, Y : out C_Float_Ptr) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetScale";
      Xa : aliased Interfaces.C.C_float;
      Ya : aliased Interfaces.C.C_float;
   begin
      SDL_Render_Get_Scale (Self.Internal,Xa'access, Ya'access);
      X := Float(Xa);
      Y := Float(Ya);
   end Get_Scale;

   procedure Set_Scale (Self : in out Renderer; X, Y : in Float) is
      function SDL_Render_Set_Scale (R : in Renderer_Pointer; X, Y : in C.C_float) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetScale";
      Result : C.int := SDL_Render_Set_Scale (Self.Internal, C.C_float (X), C.C_float (Y));
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Set_Scale;

   procedure Get_Viewport (Self : in Renderer; Rectangle : out SDL2.Video.Rectangles.Rectangle) is
      type Rectangle_Ptr is access all SDL2.Video.Rectangles.Rectangle;
      procedure SDL_Render_Get_Viewport (R    : in Renderer_Pointer;
                                         Rect : out SDL2.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetViewport";
      Tmp_R : aliased  SDL2.Video.Rectangles.Rectangle := Rectangle;
   begin
      SDL_Render_Get_Viewport (Self.Internal, Tmp_R'access);
   end Get_Viewport;

   procedure Set_Viewport (Self : in out Renderer; Rectangle : in SDL2.Video.Rectangles.Rectangle) is
      type Rectangle_Ptr is access all SDL2.Video.Rectangles.Rectangle;
      function SDL_Render_Set_Viewport (R    : in Renderer_Pointer;
                                        Rect : in SDL2.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetViewport";

      Tmp_R : aliased  SDL2.Video.Rectangles.Rectangle := Rectangle;
      Result : C.int := SDL_Render_Set_Viewport (Self.Internal, Tmp_R'access);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Set_Viewport;

   procedure Present (Self : in Renderer) is
      procedure SDL_Render_Present (R : in Renderer_Pointer) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderPresent";
   begin
      SDL_Render_Present (Self.Internal);
   end Present;

   function Supports_Targets (Self : in Renderer) return Boolean is
      function SDL_Render_Target_Supported (R : in Renderer_Pointer) return SDL_Bool with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderTargetSupported";
   begin
      return SDL_Render_Target_Supported (Self.Internal) = SDL_True ;
   end Supports_Targets;

   procedure Set_Target (Self : in out Renderer; Target : in SDL2.Video.Textures.Texture) is
      function SDL_Set_Render_Target (R : in Renderer_Pointer;
                                      T : in SDL2.Video.Textures.Texture_Pointer) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_SetRenderTarget";

      Result : C.int := SDL_Set_Render_Target (Self.Internal, Target.Get_Internal);
   begin
      if Result /= Success then
         raise Renderer_Error with SDL2.Error.Get;
      end if;
   end Set_Target;

   function Get_Renderer (Window : in SDL2.Video.Windows.Window) return Renderer is
      function SDL_Get_Renderer (W : in SDL2.Video.Windows.Windows_Pointer) return Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_GetRenderer";
   begin
      return Result : constant Renderer :=
        (Ada.Finalization.Limited_Controlled with
           Internal => SDL_Get_Renderer (Window.Get_Internal, Owns => False) do
         null;
      end return;
   end Get_Renderer;

   
   function Get_Internal (Self : in Renderer) return Renderer_Pointer is
   begin
      return Self.Internal;
   end Get_Internal;
   
   procedure Create
     (Rend   : in out Renderer;
      Window : in out SDL.Video.Windows.Window;
      Driver : in Positive;
      Flags  : in Renderer_Flags := Default_Renderer_Flags) is

      function SDL_Create_Renderer (W : in SDL2.Video.Windows.Windows_Pointer; Index : in C.int; Flags : in Renderer_Flags)
                                    return Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRenderer";
   begin
      Rend.Internal := SDL_Create_Renderer (Window.Get_Internal, C.int (Driver), Flags);
      Rend.Owns     := True; --?
   end Create;

   procedure Create
     (Rend   : in out Renderer;
      Window : in out SDL.Video.Windows.Window;
      Flags  : in Renderer_Flags := Default_Renderer_Flags) is

      function SDL_Create_Renderer (W : in SDL2.Video.Windows.Windows_Pointer; Index : in C.int; Flags : in Renderer_Flags)
                                    return Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateRenderer";
   begin
      Rend.Internal := SDL_Create_Renderer (Window.Get_Internal, -1, Flags);
      Rend.Owns     := True;
   end Create;

   procedure Create
     (Rend    : in out Renderer;
      Surface : in SDL.Video.Surfaces.Surface) is

      function SDL_Create_Software_Renderer (S : in SDL.Video.Surfaces.Surface_Pointer)
                                             return Renderer_Pointer with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_CreateSoftwareRenderer";
   begin
      Rend.Internal := SDL_Create_Software_Renderer (Surface.Get_Internal);
      Rend.Owns     := True;
   end Create;
   
end SDL2.Video.Renderers;
