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
with System;

with SDL2.Log;

package body SDL2.Video.Renderers is
   package C renames Interfaces.C;

   type Internal_Flip is mod 2 ** 32 ;
   pragma Convention(C,Internal_Flip);

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
      function SDL_Get_Num_Render_Drivers return C.int ;
      pragma Import (C,SDL_Get_Num_Render_Drivers,"SDL_GetNumRenderDrivers");
      Result : C.int := SDL_Get_Num_Render_Drivers;
   begin
      if Result < C.int (Positive'First) then
         raise SDL_Error with SDL2.Error.Get;
      end if;
      return Positive (Result);
   end Total_Drivers;

   overriding
   procedure Finalize (Self : in out Renderer) is
      procedure SDL_Destroy_Renderer (R : in Renderer_Pointer) ;
      pragma Import (C,SDL_Destroy_Renderer,"SDL_DestroyRenderer");
   begin
      if Self.Pointer /= null and then Self.Owner then
         SDL_Destroy_Renderer (Self.Pointer);
         Self.Pointer := null;
      end if;
   end Finalize;

   function Get_Blend_Mode (Self : in Renderer) return SDL2.Video.Textures.Blend_Modes is
      function SDL_Get_Render_Draw_Blend_Mode (R : in Renderer_Pointer;
                                               M : access SDL2.Video.Textures.Blend_Modes) return C.int ;
      pragma Import (C,SDL_Get_Render_Draw_Blend_Mode,"SDL_GetRenderDrawBlendMode");

      Mode   : aliased SDL2.Video.Textures.Blend_Modes;
      Result : C.int := SDL_Get_Render_Draw_Blend_Mode (Self.Pointer, Mode'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
      return Mode;
   end Get_Blend_Mode;

   procedure Set_Blend_Mode (Self : in out Renderer; Mode : in SDL2.Video.Textures.Blend_Modes) is
      function SDL_Set_Render_Draw_Blend_Mode (R : in Renderer_Pointer;
                                               M : in SDL2.Video.Textures.Blend_Modes) return C.int ;
      pragma Import (C,SDL_Set_Render_Draw_Blend_Mode,"SDL_SetRenderDrawBlendMode");
      Result : C.int := SDL_Set_Render_Draw_Blend_Mode (Self.Pointer, Mode);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Set_Blend_Mode;

   function Get_Draw_Color (Self : in Renderer) return SDL2.Video.Palettes.Color is
      function SDL_Get_Render_Draw_Color
        (R                       : in Renderer_Pointer;
         Red, Green, Blue, Alpha : access SDL2.Video.Palettes.Color_Component) return C.int ;
      pragma Import (C,SDL_Get_Render_Draw_Color,"SDL_GetRenderDrawColor");
      Color : SDL2.Video.Palettes.Color;
      R,G,B,A : aliased SDL2.Video.Palettes.Color_Component;
      Result : C.int := SDL_Get_Render_Draw_Color (Self.Pointer, R'access, G'access, B'access, A'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
      Color := (R, G, B, A);
      return Color;
   end Get_Draw_Color;

   procedure Set_Draw_Color (Self : in out Renderer; Color : in SDL2.Video.Palettes.Color) is
      function SDL_Set_Render_Draw_Color
        (R                       : in Renderer_Pointer;
         Red, Green, Blue, Alpha : in SDL2.Video.Palettes.Color_Component) return C.int ;
      pragma Import (C,SDL_Set_Render_Draw_Color,"SDL_SetRenderDrawColor");
      Result : C.int := SDL_Set_Render_Draw_Color (Self.Pointer, Color.Red, Color.Green, Color.Blue, Color.Alpha);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Set_Draw_Color;

   procedure Clear (Self : in out Renderer) is
      function SDL_Render_Clear (R : in Renderer_Pointer) return C.int;
      pragma Import (C,SDL_Render_Clear,"SDL_RenderClear");
      Result : C.int := SDL_Render_Clear (Self.Pointer);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Clear;

   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL2.Video.Textures.Texture) is

      function SDL_Render_Copy
        (R         : in Renderer_Pointer;
         T         : in SDL2.Texture_Pointer;
         Src       : access SDL2.Video.Rectangles.Rectangle;
         Dest      : access SDL2.Video.Rectangles.Rectangle) return C.int ;
      pragma Import (C,SDL_Render_Copy,"SDL_RenderCopy");
      Result : C.int := SDL_Render_Copy (Self.Pointer,
                                         Copy_From.Get_Pointer,
                                         null,
                                         null);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
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
         T         : in SDL2.Texture_Pointer;
         Src       : access SDL2.Video.Rectangles.Rectangle;
         Dest      : access SDL2.Video.Rectangles.Rectangle) return C.int ;
      pragma Import (C,SDL_Render_Copy,"SDL_RenderCopy");
      Tmp_From : aliased SDL2.Video.Rectangles.Rectangle := From;
      Tmp_To : aliased SDL2.Video.Rectangles.Rectangle := To;

      Result : C.int := SDL_Render_Copy (Self.Pointer,
                                         Copy_From.Get_Pointer,
                                         Tmp_From'access,
                                         Tmp_To'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Copy;

   --bnl
   procedure Copy
     (Self      : in out Renderer;
      Copy_From : in SDL2.Video.Textures.Texture;
      To        : in SDL2.Video.Rectangles.Rectangle) is

      function SDL_Render_Copy
        (R         : in Renderer_Pointer;
         T         : in SDL2.Texture_Pointer;
         Src       : access SDL2.Video.Rectangles.Rectangle;
         Dest      : access SDL2.Video.Rectangles.Rectangle) return C.int ;
      pragma Import (C,SDL_Render_Copy,"SDL_RenderCopy");
      Tmp_To : aliased SDL2.Video.Rectangles.Rectangle := To;
      Result : C.int := SDL_Render_Copy (Self.Pointer,
                                         Copy_From.Get_Pointer,
                                         null,
                                         Tmp_To'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
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
      Center    : in SDL2.Video.Rectangles.Point;
      Flip      : in Renderer_Flip) is

      function SDL_Render_Copy_Ex
        (R         : in Renderer_Pointer;
         T         : in SDL2.Texture_Pointer;
         Src, Dest : access SDL2.Video.Rectangles.Rectangle;
         A         : in C.Double;
         Center    : access SDL2.Video.Rectangles.Point;
         F         : in Internal_Flip) return C.int;
      pragma Import (C,SDL_Render_Copy_Ex,"SDL_RenderCopyEx");

      Tmp_From : aliased SDL2.Video.Rectangles.Rectangle := From;
      Tmp_To : aliased SDL2.Video.Rectangles.Rectangle := To;
      Tmp_Center : aliased SDL2.Video.Rectangles.Point := Center;
      Result : C.int := SDL_Render_Copy_Ex (Self.Pointer,
                                            Copy_From.Get_Pointer,
                                            Tmp_From'access,
                                            Tmp_To'access,
                                            C.double (Angle),
                                            Tmp_Center'access,
                                            Internal_Flips (Flip));
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Copy;

   procedure Draw (Self : in out Renderer; Line : in SDL2.Video.Rectangles.Line_Segment) is
      function SDL_Render_Draw_Line (R              : in Renderer_Pointer;
                                     X1, Y1, X2, Y2 : in C.int) return C.int ;
      pragma Import (C,SDL_Render_Draw_Line,"SDL_RenderDrawLine");
      Result : C.int := SDL_Render_Draw_Line (Self.Pointer, Line.Start.X, Line.Start.Y, Line.Finish.X, Line.Finish.Y);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Draw;

   --  TODO: Check this works!
   procedure Draw (Self : in out Renderer; Lines : in SDL2.Video.Rectangles.Line_Arrays) is
      --  As the records and arrays are defined as C types, an array of lines is also an array of points.
      function SDL_Render_Draw_Lines (R     : in Renderer_Pointer;
                                      P     : access SDL2.Video.Rectangles.Line_Arrays;
                                      Count : in C.int) return C.int ;
      pragma Import (C,SDL_Render_Draw_Lines,"SDL_RenderDrawLines");
      Tmp_Lines : aliased SDL2.Video.Rectangles.Line_Arrays := Lines;
      Result : C.int := SDL_Render_Draw_Lines (Self.Pointer, Tmp_Lines'access, C.int (Lines'Length * 2));
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Point : in SDL2.Video.Rectangles.Point) is
      function SDL_Render_Draw_Point (R : in Renderer_Pointer; X, Y : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoint";

      Result : C.int := SDL_Render_Draw_Point (Self.Pointer, Point.X, Point.Y);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Points : in SDL2.Video.Rectangles.Point_Arrays) is
      function SDL_Render_Draw_Points (R     : in Renderer_Pointer;
                                       P     : in System.Address;
                                       Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawPoints";

      Result : C.int := SDL_Render_Draw_Points (Self.Pointer, Points'address, C.int (Points'Length));
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangle : in SDL2.Video.Rectangles.Rectangle) is
      type Rectangle_Ptr is access all SDL2.Video.Rectangles.Rectangle;
      function SDL_Render_Draw_Rect (R    : in Renderer_Pointer;
                                     Rect : in Rectangle_Ptr) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRect";
      Tmp_R : aliased SDL2.Video.Rectangles.Rectangle := Rectangle;
      Result : C.int := SDL_Render_Draw_Rect (Self.Pointer, Tmp_R'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Draw;

   procedure Draw (Self : in out Renderer; Rectangles : in SDL2.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Render_Draw_Rects (R     : in Renderer_Pointer;
                                      Rect  : in System.Address;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderDrawRects";

      Result : C.int := SDL_Render_Draw_Rects (Self.Pointer, Rectangles'address, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
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
      Result : C.int := SDL_Render_Fill_Rect (Self.Pointer, Tmp_R'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Fill;

   procedure Fill (Self : in out Renderer; Rectangles : in SDL2.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Render_Fill_Rects (R     : in Renderer_Pointer;
                                      Rect  : in System.Address;
                                      Count : in C.int) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderFillRects";
      Result : C.int := SDL_Render_Fill_Rects (Self.Pointer, Rectangles'address, C.int (Rectangles'Length));
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Fill;

   procedure Get_Clip (Self : in Renderer; Rectangle : out SDL2.Video.Rectangles.Rectangle) is
      procedure SDL_Render_Get_Clip_Rect (R    : in Renderer_Pointer;
                                          Rect : access SDL2.Video.Rectangles.Rectangle) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetClipRect";
      Tmp_R : aliased SDL2.Video.Rectangles.Rectangle := SDL2.Video.Rectangles.Null_Rectangle;
   begin
      SDL_Render_Get_Clip_Rect (Self.Pointer, Tmp_R'access);
      Rectangle := Tmp_R;
   end Get_Clip;

   procedure Set_Clip (Self : in out Renderer; Rectangle : in SDL2.Video.Rectangles.Rectangle) is
      function SDL_Render_Set_Clip_Rect (R    : in Renderer_Pointer;
                                         Rect : access SDL2.Video.Rectangles.Rectangle) return C.int with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderSetClipRect";

      Tmp_R : aliased SDL2.Video.Rectangles.Rectangle := Rectangle;
      Result : C.int := SDL_Render_Set_Clip_Rect (Self.Pointer, Tmp_R'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Set_Clip;

   procedure Get_Logical_Size (Self : in Renderer; Size : out SDL2.Video.Rectangles.Size) is
      procedure SDL_Render_Get_Logical_Size (R : in Renderer_Pointer;
                                             W : access Interfaces.C.Int;
                                             H : access Interfaces.C.Int ) with
        Import        => True,
        Convention    => C,
        External_Name => "SDL_RenderGetLogicalSize";
      We,He : aliased Interfaces.C.Int ;
   begin
      SDL_Render_Get_Logical_Size (Self.Pointer, We'access, He'access);
      Size := SDL2.Video.Rectangles.Size'(We,He);
   end Get_Logical_Size;

   procedure Set_Logical_Size (Self : in out Renderer; Size : in SDL2.Video.Rectangles.Size) is
      function SDL_Render_Set_Logical_Size (R : in Renderer_Pointer;
                                            W : Interfaces.C.Int;
                                            H : Interfaces.C.Int ) return C.int ;
      pragma Import (C, SDL_Render_Set_Logical_Size,"SDL_RenderSetLogicalSize");
      Result : C.int := SDL_Render_Set_Logical_Size (Self.Pointer, Size.Width, Size.Height);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Set_Logical_Size;

   procedure Get_Scale (Self : in Renderer; X, Y : out Float) is
      procedure SDL_Render_Get_Scale (R : in Renderer_Pointer;
                                      X : access C.C_Float;
                                      Y : access C.C_Float) ;
      pragma Import (C, SDL_Render_Get_Scale,"SDL_RenderGetScale");
      Xa : aliased C.C_float;
      Ya : aliased C.C_float;
   begin
      SDL_Render_Get_Scale (Self.Pointer,Xa'access, Ya'access);
      X := Float(Xa);
      Y := Float(Ya);
   end Get_Scale;

   procedure Set_Scale (Self : in out Renderer; X, Y : in Float) is
      function SDL_Render_Set_Scale (R : in Renderer_Pointer; X, Y : in C.C_float) return C.int ;
      pragma Import (C, SDL_Render_Set_Scale,"SDL_RenderSetScale");
      Result : C.int := SDL_Render_Set_Scale (Self.Pointer, C.C_float (X), C.C_float (Y));
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Set_Scale;

   procedure Get_Viewport (Self : in Renderer; Rectangle : out SDL2.Video.Rectangles.Rectangle) is
      procedure SDL_Render_Get_Viewport (R    : in Renderer_Pointer;
                                         Rect : access SDL2.Video.Rectangles.Rectangle) ;
      pragma Import (C, SDL_Render_Get_Viewport,"SDL_RenderGetViewport");
      Tmp_R : aliased SDL2.Video.Rectangles.Rectangle := SDL2.Video.Rectangles.Null_Rectangle;
   begin
      SDL_Render_Get_Viewport (Self.Pointer, Tmp_R'access);
      Rectangle := Tmp_R;
   end Get_Viewport;

   procedure Set_Viewport (Self : in out Renderer; Rectangle : in SDL2.Video.Rectangles.Rectangle) is
      function SDL_Render_Set_Viewport (R    : in Renderer_Pointer;
                                        Rect : access SDL2.Video.Rectangles.Rectangle) return C.int ;
      pragma Import (C, SDL_Render_Set_Viewport,"SDL_RenderSetViewport");

      Tmp_R : aliased SDL2.Video.Rectangles.Rectangle := Rectangle;
      Result : C.int := SDL_Render_Set_Viewport (Self.Pointer, Tmp_R'access);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Set_Viewport;

   procedure Present (Self : in Renderer) is
      procedure SDL_Render_Present (R : in Renderer_Pointer) ;
      pragma Import (C, SDL_Render_Present,"SDL_RenderPresent");
   begin
      SDL_Render_Present (Self.Pointer);
   end Present;

   function Supports_Targets (Self : in Renderer) return Boolean is
      function SDL_Render_Target_Supported (R : in Renderer_Pointer) return SDL_Bool ;
      pragma Import (C, SDL_Render_Target_Supported,"SDL_RenderTargetSupported");
   begin
      return SDL_Render_Target_Supported (Self.Pointer) = SDL_True ;
   end Supports_Targets;

   procedure Set_Target (Self : in out Renderer; Target : in SDL2.Video.Textures.Texture) is
      function SDL_Set_Render_Target (R : in Renderer_Pointer;
                                      T : in SDL2.Texture_Pointer) return C.int ;
      pragma Import (C, SDL_Set_Render_Target,"SDL_SetRenderTarget");
      Result : C.int := SDL_Set_Render_Target (Self.Pointer, Target.Get_Pointer);
   begin
      if Result /= Success then
         raise SDL_Error with SDL2.Error.Get;
      end if;
   end Set_Target;

   function Get_Renderer (Window : in SDL2.Video.Windows.Window_Type) return Renderer is
     function SDL_Get_Renderer (W : in SDL2.Window_Pointer) return Renderer_Pointer ;
     pragma Import(C, SDL_Get_Renderer, "SDL_GetRenderer");
     Tmp : Renderer_Pointer  := null;
   begin
      Tmp := SDL_Get_Renderer (Window.Get_Pointer);
      if Tmp = null then
        raise SDL_Error with SDL2.Error.Get;
      end if;

      return Result : constant Renderer :=
        (Ada.Finalization.Controlled with
           Pointer => Tmp, --SDL_Get_Renderer (Window.Get_Pointer),
           Owner => False) do
         null;
      end return;
   end Get_Renderer;


   function Get_Pointer (Self : in Renderer) return Renderer_Pointer is
   begin
      return Self.Pointer;
   end Get_Pointer;

   procedure Create
     (Self   : in out Renderer;
      Window : in out SDL2.Video.Windows.Window_Type;
      Driver : in Positive;
      Flags  : in Renderer_Flags := Default_Renderer_Flags)
--            with Post => Self.Pointer /= null

      is

      function SDL_Create_Renderer (W : in SDL2.Window_Pointer; Index : in C.int; Flags : in Renderer_Flags)
                                    return Renderer_Pointer ;
     pragma Import(C, SDL_Create_Renderer, "SDL_CreateRenderer");
   begin
      Self.Pointer := SDL_Create_Renderer (Window.Get_Pointer, C.int (Driver), Flags);
      Self.Owner     := True;
   end Create;

   procedure Create
     (Self   : in out Renderer;
      Window : in out SDL2.Video.Windows.Window_Type;
      Flags  : in Renderer_Flags := Default_Renderer_Flags)
     -- with Post => Self.Pointer /= null
      is

      function SDL_Create_Renderer (W : in SDL2.Window_Pointer; Index : in C.int; Flags : in Renderer_Flags)
                                    return Renderer_Pointer ;
      pragma Import(C, SDL_Create_Renderer, "SDL_CreateRenderer");
   begin
      Self.Pointer := SDL_Create_Renderer (Window.Get_Pointer, -1, Flags);
      Self.Owner     := True;
   end Create;

   procedure Create
     (Self    : in out Renderer;
      Surface : in out SDL2.Video.Surfaces.Surface)
    --  with Post => Self.Pointer /= null
      is
      function SDL_Create_Software_Renderer (S : in SDL2.Surface_Pointer)
                                             return Renderer_Pointer ;
      pragma Import(c, SDL_Create_Software_Renderer, "SDL_CreateSoftwareRenderer");
   begin
      Self.Pointer := SDL_Create_Software_Renderer (Surface.Get_Pointer);
      Self.Owner   := True;
   end Create;

   procedure Create
     (Self    : in out Renderer;
      Pointer : SDL2.Renderer_Pointer;
      Owner   : Boolean) is
   begin
      Self.Pointer := Pointer;
      Self.Owner   := Owner;
   end Create;


end SDL2.Video.Renderers;
