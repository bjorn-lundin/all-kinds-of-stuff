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
with Ada.Unchecked_Conversion;
with Interfaces.C;
with Interfaces.C.Strings;
with SDL2.Error;
with SDL2.Video.Surfaces;
with SDL2.Log;

package body SDL2.Video.Windows is
   package C renames Interfaces.C;

   use type C.int;
   use type System.Address;

   procedure Increment_Windows is
   begin
      Total_Windows_Created := Total_Windows_Created + 1;
   end Increment_Windows;

   procedure Decrement_Windows is
   begin
      Total_Windows_Created := Total_Windows_Created + 1;
   end Decrement_Windows;

   overriding
   procedure Finalize (Self : in out Window) is
      procedure SDL_Destroy (W : in Window_Pointer);
      pragma Import(C, SDL_Destroy, "SDL_DestroyWindow");
   begin
      --  SDL2.Log.Put_Debug ("Windows.Finalize: " & (if Self.Internal = null then "null" else "not null") &
      --                       "    " & (if Self.Owns = True then "owns" else "Doesn't own"));
      --  Make sure we don't delete this twice!
      if Self.Internal /= null and then Self.Owns then
         SDL2.Log.Put_Debug ("Windows.Finalize: Deleting");
         SDL_Destroy (Self.Internal);
         Self.Internal := null;
         Decrement_Windows;
      end if;
   end Finalize;

   function Get_Brightness (Self : in Window) return Brightness is
      function SDL_Get_Brightness (W : in Window_Pointer) return C.C_float ;
      pragma Import(C, SDL_Get_Brightness, "SDL_GetWindowBrightness");
   begin
      return Brightness (SDL_Get_Brightness (Self.Internal));
   end Get_Brightness;

   procedure Set_Brightness (Self : in out Window; How_Bright : in Brightness) is
      function SDL_Set_Brightness (W : in Window_Pointer; B : in C.C_float) return C.int;
      pragma Import(C, SDL_Set_Brightness, "SDL_SetWindowBrightness");
      Result : C.int := SDL_Set_Brightness (Self.Internal, C.C_float (How_Bright));
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Set_Brightness;

   --  TODO: Try to see if we can just see User_Data_Access as the return type from the C function.
   function To_Data_Access is new Ada.Unchecked_Conversion (Source => System.Address, Target => User_Data_Access);
   function To_Address is new Ada.Unchecked_Conversion (Source => User_Data_Access, Target => System.Address);

   --  TODO: Make this and Set_Data generic.
   function Get_Data (Self : in Window; Name : in String) return User_Data_Access is
      function SDL_Get_Window_Data (W    : in Window_Pointer;
                                    Name : in C.Strings.chars_ptr) return System.Address ;
      pragma Import(C, SDL_Get_Window_Data, "SDL_GetWindowData");
      C_Name_Str : C.Strings.chars_ptr := C.Strings.New_String (Name);
      Item       : User_Data_Access    := To_Data_Access (SDL_Get_Window_Data (Self.Internal, C_Name_Str));
   begin
      C.Strings.Free (C_Name_Str);
      return Item;
   end Get_Data;

   function Set_Data (Self : in out Window; Name : in String; Item : in User_Data_Access) return User_Data_Access is
      function SDL_Set_Window_Data (W         : in Window_Pointer;
                                    Name      : in C.Strings.chars_ptr;
                                    User_Data : in System.Address) return System.Address ;
      pragma Import(C, SDL_Set_Window_Data, "SDL_SetWindowData");
      C_Name_Str    : C.Strings.chars_ptr := C.Strings.New_String (Name);
      Previous_Data : User_Data_Access    := To_Data_Access (SDL_Set_Window_Data (Self.Internal,
                                                             C_Name_Str,
                                                             To_Address (Item)));
   begin
      C.Strings.Free (C_Name_Str);
      return Previous_Data;
   end Set_Data;

   function Display_Index (Self : in Window) return Positive is
      function SDL_Get_Window_Display_Index (W : in Window_Pointer) return C.int ;
      pragma Import(C, SDL_Get_Window_Display_Index, "SDL_GetWindowDisplayIndex");
      Total : C.int := SDL_Get_Window_Display_Index (Self.Internal);
   begin
      if Total < 0 then
         raise Window_Error with SDL2.Error.Get;
      end if;

      return Positive (Total);
   end Display_Index;

   procedure Get_Display_Mode (Self : in Window; Mode : out SDL2.Video.Displays.Mode) is
      function SDL_Get_Window_Display_Mode (W : in Window_Pointer;
                                            M : out SDL2.Video.Displays.Mode) return C.int ;
      pragma Import(C, SDL_Get_Window_Display_Mode, "SDL_GetWindowDisplayMode");
      Result : C.int := SDL_Get_Window_Display_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Get_Display_Mode;

   procedure Set_Display_Mode (Self : in out Window; Mode : in SDL2.Video.Displays.Mode) is
      function SDL_Set_Window_Display_Mode (W : in Window_Pointer;
                                            M : in SDL2.Video.Displays.Mode) return C.int ;
      pragma Import(C, SDL_Set_Window_Display_Mode, "SDL_SetWindowDisplayMode");
      Result : C.int := SDL_Set_Window_Display_Mode (Self.Internal, Mode);
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Set_Display_Mode;

   function Get_Flags (Self : in Window) return Window_Flags is
      function SDL_Set_Window_Display_Mode (W : in Window_Pointer) return Window_Flags ;
      pragma Import(C, SDL_Set_Window_Display_Mode, "SDL_GetWindowFlags");
   begin
      return SDL_Get_Window_Flags (Self.Internal);
   end Get_Flags;

   function From_ID (Window_ID : in ID) return Window is
      function SDL_Get_Window_From_ID (W : in ID) return Window_Pointer ;
      pragma Import(C, SDL_Get_Window_From_ID, "SDL_GetWindowFromID");
   begin
      return W : constant Window :=
        (Ada.Finalization.Limited_Controlled with Internal => SDL_Get_Window_From_ID (Window_ID), Owns => False)
      do
         null;
      end return;
   end From_ID;

   procedure Get_Gamma_Ramp (Self : in Window; Red, Green, Blue : out SDL2.Video.Pixel_Formats.Gamma_Ramp) is
      function SDL_Get_Window_Gamma_Ramp (W       : in Window_Pointer;
                                          R, G, B : out SDL2.Video.Pixel_Formats.Gamma_Ramp) return C.int ;
      pragma Import(C, SDL_Get_Window_Gamma_Ramp, "SDL_GetWindowGammaRamp");
      Result : C.int := SDL_Get_Window_Gamma_Ramp (Self.Internal, Red, Green, Blue);
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Get_Gamma_Ramp;

   procedure Set_Gamma_Ramp (Self : in out Window; Red, Green, Blue : in SDL2.Video.Pixel_Formats.Gamma_Ramp) is
      function SDL_Set_Window_Gamma_Ramp (W       : in Window_Pointer;
                                          R, G, B : in SDL2.Video.Pixel_Formats.Gamma_Ramp) return C.int ;
      pragma Import(C, SDL_Set_Window_Gamma_Ramp, "SDL_SetWindowGammaRamp");
      Result : C.int := SDL_Set_Window_Gamma_Ramp (Self.Internal, Red, Green, Blue);
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Set_Gamma_Ramp;

   function Is_Grabbed (Self : in Window) return Boolean is
      function SDL_Get_Window_Grab (W : in Window_Pointer) return SDL_Bool ;
      pragma Import(C, SDL_Get_Window_Grab, "SDL_GetWindowGrab");
   begin
      return (SDL_Get_Window_Grab (Self.Internal) = SDL_True);
   end Is_Grabbed;

   procedure Set_Grabbed (Self : in out Window; Grabbed : in Boolean := True) is
      procedure SDL_Set_Window_Grab (W : in Window_Pointer; G : in SDL_Bool) ;
      pragma Import(C, SDL_Set_Window_Grab, "SDL_SetWindowGrab");
   begin
      SDL_Set_Window_Grab (Self.Internal, (if Grabbed = True then SDL_True else SDL_False));
   end Set_Grabbed;

   function Get_ID (Self : in Window) return ID is
      function SDL_Get_Window_ID (W : in Window_Pointer) return ID ;
      pragma Import(C, SDL_Get_Window_ID, "SDL_GetWindowID");
   begin
      return SDL_Get_Window_ID (Self.Internal);
   end Get_ID;

   function Get_Maximum_Size (Self : in Window) return Sizes is
      procedure SDL_Get_Window_Maximum_Size (Win : in Window_Pointer; W, H : out C.int) ;
      pragma Import(C, SDL_Get_Window_Maximum_Size, "SDL_GetWindowMaximumSize");
      W, H : C.int := 0;
   begin
      SDL_Get_Window_Maximum_Size (Self.Internal, W, H);
      return Sizes'(Width => Positive (W), Height => Positive (H));
   end Get_Maximum_Size;

   procedure Set_Maximum_Size (Self : in out Window; Size : in Sizes) is
      procedure SDL_Set_Window_Maximum_Size (Win : in Window_Pointer; W, H : in C.int) ;
      pragma Import(C, SDL_Set_Window_Maximum_Size, "SDL_SetWindowMaximumSize");
   begin
      SDL_Set_Window_Maximum_Size (Self.Internal, C.int (Size.Width), C.int (Size.Height));
   end Set_Maximum_Size;

   function Get_Minimum_Size (Self : in Window) return Sizes is
      procedure SDL_Get_Window_Minimum_Size (Win : in Window_Pointer; W, H : out C.int) ;
      pragma Import(C, SDL_Get_Window_Minimum_Size, "SDL_GetWindowMinimumSize");
      W, H : C.int := 0;
   begin
      SDL_Get_Window_Minimum_Size (Self.Internal, W, H);

      return Sizes'(Width => Positive (W), Height => Positive (H));
   end Get_Minimum_Size;

   procedure Set_Minimum_Size (Self : in out Window; Size : in Sizes) is
      procedure SDL_Set_Window_Minimum_Size (Win : in Window_Pointer; W, H : in C.int) ;
      pragma Import(C, SDL_Set_Window_Minimum_Size, "SDL_SetWindowMinimumSize");
   begin
      SDL_Set_Window_Minimum_Size (Self.Internal, C.int (Size.Width), C.int (Size.Height));
   end Set_Minimum_Size;

   function Pixel_Format (Self : in Window) return SDL2.Video.Pixel_Formats.Pixel_Format is
      function SDL_Get_Window_Pixel_Format (W : in Window_Pointer)
                                            return SDL2.Video.Pixel_Formats.Pixel_Format ;
      pragma Import(C, SDL_Get_Window_Pixel_Format, "SDL_GetWindowPixelFormat");
   begin
      return SDL_Get_Window_Pixel_Format (Self.Internal);
   end Pixel_Format;

   function Get_Position (Self : in Window) return Positions is
      procedure SDL_Get_Window_Position (W : in Window_Pointer; X, Y : out C.int) ;
      pragma Import(C, SDL_Get_Window_Position, "SDL_GetWindowPosition");
      X, Y : C.int := 0;
   begin
      SDL_Get_Window_Position (Self.Internal, X, Y);
      return Positions'(Positive (X), Positive (Y));
   end Get_Position;

   procedure Set_Position (Self : in out Window; Position : Positions) is
      procedure SDL_Set_Window_Position (W : in Window_Pointer; X, Y : in C.int) ;
      pragma Import(C, SDL_Set_Window_Position, "SDL_SetWindowPosition");
   begin
      SDL_Set_Window_Position (Self.Internal, C.int (Position.X), C.int (Position.Y));
   end Set_Position;

   function Get_Size (Self : in Window) return Sizes is
      procedure SDL_Get_Window_Size (Win : in Window_Pointer; W, H : out C.int) ;
      pragma Import(C, SDL_Get_Window_Size, "SDL_GetWindowSize");
      W, H : C.int := 0;
   begin
      SDL_Get_Window_Size (Self.Internal, W, H);
      return Sizes'(Width => Positive (W), Height => Positive (H));
   end Get_Size;

   procedure Set_Size (Self : in out Window; Size : in Sizes) is
      procedure SDL_Set_Window_Size (Win : in Window_Pointer; W, H : in C.int) ;
      pragma Import(C, SDL_Set_Window_Size, "SDL_SetWindowSize");
   begin
      SDL_Set_Window_Size (Self.Internal, C.int (Size.Width), C.int (Size.Height));
   end Set_Size;

   function Get_Surface_Pointer (Self : in Window) return SDL2.Video.Surfaces.Surface_Pointer is
      function SDL_Get_Window_Surface (W : in Window_Pointer) return  SDL2.Video.Surfaces.Surface_Pointer ;
      pragma Import(C, SDL_Get_Window_Surface, "SDL_GetWindowSurface");
      A : SDL2.Video.Surfaces.Surface_Pointer:= SDL_Get_Window_Surface (Self.Internal);
   begin
      if A = null then
         raise Window_Error with SDL2.Error.Get;
      end if;      
      return A;
   end Get_Surface_Pointer;

   function Get_Title (Self : in Window) return Ada.Strings.UTF_Encoding.UTF_8_String is
      function SDL_Get_Window_Title (W : in Window_Pointer) return C.Strings.Chars_Ptr ;
      pragma Import(C, SDL_Get_Window_Title, "SDL_GetWindowTitle");
   begin
      return C.Strings.Value (SDL_Get_Window_Title (Self.Internal));
   end Get_Title;

   procedure Set_Title (Self : in Window; Title : in Ada.Strings.UTF_Encoding.UTF_8_String) is
      procedure SDL_Set_Window_Title (W : in Window_Pointer; C_Str : in C.Char_Array) ;
      pragma Import(C, SDL_Set_Window_Title, "SDL_SetWindowTitle");
   begin
      SDL_Set_Window_Title (Self.Internal, C.To_C (Title));
   end Set_Title;

   procedure Hide (Self : in Window) is
      procedure SDL_Hide_Window (W : in Window_Pointer) ;
      pragma Import(C, SDL_Hide_Window, "SDL_HideWindow");
   begin
      SDL_Hide_Window (Self.Internal);
   end Hide;

   procedure Show (Self : in Window) is
      procedure SDL_Show_Window (W : in Window_Pointer) ;
      pragma Import(C, SDL_Show_Window, "SDL_ShowWindow");
   begin
      SDL_Show_Window (Self.Internal);
   end Show;

   procedure Maximise (Self : in Window) is
      procedure SDL_Maximise_Window (W : in Window_Pointer) ;
      pragma Import(C, SDL_Maximise_Window, "SDL_MaximizeWindow");
   begin
      SDL_Maximise_Window (Self.Internal);
   end Maximise;

   procedure Minimise (Self : in Window) is
      procedure SDL_Minimise_Window (W : in Window_Pointer) ;
      pragma Import(C, SDL_Minimise_Window, "SDL_MinimizeWindow");
   begin
      SDL_Minimise_Window (Self.Internal);
   end Minimise;

   procedure Raise_And_Focus (Self : in Window) is
      procedure SDL_Raise_Window (W : in Window_Pointer) ;
      pragma Import(C, SDL_Raise_Window, "SDL_RaiseWindow");
   begin
      SDL_Raise_Window (Self.Internal);
   end Raise_And_Focus;

   procedure Restore (Self : in Window) is
      procedure SDL_Restore_Window (W : in Window_Pointer) ;
      pragma Import(C, SDL_Restore_Window, "SDL_RestoreWindow");
   begin
      SDL_Restore_Window (Self.Internal);
   end Restore;

   procedure Set_Mode (Self : in out Window; Flags : in Full_Screen_Flags) is
      function SDL_Window_Full_Screen (W : in Window_Pointer;
                                       F : in Full_Screen_Flags) return C.int ;
      pragma Import(C, SDL_Window_Full_Screen, "SDL_SetWindowFullscreen");
      Result : C.int := SDL_Window_Full_Screen (Self.Internal, Flags);
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Set_Mode;

   procedure Set_Icon (Self : in out Window; Icon : in SDL2.Video.Surfaces.Surface) is
      procedure SDL_Set_Window_Icon (W : in Window_Pointer; I : SDL2.Video.Surfaces.Surface_Pointer) ;
      pragma Import(C, SDL_Set_Window_Icon, "SDL_SetWindowIcon");
      
   begin
      SDL_Set_Window_Icon (Self.Internal, Icon.Get_Internal);
   end Set_Icon;

   procedure Update_Surface (Self : in Window) is
      function SDL_Update_Window_Surface (W : in Window_Pointer) return C.int ;
      pragma Import(C, SDL_Update_Window_Surface, "SDL_UpdateWindowSurface");
      Result : C.int := SDL_Update_Window_Surface (Self.Internal);
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Update_Surface;

   procedure Update_Surface_Rectangles (Self : in Window; Rectangles : SDL2.Video.Rectangles.Rectangle_Arrays) is
      function SDL_Update_Window_Surface_Rects (W : in Window_Pointer;
                                                R : in SDL2.Video.Rectangles.Rectangle_Arrays;
                                                L : in C.int) return C.int ;                                                
      pragma Import(C, SDL_Update_Window_Surface_Rects, "SDL_UpdateWindowSurfaceRects");
      Result : C.int := SDL_Update_Window_Surface_Rects (Self.Internal, Rectangles, Rectangles'Length);
   begin
      if Result /= Success then
         raise Window_Error with SDL2.Error.Get;
      end if;
   end Update_Surface_Rectangles;

   function Exist return Boolean is
   begin
      if Total_Windows_Created /= Natural'First then
         return True;
      end if;
      return False;
   end Exist;
   
   function Get_Internal (Self : in Window) return Window_Pointer is
   begin
      return Self.Internal;
   end Get_Internal_Window;
   
   procedure Create
     (Win    : in out Window;
      Title  : in Ada.Strings.UTF_Encoding.UTF_8_String;
      X      : in Integer;
      Y      : in Integer;
      Width  : in Integer;
      Height : in Integer;
      Flags  : in Window_Flags := OpenGL) is

      function SDL_Create
        (Title      : C.Strings.Chars_Ptr;
         X, Y, W, H : in C.int;
         F          : in Window_Flags) return Window_Pointer ;
      pragma Import(C, SDL_Create, "SDL_CreateWindow");
      C_Title_Str : C.Strings.Chars_Ptr := C.Strings.New_String (Title);
   begin
      Win.Internal := SDL_Create (C_Title_Str, C.Int (X), C.Int (Y), C.Int (Width), C.Int (Height), Flags);
      C.Strings.Free (C_Title_Str);
      if Win.Internal = null then
         raise Window_Error with SDL2.Error.Get;
      end if;
      Increment_Windows;
   end Create;

   procedure Create (Win : in out Window; Native : in Native_Window) is
      function SDL_Create_Window_From (Native : Native_Window) return Window_Pointer with
      pragma Import(C, SDL_Create_Window_From, "SDL_CreateWindowFrom");
   begin
      Win.Internal := SDL_Create_Window_From (Native);
      Win.Owns     := True;
      if Win.Internal = null then
         raise Window_Error with SDL2.Error.Get;
      end if;
      Increment_Windows;
   end Create;
   
   
end SDL2.Video.Windows;
