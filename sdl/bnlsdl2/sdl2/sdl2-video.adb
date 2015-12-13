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
with Interfaces.C.Strings;
with Ada.Text_IO; use Ada.Text_IO;
with SDL2.Error;

package body SDL2.Video is
   package C renames Interfaces.C;

   use type C.Int;

   function Is_Screen_Saver_Enabled return Boolean is
      function SDL_Is_Screen_Saver_Enabled return C.Int;
      pragma Import (C, SDL_Is_Screen_Saver_Enabled, "SDL_IsScreenSaverEnabled");
   begin
      return (if SDL_Is_Screen_Saver_Enabled = 1 then True else False);
   end Is_Screen_Saver_Enabled;

   function Initialise (Name : in String) return Boolean is
   
      function SDL_Video_Init (C_Name : in C.Strings.Chars_Ptr) return C.Int;
      pragma Import (C, SDL_Video_Init, "SDL_VideoInit");

      C_Str  : C.Strings.Chars_Ptr := C.Strings.Null_Ptr;
      Result : C.Int;
   begin
      if Name /= "" then
         C_Str := C.Strings.New_String (Name);
         Result := SDL_Video_Init (C_Name => C_Str);
         C.Strings.Free (C_Str);
      else
         Result := SDL_Video_Init (C_Name => C.Strings.Null_Ptr);
      end if;
      return (Result = Success);
   end Initialise;

   function Total_Drivers return Positive is
      function SDL_Get_Num_Video_Drivers return C.Int ;
      pragma Import (C, SDL_Get_Num_Video_Drivers, "SDL_GetNumVideoDrivers");
      Num : constant C.Int := SDL_Get_Num_Video_Drivers;
   begin
      if Num < 0 then
         raise Video_Error with SDL2.Error.Get;
      end if;
      return Positive (Num);
   end Total_Drivers;

   function Driver_Name (Index : in Positive) return String is
      function SDL_Get_Video_Driver (I : in C.Int) return C.Strings.Chars_Ptr;
      pragma Import (C, SDL_Get_Video_Driver, "SDL_GetVideoDriver");
      --  Index is zero based, so need to subtract 1 to correct it.
      C_Str : C.Strings.Chars_Ptr := SDL_Get_Video_Driver (C.Int (Index) - 1);
   begin
      return C.Strings.Value (C_Str);
   end Driver_Name;

   function Current_Driver_Name return String is
      function SDL_Get_Current_Video_Driver return C.Strings.Chars_Ptr ;
      pragma Import (C, SDL_Get_Current_Video_Driver, "SDL_GetCurrentVideoDriver");
      C_Str : constant C.Strings.Chars_Ptr := SDL_Get_Current_Video_Driver;
      use type C.Strings.Chars_Ptr;
   begin
      if C_Str = C.Strings.Null_Ptr then
         raise Video_Error with SDL2.Error.Get;
      end if;
      return C.Strings.Value (C_Str);
   end Current_Driver_Name;

   function Total_Displays return Positive is
      function SDL_Get_Num_Video_Displays return C.Int ;
      pragma Import (C, SDL_Get_Num_Video_Displays, "SDL_GetNumVideoDisplays");
      Num : constant C.Int := SDL_Get_Num_Video_Displays;
   begin
      if Num <= 0 then
         raise Video_Error with SDL2.Error.Get;
      end if;
      return Positive (Num);
   end Total_Displays;
end SDL2.Video;
