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
with Interfaces.C.Strings; use Interfaces.C.Strings;

package body SDL2.Log is
   package C renames Interfaces.C;

   procedure Put (Message : in String) is
      procedure SDL_Log (Message : in C.char_array) ;
      pragma Import(C, SDL_Log, "SDL_Log");
   begin
      SDL_Log (C.To_C (Message));
   end Put;

   procedure Put (Message  : in String; Category : in Categories; Priority : in Priorities) is
      procedure SDL_Log_Message
        (Category : in Categories;
         Priority : in Priorities;
         Message  : in C.char_array) ;
      pragma Import(C, SDL_Log_Message, "SDL_LogMessage");
   begin
      SDL_Log_Message (Category, Priority, C.To_C (Message));
   end Put;

   procedure Put_Critical (Message : in String; Category : in Categories := Application) is
      procedure SDL_Log_Critical (Category : in Categories; Message  : in Chars_Ptr) ;
      pragma Import(C, SDL_Log_Critical, "SDL_LogCritical");
      C_Message : Chars_Ptr := New_String(Message);
   begin
      SDL_Log_Critical (Category, C_Message);
      Free(C_Message);
   end Put_Critical;

   procedure Put_Debug (Message : in String; Category : in Categories := Application) is
      procedure SDL_Log_Debug (Category : in Categories; Message  : in Chars_Ptr) ;
      pragma Import(C, SDL_Log_Debug, "SDL_LogDebug");
      C_Message : Chars_Ptr := New_String(Message);
   begin
      SDL_Log_Debug (Category, C_Message);
      Free(C_Message);
   end Put_Debug;

   procedure Put_Error (Message : in String; Category : in Categories := Application) is
      procedure SDL_Log_Error (Category : in Categories; Message  : in C.char_array) ;
      pragma Import(C, SDL_Log_Error, "SDL_LogError");
   begin
      SDL_Log_Error (Category, C.To_C (Message));
   end Put_Error;

   procedure Put_Info (Message : in String; Category : in Categories := Application) is
      procedure SDL_Log_Info (Category : in Categories; Message  : in C.char_array) ;
      pragma Import(C, SDL_Log_Info, "SDL_LogInfo");
   begin
      SDL_Log_Info (Category, C.To_C (Message));
   end Put_Info;

   procedure Put_Verbose (Message : in String; Category : in Categories := Application) is
      procedure SDL_Log_Verbose (Category : in Categories; Message  : in C.char_array) ;
      pragma Import(C, SDL_Log_Verbose, "SDL_LogVerbose");
   begin
      SDL_Log_Verbose (Category, C.To_C (Message));
   end Put_Verbose;

   procedure Put_Warn (Message : in String; Category : in Categories := Application) is
      procedure SDL_Log_Warn (Category : in Categories; Message  : in C.char_array) ;
      pragma Import(C, SDL_Log_Warn, "SDL_LogWarn");
   begin
      SDL_Log_Warn (Category, C.To_C (Message));
   end Put_Warn;

   procedure Reset_Priorities is
      procedure SDL_Log_Reset_Priorities ;
      pragma Import(C, SDL_Log_Reset_Priorities, "SDL_LogResetPriorities");
   begin
      SDL_Log_Reset_Priorities;
   end Reset_Priorities;

   procedure Set (Priority : in Priorities) is
      procedure SDL_Log_Set_All_Priority (Priority : in Priorities) ;
      pragma Import(C, SDL_Log_Set_All_Priority, "SDL_LogSetAllPriority");
   begin
      SDL_Log_Set_All_Priority (Priority);
   end Set;

   procedure Set (Category : in Categories; Priority : in Priorities) is
      procedure SDL_Set_Priority (Category : in Categories; Priority : in Priorities) ;
      pragma Import(C, SDL_Set_Priority, "SDL_LogSetPriority");
   begin
      SDL_Set_Priority (Category, Priority);
   end Set;

   --  Logging.
   --  A local record type which gets initialised with an access to callback
   --  and a copy to the actual user data.
   type Local_User_Data is
      record
         Callback : Output_Callback;
         Data     : Root_User_Data;
      end record ;
    pragma Convention(C,Local_User_Data);

   procedure Local_Callback
     (User_Data : in Local_User_Data;
      Category  : in Categories;
      Priority  : in Priorities;
      Message   : in C.Strings.chars_ptr) ;
    pragma Convention(C,Local_Callback);

   procedure Local_Callback
     (User_Data : in Local_User_Data;
      Category  : in Categories;
      Priority  : in Priorities;
      Message   : in C.Strings.chars_ptr) is
   begin
      --  Call the Ada callback now.
      User_Data.Callback
        (User_Data.Data,
         Category,
         Priority,
         C.Strings.Value (Message));
   end Local_Callback;
end SDL2.Log;
