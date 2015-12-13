
--with Ada.Finalization;
with Interfaces.C.Strings; use Interfaces.C.Strings;
with SDL2.Error;
with SDL2.Thin;
package body SDL2.Video.Windows is


  function Create (Title : String; X,Y,W,H : Int; Flags : Window_Flags := 0)
        return Window_Type is
      C_Title_Str : Chars_Ptr := New_String (Title);
      Window_Ptr  : SDL2.Window_Pointer := null;
  begin    
    Window_Ptr := SDL2.Thin.SDL_Create (C_Title_Str,X,Y, W,H, Flags);
    Free (C_Title_Str);
    if Window_Ptr = null then
       raise SDL_Windows_Error with SDL2.Error.Get;
    end if; 
    return W : Window_Type := (Ada.Finalization.Controlled with 
       Pointer => Window_Ptr, 
       Owner   => True) do
       null;
    end return;
  end Create;        
  
  function Get_Pointer (Self : Window_Type) return SDL2.Window_Pointer is
  begin
    return Self.Pointer;
  end Get_Pointer;
  

end SDL2.Video.Windows;