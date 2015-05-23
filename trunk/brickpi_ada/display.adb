
with Brickpi.Constants;

package body Display is
 
   --for files
  type Size_T is new Interfaces.C.Long; --Long_Integer;
  subtype Ssize_T is Size_T;
 
  -----------------------------------------------------------------  
  procedure Open(Lcd : in out LCD_Type) is
    subtype Path_Name_Type is String(1..13);
    LCD_Path : aliased Path_Name_Type := "/dev/ttyACM0" & Ascii.Nul;
    function C_Open(Path  : access Path_Name_Type;
                   Flags : Interfaces.C.Int) return File_Id;
    pragma Import(C, C_Open, "open" );
  begin
    Lcd.File_Ptr := C_Open( LCD_Path'access, Brickpi.Constants.O_WRONLY);
  end Open;
  -----------------------------------------------------------------  
  procedure Close(Lcd : in out LCD_Type) is
    Close_Result : File_Id;
    pragma Warnings(Off,Close_Result);
    function C_Close( File : File_Id ) return File_Id;
    pragma import( C, C_Close ,"close" );
  begin
    Close_Result := C_Close(Lcd.File_Ptr);  
  end Close; 
  -----------------------------------------------------------------  
  procedure Write(Lcd : in out LCD_Type; What : in String) is
    subtype Text_Type is String(What'first .. What'last);
    function C_Write(Fd    : File_Id;
                     Buf   : access Text_Type;
                     Count : Size_T ) return Ssize_T;
    pragma import(C, C_Write , "write");
    Message_Buffer : aliased Text_Type := What;
    Write_Result :  Ssize_T := 0;
    pragma Warnings(Off,Write_Result);
  begin
    Write_Result := C_Write(
                       Fd    => Lcd.File_Ptr, 
                       Buf   => Message_Buffer'access,
                       Count => Size_T(Message_Buffer'Length)); 
  end Write;  
  -----------------------------------------------------------------  

  procedure Write_Command(Lcd : in out LCD_Type; What : in Byte_2_Array) is
    function C_Write(Fd    : File_Id;
                     Buf   : access Byte_2_Array;
                     Count : Size_T ) return Ssize_T;
    pragma import(C, C_Write , "write");
    Message_Buffer : aliased Byte_2_Array := What;
    Write_Result :  Ssize_T := 0;
    pragma Warnings(Off,Write_Result);
  begin
    Write_Result := C_Write(
                       Fd    => Lcd.File_Ptr, 
                       Buf   => Message_Buffer'access,
                       Count => Size_T(Message_Buffer'Length)); 
  end Write_Command;  
  -----------------------------------------------------------------  
  procedure Clear(Lcd : in out LCD_Type) is 
  begin
    Lcd.Write_Command(What => Cmd_Clear); 
  end Clear;
  -----------------------------------------------------------------  
  procedure Home(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Home); 
  end Home;
  -----------------------------------------------------------------  

  procedure Display_On(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Display_On); 
  end Display_On;
  
  procedure Display_Off(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Display_Off); 
  end Display_Off;
  
  
end Display;