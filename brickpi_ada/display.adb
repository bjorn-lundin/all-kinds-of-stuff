
--with Text_Io;
with Interfaces.C.Strings;
with C_Constants;

package body Display is

  -- procedure Debug (What : String ) is
  -- begin
  --   Text_Io.Put_Line(What);
  -- end Debug;  
  -- procedure Debug2 (What : String ) is
  -- begin
  --   Text_Io.Put(What);
  -- end Debug2;  


   --for files
  type Size_T is new Interfaces.C.Long; --Long_Integer;
  subtype Ssize_T is Size_T;

  procedure Open(Lcd : in out LCD_Type; Path : String) is
    subtype Path_Name_Type is String(Path'first .. Path'last +1);
    LCD_Path : aliased Path_Name_Type := Path & Ascii.Nul;
    function C_Open(Path  : access Path_Name_Type;
                   Flags : Interfaces.C.Int) return File_Id;
    pragma Import(C, C_Open, "open" );
  begin
    Lcd.File_Ptr := C_Open(LCD_Path'access, C_Constants.O_WRONLY);
  end Open;
  -----------------------------------------------------------------


  -----------------------------------------------------------------
  procedure Open(Lcd : in out LCD_Type) is
  begin
    Lcd.Open(Path =>"/dev/ttyACM0");
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
    use Interfaces.C.Strings;
    function C_Write(Fd    : File_Id;
                     Buf   : Chars_Ptr;
                     Count : Size_T ) return Ssize_T;
    pragma import(C, C_Write , "write");
    Write_Result :  Ssize_T := 0;
    pragma Warnings(Off,Write_Result);
    Message_Buffer : Chars_Ptr := New_String(What);
  begin
    Write_Result := C_Write(Fd    => Lcd.File_Ptr,
                            Buf   => Message_Buffer,
                            Count => Size_T(What'Length));
    Interfaces.C.Strings.Free (Message_Buffer);
    delay Delay_After_Write;
  end Write;
  -----------------------------------------------------------------
  procedure Write_Command(Lcd : in out LCD_Type; What : in Byte_Array) is
    subtype This_Byte_Array is Byte_Array(What'first .. What'last);
    function C_Write(Fd    : File_Id;
                     Buf   : access This_Byte_Array;
                     Count : Size_T ) return Ssize_T;
    pragma import(C, C_Write , "write");
    Message_Buffer : aliased This_Byte_Array := What;
    Write_Result :  Ssize_T := 0;
    pragma Warnings(Off,Write_Result);
  begin
    Write_Result := C_Write(
                       Fd    => Lcd.File_Ptr,
                       Buf   => Message_Buffer'access,
                       Count => Size_T(Message_Buffer'Length));
    delay Delay_After_Write;
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

  procedure GotoRC(Lcd : in out LCD_Type; Row : in Row_Type; Col : in Column_Type) is
    subtype Byte_Array_4 is Byte_Array(1..4);
    -- col & row NOT row & col
    Cmd_Goto_And_Args : Byte_Array_4 := Cmd_Goto & Byte(Col) & Byte(Row); 
  begin
   Lcd.Write_Command(What => Cmd_Goto_And_Args);   
  end GotoRC;
  
  ---------------------------------------------------------------
  procedure Display_On(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Display_On);
  end Display_On;
 ---------------------------------------------------------------
  procedure Display_Off(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Display_Off);
  end Display_Off;
  ---------------------------------------------------------------

  procedure Cursor_Back(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Cursor_Back);
  end Cursor_Back;
  ---------------------------------------------------------------
  procedure Cursor_Forward(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Cursor_Forward);
  end Cursor_Forward;
  ---------------------------------------------------------------
  procedure Underline_Cursor_On(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Underline_Cursor_On);
  end Underline_Cursor_On;
  ---------------------------------------------------------------
  procedure Underline_Cursor_Off(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Underline_Cursor_Off);
  end Underline_Cursor_Off;
  ---------------------------------------------------------------
  procedure Block_Cursor_On(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Block_Cursor_On);
  end Block_Cursor_On;
  ---------------------------------------------------------------
  procedure Block_Cursor_Off(Lcd : in out LCD_Type) is
  begin
    Lcd.Write_Command(What => Cmd_Block_Cursor_Off);
  end Block_Cursor_Off;
  ---------------------------------------------------------------

end Display;