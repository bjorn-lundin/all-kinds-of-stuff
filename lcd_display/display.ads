
with Ada.Finalization;
with Interfaces.C;

package Display is
  type Byte is range 0 .. 255 ;
  for Byte'size use 8;
  
  type Byte_Array is array (Natural range  <>) of Byte;

  type Row_Type    is new Byte range 1..2;
  type Column_Type is new Byte range 1..16;

  type LCD_Type is new Ada.Finalization.Controlled with private;
 
  procedure Open(Lcd : in out LCD_Type);
  procedure Open(Lcd : in out LCD_Type; Path : String);
  procedure Close(Lcd : in out LCD_Type) ;
  procedure Write(Lcd : in out LCD_Type; What : in String);
  procedure Write_Command(Lcd : in out LCD_Type; What : in Byte_Array);
  procedure Clear(Lcd : in out LCD_Type);
  procedure Home(Lcd : in out LCD_Type);
  procedure GotoRC(Lcd : in out LCD_Type; Row : in Row_Type; Col : in Column_Type);
  procedure Display_On(Lcd : in out LCD_Type);
  procedure Display_Off(Lcd : in out LCD_Type);
  procedure Cursor_Back(Lcd : in out LCD_Type);
  procedure Cursor_Forward(Lcd : in out LCD_Type);
  procedure Underline_Cursor_On(Lcd : in out LCD_Type);
  procedure Underline_Cursor_Off(Lcd : in out LCD_Type);
  procedure Block_Cursor_On(Lcd : in out LCD_Type);
  procedure Block_Cursor_Off(Lcd : in out LCD_Type);
  
  
private 

   type File_Id is new Interfaces.C.Int;
   type LCD_Type is new Ada.Finalization.Controlled with record
     File_Ptr : File_Id := 0;
   end record;

   Delay_After_Write        : constant Duration := 0.1; 
   Cmd_Clear                : constant Byte_Array := (16#FE#, 16#58#);
   Cmd_Home                 : constant Byte_Array := (16#FE#, 16#48#);
   Cmd_Display_On           : constant Byte_Array := (16#FE#, 16#42#, 16#0#);
   Cmd_Display_Off          : constant Byte_Array := (16#FE#, 16#46#);
   Cmd_Goto                 : constant Byte_Array := (16#FE#, 16#47#);
   Cmd_Cursor_Back          : constant Byte_Array := (16#FE#, 16#4C#);
   Cmd_Cursor_Forward       : constant Byte_Array := (16#FE#, 16#4D#);
   Cmd_Underline_Cursor_On  : constant Byte_Array := (16#FE#, 16#4A#);
   Cmd_Underline_Cursor_Off : constant Byte_Array := (16#FE#, 16#4B#);
   Cmd_Block_Cursor_On      : constant Byte_Array := (16#FE#, 16#53#);
   Cmd_Block_Cursor_Off     : constant Byte_Array := (16#FE#, 16#54#);
    
end Display;