
with Ada.Finalization;
with Interfaces.C;

package Display is
  type Byte is range 0 .. 255 ;
  for Byte'size use 8;
  
  type Byte_2_Array is array (1..2) of Byte;

--  type LCD_Type is private;
   type File_Id is new Interfaces.C.Int;
   type LCD_Type is new Ada.Finalization.Controlled with record
     File_Ptr : File_Id := 0;
   end record;
 
  procedure Open(Lcd : in out LCD_Type);
  procedure Open(Lcd : in out LCD_Type; Path : String);
  procedure Close(Lcd : in out LCD_Type) ;
  procedure Write(Lcd : in out LCD_Type; What : in String);
  procedure Write_Command(Lcd : in out LCD_Type; What : in Byte_2_Array);
  procedure Clear(Lcd : in out LCD_Type);
  procedure Home(Lcd : in out LCD_Type);
 -- procedure Display_On(Lcd : in out LCD_Type);
 -- procedure Display_Off(Lcd : in out LCD_Type);
  
  
private 
--
--  type File_Id is new Interfaces.C.Int;
--  type LCD_Type is new Ada.Finalization.Controlled with record
--    File_Ptr : File_Id := 0;
--  end record;
    Delay_After_Write : constant Duration := 0.05; 
    Cmd_Clear         : constant Byte_2_Array := (16#FE#, 16#58#);
    Cmd_Home          : constant Byte_2_Array := (16#FE#, 16#48#);
    Cmd_Display_On    : constant Byte_2_Array := (16#FE#, 16#42#);
    Cmd_Display_Off   : constant Byte_2_Array := (16#FE#, 16#46#);
    
end Display;