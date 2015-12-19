
with Ada.Finalization;
with SDL2.Video.Rectangles;

package SDL2.Video.Windows is

  SDL_Windows_Error : exception ;


  type Window_Flags is mod 2 ** 32 ;     
  pragma Convention(C, Window_Flags) ;    
  
   Windowed            : constant Window_Flags := 16#0000_0000#;
   Full_Screen         : constant Window_Flags := 16#0000_0001#;
   OpenGL              : constant Window_Flags := 16#0000_0002#;
   Shown               : constant Window_Flags := 16#0000_0004#;
   Hidden              : constant Window_Flags := 16#0000_0008#;
   Borderless          : constant Window_Flags := 16#0000_0010#;
   Resizable           : constant Window_Flags := 16#0000_0020#;
   Minimised           : constant Window_Flags := 16#0000_0040#;
   Maximised           : constant Window_Flags := 16#0000_0080#;
   Input_Grabbed       : constant Window_Flags := 16#0000_0100#;
   Input_Focus         : constant Window_Flags := 16#0000_0200#;
   Mouse_Focus         : constant Window_Flags := 16#0000_0400#;
   Full_Screen_Desktop : constant Window_Flags := Full_Screen or 16#0000_1000#;
  
  

  type Window_Type is new Ada.Finalization.Controlled with record
    Pointer : SDL2.Window_Pointer := null;
    Owner   : Boolean := True;
  end record;

  function Create (Title : String; X,Y,W,H : Int; Flags : Window_Flags := 0)
        return Window_Type;
        

  function Create (Title : String; X,Y: Int; S: SDL2.Video.Rectangles.Size ; Flags : Window_Flags := 0)
        return Window_Type;  
        
  function Create (Title : String; R: SDL2.Video.Rectangles.Rectangle ; Flags : Window_Flags := 0)
        return Window_Type;  


        
  function Get_Pointer (Self : Window_Type) return SDL2.Window_Pointer;
  
  
  type ID is mod 2 ** 32 ;
  pragma Convention (C, Id);

  type Positions is record
     X : Positive;
     Y : Positive;
  end record;

  type Sizes is record
     Width  : Positive;
     Height : Positive;
  end record;
  
  
end SDL2.Video.Windows;