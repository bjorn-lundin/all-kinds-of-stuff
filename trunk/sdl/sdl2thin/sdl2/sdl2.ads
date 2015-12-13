
with Interfaces.C;          use Interfaces.C;

package SDL2 is

  type Init_Flags is mod 2 ** 32;
  pragma Convention(C, Init_Flags) ;    
  
 
  Enable_Timer           : constant Init_Flags := 16#0000_0001#;
  Enable_Audio           : constant Init_Flags := 16#0000_0010#;
  Enable_Screen          : constant Init_Flags := 16#0000_0020#;
  Enable_Joystick        : constant Init_Flags := 16#0000_0200#;
  Enable_Haptic          : constant Init_Flags := 16#0000_1000#;
  Enable_Game_Controller : constant Init_Flags := 16#0000_2000#;
  Enable_Events          : constant Init_Flags := 16#0000_4000#;
  Enable_No_Parachute    : constant Init_Flags := 16#0010_0000#;
  Enable_Everything      : constant Init_Flags :=
     Enable_Timer or Enable_Audio or Enable_Screen or Enable_Joystick or Enable_Haptic or
     Enable_Game_Controller or Enable_Events or Enable_No_Parachute;  
  -- sdl

  
  -- types in c
  type Font_Pointer     is access all Int;
  type Surface_Pointer  is access all Int;
  type Texture_Pointer  is access all Int;
  type Renderer_Pointer is access all Int;
  type Window_Pointer   is access all Int;
  


end SDL2;