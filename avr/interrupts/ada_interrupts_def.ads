

with AVR;                          use AVR;
--  address and bit name constants for the MCU
with AVR.MCU;
with AVR.Timer0;
package Ada_Interrupts_Def is

  package Timer_A renames AVR.Timer0;
   ---------------------------------

   ---------------------------------
   procedure Pin_Change_Fired;
   pragma Machine_Attribute (Entity         => Pin_Change_Fired,
                             Attribute_Name => "signal");
   pragma Export (Convention    => C,
                  Entity        => Pin_Change_Fired,
                  External_Name =>  AVR.MCU.Sig_INT0_String);
   ---------------------------------
   procedure Timer_A_Tick;
   pragma Machine_Attribute (Entity         => Timer_A_Tick,
                             Attribute_Name => "signal");
   pragma Export (Convention    => C,
                  Entity        => Timer_A_Tick,
                  External_Name => Timer_A.Signal_Compare);
   ---------------------------------
   procedure Init ;
   procedure Finish ;
   
end Ada_Interrupts_Def;
