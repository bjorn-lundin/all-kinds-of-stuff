with AVR;                          use AVR;
with AVR.MCU;

private package LCD.Wiring is
   pragma Preelaborate;

   type Bus_Mode is (Mode_4bit, Mode_8bit);

   Bus_Width         : constant Bus_Mode := Mode_4bit;

   Data_Port         : Bits_In_Byte renames MCU.PORTD_Bits;
   Data_DD           : Bits_In_Byte renames MCU.DDRD_Bits;
   
   Data_PortB        : Bits_In_Byte renames MCU.PORTB_Bits;
   Data_DDB          : Bits_In_Byte renames MCU.DDRB_Bits;
   
   Data0             : Boolean renames Data_Port (5);
   Data1             : Boolean renames Data_Port (4);
   Data2             : Boolean renames Data_Port (3);
   Data3             : Boolean renames Data_Port (2);
   Data0_DD          : Boolean renames Data_DD (5);
   Data1_DD          : Boolean renames Data_DD (4);
   Data2_DD          : Boolean renames Data_DD (3);
   Data3_DD          : Boolean renames Data_DD (2);

   RegisterSelect    : Boolean renames Data_Port (7);
   RegisterSelect_DD : Boolean renames Data_DD (7);
   Enable            : Boolean renames Data_Port (6);
   Enable_DD         : Boolean renames Data_DD (6);

   ReadWrite         : Boolean renames Data_PortB (0);
   ReadWrite_DD      : Boolean renames Data_DDB (0);
   
   Processor_Speed   : constant := 16_000_000;

end LCD.Wiring;
