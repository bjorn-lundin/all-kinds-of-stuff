
with Ada.Text_IO;

package Types is

   type Byte is range 0 .. 2**8-1;
   for  Byte'Size use 8;

   type Integer_2 is range -2**15 .. 2**15-1;
   for  Integer_2'Size use 16;

   type Word is range 0 .. 2**16-1;
   for Word'size use 16;
   
   type Integer_4 is range -2**31 .. 2**31-1;
   for  Integer_4'Size use 32;

   --type Integer_8 is range -9_223_372_036_854_775_808 .. 9_223_372_036_854_775_807;
   --type Integer_8 is range -2**63 .. 2**63-1;
   --for  Integer_8'Size use 64;

   type Float_8 is new Long_Float; 
   
   package F8 is new Ada.Text_IO.Float_IO (Float_8);
   
end Types;
