
with Interfaces.C;

package Thin_C_Bindings is

  package C renames Interfaces.C;

  type Byte is range 0..255;
  for Byte'Size use 8;

  Port_Access_Failure : exception;

  --------------------------------------------------------------------------------

  function Ioperm(From, Num : in C.Int ; Turn_On : in Boolean) return Boolean ;

  --------------------------------------------------------------------------------

  procedure Outb(Value : in Byte ; Port : in C.Int) ;

  --------------------------------------------------------------------------------

  function Inb(Port : in C.Int) return Byte ;

  --------------------------------------------------------------------------------

--  procedure Write_Parallel_Port(Value : Byte; Port : C.Int ) ;
  procedure Write_Parallel_Port(Value : Byte) ;

 --------------------------------------------------------------------------------

  procedure Perror(Text : in String) ;
  --------------------------------------------------------------------------------

  procedure Get_Parallel_Port_Access ;
  --------------------------------------------------------------------------------



end Thin_C_Bindings;


