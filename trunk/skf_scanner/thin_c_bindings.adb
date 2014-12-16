
with Config;

package body Thin_C_Bindings is

  --------------------------------------------------------------------------------

  function Ioperm(From, Num : in C.Int ; Turn_On : in Boolean) return Boolean is
    use type C.Int;
    i : C.Int := 2;
    function C_Ioperm(From, Num, Turn_On : in C.Int ) return C.Int ;
    pragma import(C,C_Ioperm,"ioperm");
  begin
    i := C_Ioperm(From,Num,Boolean'pos(Turn_On));
    return (i /= -1);
  end Ioperm;

  --------------------------------------------------------------------------------

  procedure Outb(Value : in Byte ; Port : in C.Int) is
    procedure C_Outb(Value : in Byte ; Port : in C.Int);
    pragma import(C,C_Outb,"c_outb");
  begin
    C_Outb(Value,Port);
  end Outb;

  --------------------------------------------------------------------------------

  function Inb(Port : in C.Int) return Byte is
    function C_Inb(Port : in C.Int) return Byte;
    pragma import(C,C_Inb,"c_inb");
  begin
    return C_Inb(Port);
  end Inb;

  --------------------------------------------------------------------------------

  procedure Write_Parallel_Port(Value : Byte; Port : C.Int ) is
  begin
    Outb(Value,Port);
  end Write_Parallel_Port;

 --------------------------------------------------------------------------------
  procedure Write_Parallel_Port(Value : Byte) is
  begin
    Write_Parallel_Port(Value => Value, Port => Config.Parallel_Port_Address) ;
  end Write_Parallel_Port;

 --------------------------------------------------------------------------------

  procedure Perror(Text : in String) is
    --void perror(const char *s);
    procedure C_Perror(Text : in String);
    pragma import(C,C_perror,"perror");
  begin
    C_Perror(Text & Ascii.Nul);
  end Perror;

  --------------------------------------------------------------------------------

  procedure Get_Parallel_Port_Access is
  begin
--   Get access to the port
    if not Ioperm(Config.Parallel_Port_Address, 8, True) then
      Perror("Get_Parallel_Port_Access");
      raise Port_Access_Failure;
    end if;
  end Get_Parallel_Port_Access;

  --------------------------------------------------------------------------------


end Thin_C_Bindings;


