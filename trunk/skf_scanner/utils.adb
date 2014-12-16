
with Gnat.Traceback.Symbolic;

package body Utils is

  protected body Binary_Lock_Type is
    --------------------------------------------------------------------------------
    entry Lock when not Locked is
    begin
      Locked := True;
    end Lock;
    --------------------------------------------------------------------------------
    procedure Release is
    begin
      Locked := False;
    end Release;
    --------------------------------------------------------------------------------
    function Is_Locked return Boolean is
    begin
      return Locked;
    end Is_Locked;
    --------------------------------------------------------------------------------
  end Binary_Lock_Type;


  ---------------------------------------------------------------------------
  function Tracebackinfo(E : Ada.Exceptions.Exception_Occurrence) return String is
  begin
    return "Exception raised : "                                 & Ascii.LF &
           Ada.Exceptions.Exception_Name(E)                      & Ascii.LF &
           "Message : " & Ada.Exceptions.Exception_Message(E)    & Ascii.LF &
           Ada.Exceptions.Exception_Information(E)               & Ascii.LF &
           "..................................................." & Ascii.LF &
           "Hex      Subprogram name and file"                   & Ascii.LF &
           "-----    ------------------------"                   & Ascii.LF &
           Gnat.Traceback.Symbolic.Symbolic_Traceback(E);
  end Tracebackinfo;

end Utils;


