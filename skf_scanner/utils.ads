

with Ada.Exceptions;

package Utils is



  protected type Binary_Lock_Type is
    --------------------------------------------------------------------------------
    entry Lock;
    --------------------------------------------------------------------------------
    procedure Release;
    --------------------------------------------------------------------------------
    function Is_Locked return Boolean;
    --------------------------------------------------------------------------------
    private
      Locked : Boolean := False;
  end Binary_Lock_Type;
  --------------------------------------------------------------------------- 



  function Tracebackinfo(E : Ada.Exceptions.Exception_Occurrence) return String ;
  --------------------------------------------------------------------------- 

end Utils;


