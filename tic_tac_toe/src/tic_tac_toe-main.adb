with Ada.Exceptions;

with Gnoga.Application.Multi_Connect;

with Tic_Tac_Toe.Controller;

procedure Tic_Tac_Toe.Main is
begin
   Gnoga.Application.Title ("Tic Tac Toe");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");
   
   Gnoga.Application.Multi_Connect.Initialize;
   
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end Tic_Tac_Toe.Main;
