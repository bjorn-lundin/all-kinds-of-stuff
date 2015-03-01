
with Tic_Tac_Toe.View;
package body Tic_Tac_Toe.View is

   ------------
   -- Create --
   ------------

   overriding
   procedure Create
     (View   : in out Default_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      
      View.Label_Text.Create (View);
      View.Click_Button.Create (View, "Click Me");
      for Row in Tic_Tac_Toe.View.Row_Type'range loop
        for Col in Tic_Tac_Toe.View.Col_Type'range loop     
          View.Board(Row,Col).Elem.Create(View,"");
        end loop;
      end loop;
   end Create;
 
--   type Coordinats_Type is tagged record
--   end record;  
  function To_String(C : Coordinats_Type) return String is
  begin
    return "Row=" & C.Row'img & 
           " Col=" & C.Col'img & 
           " On_Board=" & C.On_Board'img & 
           " Is_Set=" & C.Is_Set'img &
           "";           
  end To_String;

   
end Tic_Tac_Toe.View;
