with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element.Common;

package Tic_Tac_Toe.View is
   
   type Row_Type is new Integer range 1..4 ;
   type Col_Type is new Integer range 1..4 ;
   
   type Square_Value_Type is (X, O, Free);
   
   type Square_Type is record
     State : Square_Value_Type := Free;
     Elem  : Gnoga.Gui.Element.Common.DIV_Type;
   end record;
   
   type Coordinats_Type is tagged record
     Row : Row_Type := 4; 
     Col : Col_Type := 4; 
     On_Board : Boolean := False;
     Is_Set   : Boolean := False;
   end record;
  function To_String(C : Coordinats_Type) return String;

   type Board_Type is array(Row_Type'range, Col_Type'range) of Square_Type;
   
   type Default_View_Type is new Gnoga.Gui.View.View_Type with
      record
         Label_Text     : Gnoga.Gui.View.View_Type;
         Click_Button   : Gnoga.Gui.Element.Common.Button_Type;
         Board          : Board_Type;
         Is_Dragging    : Square_Value_Type := Free; -- source type
         Source         : Coordinats_Type;
         Target         : Coordinats_Type;
      end record;
   type Default_View_Access is access all Default_View_Type;
   type Pointer_to_Default_View_Class is access all Default_View_Type'Class;

   overriding
   procedure Create
     (View   : in out Default_View_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String  := "");     
   
end Tic_Tac_Toe.View;
