with Gnoga.Gui.Base;
with Gnoga.Gui.Element;
with Tic_Tac_Toe.View;
with Gnoga.Server.Template_Parser;
with Gnoga.Gui.Element.Common;
with Text_Io;

package body Tic_Tac_Toe.Controller is



   procedure Debug(Where, What : in String) is
   begin
     Text_Io.Put_Line(Text_Io.Standard_Error,Where & " - " & What);
   end Debug;

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure On_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : Tic_Tac_Toe.View.Default_View_Access :=
               Tic_Tac_Toe.View.Default_View_Access (Object.Parent);
   begin
      View.Label_Text.Put_Line ("Click");
   end On_Click;


      -- set source
   procedure Start_Drag (Object       : in out Gnoga.Gui.Base.Base_Type'Class;
                         Row          : in     Tic_Tac_Toe.View.Row_Type;
                         Col          : in     Tic_Tac_Toe.View.Col_Type ) ;
   -- reset source
   procedure End_Drag (Object       : in out Gnoga.Gui.Base.Base_Type'Class) ;



      -- set target
   procedure Enter_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                         Row       : in     Tic_Tac_Toe.View.Row_Type;
                         Col       : in     Tic_Tac_Toe.View.Col_Type ) ;
      -- clear target
   procedure Leave_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class ) ;

      -- execute on target
   procedure Drop (Object    : in out Gnoga.Gui.Base.Base_Type'Class;
                   Drag_Text : in     String ) ;




   procedure Start_Drag_1_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 1, 1);
   end Start_Drag_1_1;
   ---------
   procedure Start_Drag_1_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 1, 2);
   end Start_Drag_1_2;
   -----------
   procedure Start_Drag_1_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 1, 3);
   end Start_Drag_1_3;
   -----------
   procedure Start_Drag_1_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 1, 4);
   end Start_Drag_1_4;
   -----------
   procedure Start_Drag_2_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 2, 1);
   end Start_Drag_2_1;
   ---------
   procedure Start_Drag_2_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 2, 2);
   end Start_Drag_2_2;
   -----------
   procedure Start_Drag_2_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 2, 3);
   end Start_Drag_2_3;
   -----------
   procedure Start_Drag_2_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 2, 4);
   end Start_Drag_2_4;
   -----------
   procedure Start_Drag_3_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 3, 1);
   end Start_Drag_3_1;
   ---------
   procedure Start_Drag_3_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 3, 2);
   end Start_Drag_3_2;
   -----------
   procedure Start_Drag_3_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 3, 3);
   end Start_Drag_3_3;
   -----------
   procedure Start_Drag_3_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 3, 4);
   end Start_Drag_3_4;
   -----------
   procedure Start_Drag_4_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 4, 1);
   end Start_Drag_4_1;
   ---------
   procedure Start_Drag_4_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 4, 2);
   end Start_Drag_4_2;
   -----------
   procedure Start_Drag_4_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 4, 3);
   end Start_Drag_4_3;
   -----------
   procedure Start_Drag_4_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Start_Drag(Object, 4, 4);
   end Start_Drag_4_4;
   -----------


   procedure Enter_Drag_1_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 1, 1);
   end Enter_Drag_1_1;
   ---------
   procedure Enter_Drag_1_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 1, 2);
   end Enter_Drag_1_2;
   -----------
   procedure Enter_Drag_1_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 1, 3);
   end Enter_Drag_1_3;
   -----------
   procedure Enter_Drag_1_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 1, 4);
   end Enter_Drag_1_4;
   -----------
   procedure Enter_Drag_2_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 2, 1);
   end Enter_Drag_2_1;
   ---------
   procedure Enter_Drag_2_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 2, 2);
   end Enter_Drag_2_2;
   -----------
   procedure Enter_Drag_2_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 2, 3);
   end Enter_Drag_2_3;
   -----------
   procedure Enter_Drag_2_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 2, 4);
   end Enter_Drag_2_4;
   -----------
   procedure Enter_Drag_3_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 3, 1);
   end Enter_Drag_3_1;
   ---------
   procedure Enter_Drag_3_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 3, 2);
   end Enter_Drag_3_2;
   -----------
   procedure Enter_Drag_3_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 3, 3);
   end Enter_Drag_3_3;
   -----------
   procedure Enter_Drag_3_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 3, 4);
   end Enter_Drag_3_4;
   -----------
   procedure Enter_Drag_4_1 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 4, 1);
   end Enter_Drag_4_1;
   ---------
   procedure Enter_Drag_4_2 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 4, 2);
   end Enter_Drag_4_2;
   -----------
   procedure Enter_Drag_4_3 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 4, 3);
   end Enter_Drag_4_3;
   -----------
   procedure Enter_Drag_4_4 (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
     Enter_Drag(Object, 4, 4);
   end Enter_Drag_4_4;
   -----------


      -- set source for Board
   procedure Start_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                         Row    : in     Tic_Tac_Toe.View.Row_Type;
                         Col    : in     Tic_Tac_Toe.View.Col_Type ) is
      View : Tic_Tac_Toe.View.Default_View_Access :=
               Tic_Tac_Toe.View.Default_View_Access (Object.Parent);
      use Tic_Tac_Toe.View;
   begin
      Debug("start 'Start_Drag'",View.Source.To_String & " Target: " & View.Target.To_String);
     -- Gnoga.Gui.Element.Common.DIV_Type(Object).Opacity (0.4);
      View.Source.Row := Row;
      View.Source.Col := Col;

      if Row = Row_Type'last and then Col < Col_Type'last then -- the X's
        View.Is_Dragging := X;
        View.Source.On_Board := False;
      elsif Row < Row_Type'last and then Col = Col_Type'last then  -- the O's
        View.Is_Dragging := O;
        View.Source.On_Board := False;
      elsif Row < Row_Type'last and then Col < Col_Type'last then  -- get it from board
        View.Is_Dragging := View.Board(View.Source.Row, View.Source.Col).State;
        View.Source.On_Board := True;
      else
        View.Source.On_Board := False;
        View.Source.Is_Set := False;
        Debug("'within_Drag'",View.Source.To_String & " Target: " & View.Target.To_String); --(4,4)
        return;
      end if;
      View.Source.Is_Set := True;

      Debug("stop 'Start_Drag'",View.Source.To_String & " Target: " & View.Target.To_String);
      Gnoga.Server.Template_Parser.Write_String_To_File
        ("start_drag2.html",View.Outer_HTML);
   end Start_Drag;

      -- for Board -- source was set by start_drag
   procedure End_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      View : Tic_Tac_Toe.View.Default_View_Access :=
               Tic_Tac_Toe.View.Default_View_Access (Object.Parent);
      use Tic_Tac_Toe.View ;
   begin
     null;
--      Debug("Start 'End_Drag'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String);
--      View.Is_Dragging := Free;
--      View.Source.Is_Set := False;
--      Debug("stop 'End_Drag'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String);
   end End_Drag;
   ------------------------------------------------------------------------------

    -- set target
   procedure Enter_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                         Row    : in     Tic_Tac_Toe.View.Row_Type;
                         Col    : in     Tic_Tac_Toe.View.Col_Type ) is
      View : Tic_Tac_Toe.View.Default_View_Access :=
               Tic_Tac_Toe.View.Default_View_Access (Object.Parent);
      use Tic_Tac_Toe.View ;
   begin
     Debug("start 'Enter_Drag'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String);
     --case View.Board(Row,Col).State is
     --  when Free  => View.Board(Row,Col).Elem.Border (Color => "Green");
     --  when X | O => View.Board(Row,Col).Elem.Border (Color => "Red");
     --end case;
     View.Target.Row := Row;
     View.Target.Col := Col;
     View.Target.Is_Set := True;
      if Row = Row_Type'last and then Col < Col_Type'last then -- the X's
        View.Target.On_Board := False;
      elsif Row < Row_Type'last and then Col = Col_Type'last then  -- the O's
        View.Target.On_Board := False;
      elsif Row < Row_Type'last and then Col < Col_Type'last then  -- get it from board
        View.Target.On_Board := True;
      else
        View.Target.On_Board := False;
      end if;
     
     Debug("stop 'Enter_Drag'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String);
   end Enter_Drag;
   ------------------------------------------------------------------------------

   -- target is set here by Enter_Drag
   procedure Leave_Drag (Object : in out Gnoga.Gui.Base.Base_Type'Class ) is
      View : Tic_Tac_Toe.View.Default_View_Access :=
               Tic_Tac_Toe.View.Default_View_Access (Object.Parent);
      use Tic_Tac_Toe.View ;
   begin
     null;
     --Debug("start 'Leave_Drag'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String);
     --case View.Board(View.Target.Row,View.Target.Col).State is
     --  when Free  => View.Board(View.Target.Row,View.Target.Col).Elem.Border (Color => "Green");
     --  when X | O => View.Board(View.Target.Row,View.Target.Col).Elem.Border (Color => "Red");
     --end case;
     ----View.Target.Is_Set := False;
     --Debug("stop 'Leave_Drag'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String);
   end Leave_Drag;
   --------------------------

   procedure Drop (Object    : in out Gnoga.Gui.Base.Base_Type'Class;
                   Drag_Text : in     String ) is
      View : Tic_Tac_Toe.View.Default_View_Access :=
               Tic_Tac_Toe.View.Default_View_Access (Object.Parent);
      use Tic_Tac_Toe.View ;
   begin
     Debug("start 'Drop'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String );
     if not View.Target.Is_Set then
       Debug("inside 'Drop'","target not set");
       return;
     end if;
     
     if not View.Target.On_Board then
       Debug("inside 'Drop'","target not on board");
       return;
     end if;

     case View.Board(View.Target.Row,View.Target.Col).State is
       when Free  =>
        -- View.Board(View.Target.Row,View.Target.Col).Elem.Border (Color => "Blue");
         View.Board(View.Target.Row,View.Target.Col).Elem.Text (Drag_Text);

         case View.Is_Dragging is
           when Free => Debug ("inside 'Drop'","View.Is_Dragging is 'Free' !!");
           when X    =>
             View.Board(View.Target.Row,View.Target.Col).State := X ;
             View.Board(View.Target.Row,View.Target.Col).Elem.Put_HTML ("<img src='/img/x.png'>");
             -- empty where we came from
             View.Board(View.Source.Row,View.Source.Col).Elem.Inner_HTML("");
             View.Board(View.Source.Row,View.Source.Col).State := Free;

           when O    =>
             View.Board(View.Target.Row,View.Target.Col).State := O ;
             View.Board(View.Target.Row,View.Target.Col).Elem.Put_HTML ("<img src='/img/o.png'>");
             -- empty where we came from

             View.Board(View.Source.Row,View.Source.Col).Elem.Inner_HTML("");
             View.Board(View.Source.Row,View.Source.Col).State := Free;

         end case;
       when X | O => null;
        -- View.Board(View.Target.Row,View.Target.Col).Elem.Border (Color => "Red");
     end case;
     
   --  View.Board(View.Source.Row,View.Source.Col).Elem.Opacity (1.0);
     View.Board(View.Source.Row,View.Source.Col).Elem.Draggable(View.Source.On_Board);
     -- reset
     
     View.Is_Dragging := Free;
     View.Source.Is_Set := False;
     View.Target.Is_Set := False;
     
     Gnoga.Server.Template_Parser.Write_String_To_File
        ("drop.html",View.Outer_HTML);
     Debug("stop 'Drop'", "Source: " & View.Source.To_String & " Target: " & View.Target.To_String);
   end Drop;



   procedure Default
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  : access
        Gnoga.Application.Multi_Connect.Connection_Holder_Type) is
      View : Tic_Tac_Toe.View.Default_View_Access :=
               new Tic_Tac_Toe.View.Default_View_Type;
      use Tic_Tac_Toe.View;
   begin
      View.Dynamic;
      View.Create (Main_Window);
      View.Click_Button.On_Click_Handler (On_Click'access);

      --  Draw our board at a fixed position in browser
      for Row in Row_Type'range loop
        for Col in Col_Type'range loop
          View.Board(Row,Col).Elem.Position (Gnoga.Gui.Element.Fixed);
          View.Board(Row,Col).Elem.Top(150 * Integer(Row));
          View.Board(Row,Col).Elem.Left(150 * Integer(Col));
          View.Board(Row,Col).Elem.Width(112);
          View.Board(Row,Col).Elem.Height(112);
          View.Board(Row,Col).Elem.Border;

          View.Board(Row,Col).Elem.On_Drop_Handler(Drop'Unrestricted_Access); -- is shared
          View.Board(Row,Col).Elem.On_Drag_Leave_Handler(Leave_Drag'Unrestricted_Access); -- is shared
          View.Board(Row,Col).Elem.On_Drag_End_Handler(End_Drag'Unrestricted_Access);

          if Row = Row_Type'last and then Col < Col_Type'last then
            View.Board(Row,Col).Elem.Put_HTML ("<img src='/img/x.png'>");
            View.Board(Row,Col).Elem.Draggable;
            View.Board(Row,Col).Elem.Border (Color => "Red");
            
          elsif Col = Col_Type'last and then Row < Row_Type'last then
            View.Board(Row,Col).Elem.Put_HTML ("<img src='/img/o.png'>");
            View.Board(Row,Col).Elem.Draggable;
            View.Board(Row,Col).Elem.Border (Color => "Red");
          elsif Col = Col_Type'last and then Row = Row_Type'last then
            View.Board(Row,Col).Elem.Hidden(True); -- do not show/remove
          end if;


          case Row is
            when 1 =>
              case Col is
                when 1 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_1_1'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_1_1'Unrestricted_Access,
                             Drag_Text => "" );
                when 2 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_1_2'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_1_2'Unrestricted_Access,
                             Drag_Text => "" );
                when 3 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_1_3'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_1_3'Unrestricted_Access,
                             Drag_Text => "" );
                when 4 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_1_4'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_1_4'Unrestricted_Access,
                             Drag_Text => "" );
              end case;
            when 2 =>
              case Col is
                when 1 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_2_1'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_2_1'Unrestricted_Access,
                             Drag_Text => "" );
                when 2 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_2_2'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_2_2'Unrestricted_Access,
                             Drag_Text => "" );
                when 3 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_2_3'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_2_3'Unrestricted_Access,
                             Drag_Text => "" );
                when 4 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_2_4'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_2_4'Unrestricted_Access,
                             Drag_Text => "" );
              end case;
            when 3 =>
              case Col is
                when 1 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_3_1'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_3_1'Unrestricted_Access,
                             Drag_Text => "" );
                when 2 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_3_2'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_3_2'Unrestricted_Access,
                             Drag_Text => "" );
                when 3 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_3_3'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_3_3'Unrestricted_Access,
                             Drag_Text => "" );
                when 4 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_3_4'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_3_4'Unrestricted_Access,
                             Drag_Text => "" );
              end case;
            when 4 =>
              case Col is
                when 1 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_4_1'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_4_1'Unrestricted_Access,
                             Drag_Text => "" );
                when 2 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_4_2'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_4_2'Unrestricted_Access,
                             Drag_Text => "" );
                when 3 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_4_3'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_4_3'Unrestricted_Access,
                             Drag_Text => "" );
                when 4 =>
                   View.Board(Row,Col).Elem.On_Drag_Enter_Handler (Enter_Drag_4_4'Unrestricted_Access);
                   View.Board(Row,Col).Elem.On_Drag_Start_Handler(
                             Handler   => Start_Drag_4_4'Unrestricted_Access,
                             Drag_Text => "" );
              end case;
          end case;
        end loop;
      end loop;

     Gnoga.Server.Template_Parser.Write_String_To_File
        ("site2.html",View.Outer_HTML);


   end Default;

begin
   Gnoga.Application.Multi_Connect.On_Connect_Handler
     (Default'Access, "default");
end Tic_Tac_Toe.Controller;
