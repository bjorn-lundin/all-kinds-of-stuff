with Ada.Exceptions;

with Gnoga.Application.Singleton;
with Gnoga.Gui.Window;
with Gnoga.Gui.View.Console;
with Gnoga.Gui.View.Grid;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Canvas;
with Gnoga.Gui.Element.Canvas.Context_2D;
procedure dsp is
   use Gnoga.Gui.View.Grid;
   
   Main_Window : Gnoga.Gui.Window.Window_Type;
   
   Layout_View  : Gnoga.Gui.View.Grid.Grid_View_Type;   
   Text_View    : Gnoga.Gui.View.Console.Console_View_Type;
   Canvas_View  : Gnoga.Gui.View.View_Type;
   My_Canvas    : Gnoga.Gui.Element.Canvas.Canvas_Type;
   Start_Button : Gnoga.Gui.Element.Common.Button_Type;
begin
   Gnoga.Application.Title ("dsp");
   Gnoga.Application.HTML_On_Close
     ("<b>Connection to Application has been terminated</b>");
   
   Gnoga.Application.Open_URL ("http://127.0.0.1:8080");
   Gnoga.Application.Singleton.Initialize (Main_Window, Port => 8080);

   Layout_View.Create (Parent      => Main_Window,
                       Layout      => ((1 => COL), (1 => COL), (1 => COL)));
   
   Start_Button.Create (Layout_View.Panel (1, 1).all, "Start");   

   Canvas_View.Create (Layout_View.Panel (2, 1).all);
   Canvas_View.Overflow (Gnoga.Gui.Element.Auto);
   Canvas_View.Fill_Parent;
   My_Canvas.Create (Canvas_View, 640, 480);
   My_Canvas.Border;

   Text_View.Create (Layout_View.Panel (3, 1).all);
   Text_View.Fill_Parent;
   
   for i in 1 .. 100 loop
      Text_View.Put_Line ("Hello World!");
   end loop;
   
   Gnoga.Application.Singleton.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (Ada.Exceptions.Exception_Name (E) & " - " &
                   Ada.Exceptions.Exception_Message (E));
end dsp;

