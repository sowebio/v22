with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package Breadcrumb is

   function Add_To_Breadcrumb
     (View          : in out Gnoga.Gui.View.View_Type;
      Handler       : in     Gnoga.Gui.Base.Action_Event;
      Content       : in     UXString := "";
      Current_Depth : in     Integer;
      Depth         : in     Integer  := 0)
      return Integer;

   function Remove_From_Breadcrumb
     (View         : in out Gnoga.Gui.View.View_Type;
      Current_Depth : in     Integer)
      return Integer;

   function Update_Breadcrumb
     (View          : in out Gnoga.Gui.View.View_Type;
      Handler       : in     Gnoga.Gui.Base.Action_Event;
      Content       : in     UXString := "";
      Current_Depth : in out Integer;
      Depth         : in     Integer  := 0)
      return Integer;

end Breadcrumb;
