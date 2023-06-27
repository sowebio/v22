with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element; use Gnoga.Gui.Element;

package body Breadcrumb is

   function Add_To_Breadcrumb
     (View          : in out Gnoga.Gui.View.View_Type;
      Handler       : in     Gnoga.Gui.Base.Action_Event;
      Content       : in     UXString := "";
      Current_Depth : in     Integer;
      Depth         : in     Integer  := 0)
      return Integer
   is
      Left_String  : UXString                                            := "";
      Element_Name : UXString                                            := "";
      New_Button   : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
   begin
      Element_Name := "Button_" & From_UTF_8 (Depth'Image).Delete (1, 1);
      Left_String  := From_UTF_8 (Integer'Image (150 * (Depth + 1)));
      Gnoga.Gui.Element.Common.Button_Access (New_Button).Create (View, Content);
      View.Add_Element (Element_Name, New_Button);
      New_Button.Dynamic;
      New_Button.On_Click_Handler (Handler);
      New_Button.Style ("position", "absolute");
      New_Button.Style ("top", "0px");
      New_Button.Style ("left", Left_String & "px");
      return Current_Depth;
   end Add_To_Breadcrumb;

   function Remove_From_Breadcrumb
     (View          : in out Gnoga.Gui.View.View_Type;
      Current_Depth : in     Integer)
      return Integer
   is
      Element_Name : UXString := "";
   begin
      Element_Name := "Button_" & From_UTF_8 (Current_Depth'Image).Delete (1, 1);
      if View.Element (Element_Name) /= null then
         View.Element (Element_Name).Remove;
      end if;
      return Current_Depth;
   end Remove_From_Breadcrumb;

   function Update_Breadcrumb
     (View          : in out Gnoga.Gui.View.View_Type;
      Handler       : in     Gnoga.Gui.Base.Action_Event;
      Content       : in     UXString := "";
      Current_Depth : in out Integer;
      Depth         : in     Integer  := 0)
      return Integer
   is
   begin
      if Current_Depth < Depth then
         Current_Depth :=
           Add_To_Breadcrumb
             (View => View, Handler => Handler, Content => Content, Current_Depth => Current_Depth, Depth => Depth);

      elsif Current_Depth = Depth then
         Current_Depth := Remove_From_Breadcrumb (View => View, Current_Depth => Current_Depth);
         Current_Depth :=
           Add_To_Breadcrumb
             (View => View, Handler => Handler, Content => Content, Current_Depth => Current_Depth, Depth => Depth);

      elsif Current_Depth > Depth then
         while Current_Depth > Depth loop
            Current_Depth := Remove_From_Breadcrumb (View => View, Current_Depth => Current_Depth);
            Current_Depth := Current_Depth - 1;
         end loop;
      end if;
      Current_Depth := Depth;
      return Current_Depth;
   end Update_Breadcrumb;

end Breadcrumb;
