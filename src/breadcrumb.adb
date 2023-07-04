with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element; use Gnoga.Gui.Element;

package body Breadcrumb is

   function Create return Breadcrumb_Type is
      Result : Breadcrumb_Type;
   begin
      return Result;
   end Create;

   procedure Add
     (Instance : in out Breadcrumb_Type;
      View     : in out Gnoga.Gui.View.View_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0)
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
      New_Button.Style ("left", Left_String & "px");
      New_Button.Style ("width", "130px");
      New_Button.Style ("height", "40px");
      New_Button.Style ("min-height", "40px");
      New_Button.Style ("margin", "0");
      Instance.Current_Depth := Depth;
   end Add;

   procedure Remove
     (Instance : in out Breadcrumb_Type;
      View     : in out Gnoga.Gui.View.View_Type)
   is
      Element_Name : UXString := "";
   begin
      Element_Name := "Button_" & From_UTF_8 (Instance.Current_Depth'Image).Delete (1, 1);
      if View.Element (Element_Name) /= null then
         View.Element (Element_Name).Remove;
      end if;
   end Remove;

   procedure Update
     (Instance : in out Breadcrumb_Type;
      View     : in out Gnoga.Gui.View.View_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0)
   is
   begin
      if Instance.Current_Depth < Depth then
         Add (Instance, View => View, Handler => Handler, Content => Content, Depth => Depth);

      elsif Instance.Current_Depth = Depth then
         Remove (Instance, View => View);
         Add (Instance, View => View, Handler => Handler, Content => Content, Depth => Depth);

      elsif Instance.Current_Depth > Depth then
         while Instance.Current_Depth > Depth loop
            Remove (Instance, View => View);
            Instance.Current_Depth := Instance.Current_Depth - 1;
         end loop;
         Remove (Instance, View => View);
         Add (Instance, View => View, Handler => Handler, Content => Content, Depth => Depth);
      end if;
      Instance.Current_Depth := Depth;
   end Update;

end Breadcrumb;
