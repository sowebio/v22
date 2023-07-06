with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element; use Gnoga.Gui.Element;

package body Breadcrumb is

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   procedure Remove_Last (Instance : in out Breadcrumb_Type) is
      Element_Name : UXString := "";
   begin
      Element_Name := "Button_" & From_UTF_8 (Instance.Current_Depth'Image).Delete (1, 1);
      if Instance.Parent.Element (Element_Name) /= null then
         Instance.Parent.Element (Element_Name).Remove;
      end if;
      Element_Name := "Icon_" & From_UTF_8 (Instance.Current_Depth'Image).Delete (1, 1);
      if Instance.Parent.Element (Element_Name) /= null then
         Instance.Parent.Element (Element_Name).Remove;
      end if;
   end Remove_Last;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   function Create
     (View : in out Gnoga.Gui.View.View_Type)
      return Breadcrumb_Type
   is
      Result : Breadcrumb_Type;
   begin
      Result.Parent := View'Unrestricted_Access;
      return Result;
   end Create;

   procedure Add
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0)
   is
      Button_Name : UXString;
      Icon_Name   : UXString;
      New_Button  : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
      New_Icon    : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.IMG_Type;
   begin
      Button_Name := "Button_" & From_UTF_8 (Depth'Image).Delete (1, 1);
      Icon_Name   := "Icon_" & From_UTF_8 (Depth'Image).Delete (1, 1);
      if Depth > 0 then
         Gnoga.Gui.Element.Common.IMG_Access (New_Icon).Create
           (Instance.Parent.all, URL_Source => "/css/icons/chevron.png");
         New_Icon.Style ("height", "40px");
         New_Icon.Style ("width", "40px");
         New_Icon.Margin (Left => "-4px", Right => "-4px");
         New_Icon.Dynamic;
         Instance.Parent.Add_Element (Icon_Name, New_Icon);
      end if;
      Gnoga.Gui.Element.Common.Button_Access (New_Button).Create (Instance.Parent.all, Content);
      New_Button.On_Click_Handler (Handler);
      New_Button.Style ("width", "auto");
      New_Button.Style ("height", "40px");
      New_Button.Style ("min-height", "40px");
      New_Button.Style ("margin", "0");
      New_Button.Dynamic;
      Instance.Parent.Add_Element (Button_Name, New_Button);
      Instance.Current_Depth := Depth;
   end Add;

   procedure Update
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0)
   is
   begin
      if Instance.Current_Depth < Depth then
         Instance.Add (Handler => Handler, Content => Content, Depth => Depth);
      elsif Instance.Current_Depth = Depth then
         Instance.Remove_Last;
         Instance.Add (Handler => Handler, Content => Content, Depth => Depth);
      elsif Instance.Current_Depth > Depth then
         while Instance.Current_Depth > Depth loop
            Instance.Remove_Last;
            Instance.Current_Depth := Instance.Current_Depth - 1;
         end loop;
         Instance.Remove_Last;
         Instance.Add (Handler => Handler, Content => Content, Depth => Depth);
      end if;
      Instance.Current_Depth := Depth;
   end Update;

end Breadcrumb;
