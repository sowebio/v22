-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-gui-breadcrumb.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Breadcrumb package
--
--  @description
--
--  @authors
--  Théodore Gigault - tg - developpement@soweb.io
--  Arthur Le Floch - alf - developpement@soweb.io
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

package body v22.Gui.Breadcrumb is

   package Element renames Gnoga.Gui.Element;
   package Common renames Gnoga.Gui.Element.Common;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   function To_UXString
     (Value : Integer)
      return String
   is
   begin
      return From_UTF_8 (Value'Image).Delete (1, 1);
   end To_UXString;

   procedure Remove_Last (Instance : in out Breadcrumb_Type) is
      Element_Name  : String          := "";
      Element_Index : constant String := To_UXString (Instance.Current_Depth);
   begin
      Element_Name := "Button_" & Element_Index;
      if Instance.Parent.all.Element (Element_Name) /= null then
         Instance.Parent.all.Element (Element_Name).Remove;
      end if;
      Element_Name := "Icon_" & Element_Index;
      if Instance.Parent.all.Element (Element_Name) /= null then
         Instance.Parent.all.Element (Element_Name).Remove;
      end if;
   end Remove_Last;

   procedure Add
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Base.Action_Event;
      Content  : in     String := "";
      Depth    : in     Integer  := 0)
   is
      Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
      Icon   : constant Element.Pointer_To_Element_Class := new Common.IMG_Type;
   begin
      if Depth > 0 then
         Common.IMG_Access (Icon).Create (Instance.Parent.all);
         Common.IMG_Access (Icon).URL_Source ("/css/icons/chevron.png");
         Icon.Style ("height", "25px");
         Icon.Style ("width", "25px");
         Icon.Style ("margin", "0 -2px");
         Icon.Dynamic;
         Instance.Parent.all.Add_Element ("Icon_" & To_UXString (Depth), Icon);
      end if;
      Common.Button_Access (Button).Create (Instance.Parent.all, Content);
      Button.On_Click_Handler (Handler);
      Button.Class_Name ("framework-link");
      Button.Dynamic;
      Instance.Parent.all.Add_Element ("Button_" & To_UXString (Depth), Button);
      Instance.Current_Depth := Depth;
   end Add;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   procedure Create
     (Instance : in out Breadcrumb_Type;
      Parent   : in out View.View_Type)
   is
   begin
      Instance.Parent := Parent'Unrestricted_Access;
   end Create;

   procedure Update
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Base.Action_Event;
      Content  : in     String := "";
      Depth    : in     Integer  := 0)
   is
   begin
      if Instance.Current_Depth < Depth then
         Instance.Add (Handler, Content, Depth);
      elsif Instance.Current_Depth = Depth then
         Instance.Remove_Last;
         Instance.Add (Handler, Content, Depth);
      elsif Instance.Current_Depth > Depth then
         while Instance.Current_Depth > Depth loop
            Instance.Remove_Last;
            Instance.Current_Depth := Instance.Current_Depth - 1;
         end loop;
         Instance.Remove_Last;
         Instance.Add (Handler, Content, Depth);
      end if;
      Instance.Current_Depth := Depth;
   end Update;

   procedure Clear (Instance : in out Breadcrumb_Type) is
   begin
      Instance.Update (null, "", 0);
      Instance.Remove_Last;
   end Clear;

-------------------------------------------------------------------------------
end v22.Gui.Breadcrumb;
-------------------------------------------------------------------------------
