-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-gui-header.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Header package
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

package body v22.Gui.Header is

   package Element renames Gnoga.Gui.Element;

   Header_Error : exception;

   Root_Parent_ID : constant Integer := -1;
   Root_Depth     : constant Integer := 0;

   type Data_Type is record
      Parent_ID : Integer  := Root_Parent_ID;
      Name      : String := "";
      Depth     : Integer  := Root_Depth;
      Is_Leaf   : Boolean  := True;
      Index     : Integer  := 0;

      On_Open : Base.Action_Event;
   end record;

   Max_Menu_Amount : constant Integer := 50;

   type Menu_Table_Type is array (1 .. Max_Menu_Amount) of Data_Type;

   Menu_Table : Menu_Table_Type;
   Last_Index : Integer := 0;

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

   function Button_Name
     (Index : Integer)
      return String
   is
   begin
      return "Menu_" & To_UXString (Index);
   end Button_Name;

   procedure Remove_Button
     (Instance : in out Header_Type;
      Data_ID  :        Integer)
   is
   begin
      if Instance.App_Navigation_Parent.Element (Button_Name (Data_ID)) /= null then
         Instance.App_Navigation_Parent.Element (Button_Name (Data_ID)).Remove;
      end if;
   end Remove_Button;

   procedure Update
     (Instance  : in out Header_Type;
      Parent_ID :        Integer)
   is
      Data : Data_Type;
      Button : Element.Pointer_To_Element_Class;
   begin
      for Data_ID in Menu_Table'Range loop
         Instance.Remove_Button (Data_ID);
         Data := Menu_Table (Data_ID);
         if Data.Parent_ID = Parent_ID then
            Button := new Common.Button_Type;
            Common.Button_Access (Button).Create (Instance.App_Navigation_Parent.all, Data.Name);
            Button.Class_Name ("framework-button");
            Button.Dynamic;
            Instance.App_Navigation_Parent.Add_Element (Button_Name (Data_ID), Button);
            Button.On_Click_Handler (Data.On_Open);
         end if;
      end loop;
   end Update;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   procedure Create
     (Instance         : in out Header_Type;
      Parent           : in out View.View_Type;
      On_Logo, On_User :        Base.Action_Event)
   is
      App_Parent            : constant View.Pointer_To_View_Class       := new View.View_Type;
      App_Icon              : constant Element.Pointer_To_Element_Class := new Common.IMG_Type;
      App_Navigation_Parent : constant View.Pointer_To_View_Class       := new View.View_Type;

      Breadcrumb_Parent : constant View.Pointer_To_View_Class := new View.View_Type;

      User_Parent            : constant View.Pointer_To_View_Class       := new View.View_Type;
      User_Name_Parent       : constant Element.Pointer_To_Element_Class := new Common.P_Type;
      User_Icon              : constant Element.Pointer_To_Element_Class := new Common.IMG_Type;
      User_Navigation_Parent : constant View.Pointer_To_View_Class       := new View.View_Type;
   begin
      Instance.Parent := Parent'Unrestricted_Access;

      App_Parent.Dynamic;
      App_Icon.Dynamic;
      App_Navigation_Parent.Dynamic;
      Breadcrumb_Parent.Dynamic;
      User_Parent.Dynamic;
      User_Name_Parent.Dynamic;
      User_Icon.Dynamic;
      User_Navigation_Parent.Dynamic;

      --  App icon & browse menu
      Instance.App_Parent := View.View_Access (App_Parent);
      Instance.App_Parent.Create (Parent);

      Instance.App_Parent.Class_Name ("header-app-parent");
      Instance.App_Icon := Common.IMG_Access (App_Icon);
      Instance.App_Icon.Create (Instance.App_Parent.all);
      Instance.App_Icon.Class_Name ("header-icon");
      Instance.App_Icon.On_Click_Handler (On_Logo);

      Instance.App_Navigation_Parent := View.View_Access (App_Navigation_Parent);
      Instance.App_Navigation_Parent.Create (Instance.App_Parent.all);
      Instance.App_Navigation_Parent.Class_Name ("header-app-browse-parent");

      --  Breadcrumb
      Instance.Breadcrumb_Parent := View.View_Access (Breadcrumb_Parent);
      Instance.Breadcrumb_Parent.Create (Parent);
      Instance.Breadcrumb_Parent.Class_Name ("header-breadcrumb-parent");
      Instance.Breadcrumb_Content.Create (Instance.Breadcrumb_Parent.all);

      --  User icon & user browse menu
      Instance.User_Parent := View.View_Access (User_Parent);
      Instance.User_Parent.Create (Parent);
      Instance.User_Parent.Class_Name ("header-user-parent");

      Instance.User_Name := Common.P_Access (User_Name_Parent);
      Instance.User_Name.Create (Instance.User_Parent.all);
      Instance.User_Name.Class_Name ("header-user-name");

      Instance.User_Icon := Common.IMG_Access (User_Icon);
      Instance.User_Icon.Create (Instance.User_Parent.all);
      Instance.User_Icon.Class_Name ("header-icon");
      Instance.User_Icon.On_Click_Handler (On_User);

      Instance.User_Navigation_Parent := View.View_Access (User_Navigation_Parent);
      Instance.User_Navigation_Parent.Create (Instance.User_Parent.all);
      Instance.User_Navigation_Parent.Class_Name ("header-user-browse-parent");

      Instance.User_Content.Create (Instance.User_Navigation_Parent.all);

      Instance.App_Navigation_Parent.Display ("none");
   end Create;

   -----------------------------------------------------------------------------
   --  Main Menu
   -----------------------------------------------------------------------------
   procedure Open_Menu
     (Instance  : in out Header_Type;
      Unique_ID :        Integer)
   is
   begin
      Instance.Set_Menu (Unique_ID);
      Instance.App_Navigation_Parent.Display ("block");
      Update (Instance, Unique_ID);
      Instance.App_Icon.Add_Class ("header-icon-active");
      Instance.App_Is_Open := True;
   end Open_Menu;

   procedure Close_Menu (Instance : in out Header_Type) is
   begin
      Instance.App_Navigation_Parent.Display ("none");
      Instance.App_Icon.Remove_Class ("header-icon-active");
      Instance.App_Is_Open := False;
   end Close_Menu;

   function Is_Menu_Open
     (Instance : in out Header_Type)
      return Boolean
   is
   begin
      return Instance.App_Is_Open;
   end Is_Menu_Open;

   function Set_Root
     (Name    : String;
      On_Open : Base.Action_Event)
      return Integer
   is
      Root : Data_Type;
   begin
      if Last_Index /= 0 then
         raise Header_Error with "Root must be created before childs";
      elsif Last_Index = Max_Menu_Amount then
         raise Header_Error with "Too much menus, increase Max_Menu_Amount";
      end if;
      Last_Index              := Last_Index + 1;
      Root.Name               := Name;
      Root.On_Open            := On_Open;
      Menu_Table (Last_Index) := Root;
      return Last_Index;
   end Set_Root;

   function Add_Child
     (Parent_ID : Integer;
      Name      : String;
      On_Open   : Base.Action_Event)
      return Integer
   is
      Child : Data_Type;
   begin
      if Last_Index = 0 then
         raise Header_Error with "Root must be created before childs";
      elsif Last_Index = Max_Menu_Amount then
         raise Header_Error with "Too much menus, increase Max_Menu_Amount";
      end if;
      Last_Index                     := Last_Index + 1;
      Menu_Table (Parent_ID).Is_Leaf := False;
      Child.Parent_ID                := Parent_ID;
      Child.Name                     := Name;
      Child.Depth                    := Menu_Table (Parent_ID).Depth + 1;
      Child.On_Open                  := On_Open;
      Menu_Table (Last_Index)        := Child;
      return Last_Index;
   end Add_Child;

   procedure Clear (Instance : in out Header_Type) is
   begin
      Instance.Breadcrumb_Content.Clear;
      Instance.App_Navigation_Parent.Inner_HTML ("");
   end Clear;

   procedure Set_Menu
     (Instance  : in out Header_Type;
      Unique_ID :        Integer)
   is
   begin
      Menu_Table (Unique_ID).On_Open (Instance.Parent.all);
   end Set_Menu;

   procedure Set_App_Icon
     (Instance : in out Header_Type;
      Icon_SRC :        String)
   is
   begin
      Instance.App_Icon.URL_Source (Icon_SRC);
   end Set_App_Icon;

   procedure Notify_Menu_Click
     (Instance  : in out Header_Type;
      Unique_ID :        Integer)
   is
      Data : constant Data_Type := Menu_Table (Unique_ID);
   begin
      Instance.Breadcrumb_Content.Update (Data.On_Open, Data.Name, Data.Depth);
      if Data.Is_Leaf then
         Instance.Close_Menu;
      else
         Instance.Update (Unique_ID);
      end if;
   end Notify_Menu_Click;

   -----------------------------------------------------------------------------
   --  User Menu
   -----------------------------------------------------------------------------
   procedure Open_User_Menu (Instance : in out Header_Type) is
   begin
      Instance.User_Icon.Add_Class ("header-icon-active");
      Instance.User_Content.Display;
      Instance.User_Is_Open := True;
   end Open_User_Menu;

   procedure Close_User_Menu (Instance : in out Header_Type) is
   begin
      Instance.User_Content.Clear;
      Instance.User_Icon.Remove_Class ("header-icon-active");
      Instance.User_Is_Open := False;
   end Close_User_Menu;

   function Is_User_Menu_Open
     (Instance : in out Header_Type)
      return Boolean
   is
   begin
      return Instance.User_Is_Open;
   end Is_User_Menu_Open;

   procedure Add_Element
     (Instance : in out Header_Type;
      Name     :        String;
      On_Click :        Base.Action_Event)
   is
   begin
      Instance.User_Content.Add_Element (Name, On_Click);
   end Add_Element;

   procedure Set_User_Name
     (Instance  : in out Header_Type;
      User_Name :        String)
   is
   begin
      Instance.User_Name.Inner_HTML (User_Name);
   end Set_User_Name;

   procedure Set_User_Icon
     (Instance : in out Header_Type;
      Icon_SRC :        String)
   is
   begin
      Instance.User_Icon.URL_Source (Icon_SRC);
   end Set_User_Icon;

   procedure Notify_User_Menu_Click (Instance : in out Header_Type) is
   begin
      Instance.Close_User_Menu;
   end Notify_User_Menu_Click;

-------------------------------------------------------------------------------
end v22.Gui.Header;
-------------------------------------------------------------------------------
