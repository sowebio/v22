with User_Menu;

package body Header is

   package Element renames Gnoga.Gui.Element;

   Header_Error : exception;

   Root_Parent_ID : constant Integer := -1;
   Root_Depth     : constant Integer := 0;

   type Data_Type is record
      Parent_ID : Integer  := Root_Parent_ID;
      Name      : UXString := "";
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
      return UXString
   is
   begin
      return From_UTF_8 (Value'Image).Delete (1, 1);
   end To_UXString;

   function Button_Name
     (Index : Integer)
      return UXString
   is
   begin
      return "Menu_" & To_UXString (Index);
   end Button_Name;

   procedure Remove_Button
     (Instance : in out Header_Type;
      Data_ID  :        Integer)
   is
   begin
      if Instance.App_Browse_Parent.Element (Button_Name (Data_ID)) /= null then
         Instance.App_Browse_Parent.Element (Button_Name (Data_ID)).Remove;
      end if;
   end Remove_Button;

   procedure Update
     (Instance  : in out Header_Type;
      Parent_ID :        Integer)
   is
   begin
      for Data_ID in Menu_Table'Range loop
         Instance.Remove_Button (Data_ID);
         if Menu_Table (Data_ID).Parent_ID = Parent_ID then
            declare
               Data   : constant Data_Type                                  := Menu_Table (Data_ID);
               Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
            begin
               Common.Button_Access (Button).Create (Instance.App_Browse_Parent.all, Data.Name);
               Button.Class_Name ("framework-button");
               Button.Dynamic;
               Instance.App_Browse_Parent.Add_Element (Button_Name (Data_ID), Button);
               Button.On_Click_Handler (Data.On_Open);
            end;
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
      App_Parent        : constant View.Pointer_To_View_Class       := new View.View_Type;
      App_Icon          : constant Element.Pointer_To_Element_Class := new Common.IMG_Type;
      App_Browse_Parent : constant View.Pointer_To_View_Class       := new View.View_Type;

      Breadcrumb_Parent : constant View.Pointer_To_View_Class := new View.View_Type;

      User_Parent        : constant View.Pointer_To_View_Class       := new View.View_Type;
      User_Name_Parent   : constant Element.Pointer_To_Element_Class := new Common.P_Type;
      User_Icon          : constant Element.Pointer_To_Element_Class := new Common.IMG_Type;
      User_Browse_Parent : constant View.Pointer_To_View_Class       := new View.View_Type;
   begin
      Instance.Parent := Parent'Unrestricted_Access;

      --  App icon & browse menu
      Instance.App_Parent := View.View_Access (App_Parent);
      Instance.App_Parent.Create (Parent);
      Instance.App_Parent.Class_Name ("header-app-parent");

      Instance.App_Icon := Common.IMG_Access (App_Icon);
      Instance.App_Icon.Create (Instance.App_Parent.all);
      Instance.App_Icon.Class_Name ("header-icon");
      Instance.App_Icon.On_Click_Handler (On_Logo);

      Instance.App_Browse_Parent := View.View_Access (App_Browse_Parent);
      Instance.App_Browse_Parent.Create (Instance.App_Parent.all);
      Instance.App_Browse_Parent.Class_Name ("header-app-browse-parent");

      --  Breadcrumb
      Instance.Breadcrumb_Parent := View.View_Access (Breadcrumb_Parent);
      Instance.Breadcrumb_Parent.Create (Parent);
      Instance.Breadcrumb_Parent.Class_Name ("header-breadcrumb-parent");
      Instance.Breadcrumb_Content.Create (Instance.Breadcrumb_Parent.all);
      --  Instance.Breadcrumb_Content.Update (Root.On_Open, Root.Name);

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

      Instance.User_Browse_Parent := View.View_Access (User_Browse_Parent);
      Instance.User_Browse_Parent.Create (Instance.User_Parent.all);
      Instance.User_Browse_Parent.Class_Name ("header-user-browse-parent");

      --  User_Menu.Display (Instance.User_Browse_Parent.all);
      Instance.App_Browse_Parent.Display ("none");
   end Create;

   procedure Clear (Instance : in out Header_Type) is
   begin
      Instance.Breadcrumb_Content.Clear;
   end Clear;

   function Set_Root
     (Name    : UXString;
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
      Last_Index := Last_Index + 1;
      Root.Name               := Name;
      Root.On_Open            := On_Open;
      Menu_Table (Last_Index) := Root;
      return Last_Index;
   end Set_Root;

   function Add_Child
     (Parent_ID : Integer;
      Name      : UXString;
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
      Last_Index := Last_Index + 1;
      Menu_Table (Parent_ID).Is_Leaf := False;
      Child.Parent_ID                := Parent_ID;
      Child.Name                     := Name;
      Child.Depth                    := Menu_Table (Parent_ID).Depth + 1;
      Child.On_Open                  := On_Open;
      Menu_Table (Last_Index)        := Child;
      return Last_Index;
   end Add_Child;

   -----------------------------------------------------------------------------
   --  Menu relative functions
   -----------------------------------------------------------------------------

   procedure Open_Menu
     (Instance  : in out Header_Type;
      Unique_ID :        Integer)
   is
   begin
      Instance.Set_Menu (Unique_ID);
      Instance.App_Browse_Parent.Display ("block");
      Update (Instance, Unique_ID);
      Instance.App_Icon.Add_Class ("header-icon-active");
      Instance.App_Is_Open := True;
   end Open_Menu;

   procedure Close_Menu (Instance : in out Header_Type) is
   begin
      Instance.App_Browse_Parent.Display ("none");
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

   procedure Open_User_Menu (Instance : in out Header_Type) is
   begin
      Instance.User_Icon.Add_Class ("header-icon-active");
      User_Menu.Display (Instance.User_Browse_Parent.all);
      Instance.User_Is_Open := True;
   end Open_User_Menu;

   procedure Close_User_Menu (Instance : in out Header_Type) is
   begin
      Instance.User_Browse_Parent.Inner_HTML ("");
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

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------

   procedure Set_Menu
     (Instance  : in out Header_Type;
      Unique_ID :        Integer)
   is
   begin
      Menu_Table (Unique_ID).On_Open (Instance.Parent.all);
   end Set_Menu;

   procedure Set_User_Name
     (Instance  : in out Header_Type;
      User_Name :        UXString)
   is
   begin
      Instance.User_Name.Inner_HTML (User_Name);
   end Set_User_Name;

   procedure Set_App_Icon
     (Instance : in out Header_Type;
      Icon_SRC :        UXString)
   is
   begin
      Instance.App_Icon.URL_Source (Icon_SRC);
   end Set_App_Icon;

   procedure Set_User_Icon
     (Instance : in out Header_Type;
      Icon_SRC :        UXString)
   is
   begin
      Instance.User_Icon.URL_Source (Icon_SRC);
   end Set_User_Icon;

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

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
   --  User menu
   -----------------------------------------------------------------------------

   procedure Add_Dialog
     (Title           : UXString;
      Content         : UXString          := "";
      Confirm_Text    : UXString          := "";
      Cancel_Text     : UXString          := "";
      Confirm_Handler : Base.Action_Event := null;
      Cancel_Handler  : Base.Action_Event := null)
   is
   begin
      User_Menu.Add_Dialog (Title, Content, Confirm_Text, Cancel_Text, Confirm_Handler, Cancel_Handler);
   end Add_Dialog;

   procedure Add_Web
     (Title : UXString;
      URL   : UXString)
   is
   begin
      User_Menu.Add_Web (Title, URL);
   end Add_Web;

   procedure Add_Button (Title : UXString; On_Click : Base.Action_Event)
   is
   begin
      User_Menu.Add_Button (Title, On_Click);
   end Add_Button;

end Header;
