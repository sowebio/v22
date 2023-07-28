with Gnoga.Types;
with Gnoga.Gui.Plugin;
with Gnoga.Gui.Window;
with Gnoga.Application.Multi_Connect;

with UXStrings.Hash;

with Ada.Containers.Hashed_Maps;

with Header;
with CRUD;
with Footer;

package body Framework is

   use all type Gnoga.String;

   package Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => UXString, Element_Type => Integer, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   ID_Main           : Integer;
   On_Custom_Connect : Base.Action_Event;

   Header_Dict : Dictionary.Map;

   Browse_Icon_SRC : UXString := "";
   User_Icon_SRC   : UXString := "";

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Window    : Gnoga.Gui.Window.Pointer_To_Window_Class;
      Container : View.View_Type;

      Header_Content : Header.Header_Type;

      Header_Parent : View.View_Type;
      CRUD_Parent   : View.View_Type;
      Content       : View.View_Type;
      Footer_Parent : View.View_Type;

      Content_Header : View.View_Type;
      Content_Text   : aliased View.View_Type;

      Footer_Content : Footer.Footer_Type;

      Crud_Dict   : Dictionary.Map;
      Header_Dict : Dictionary.Map;
      User_Dict   : Dictionary.Map;

      CRUD_Instance : CRUD.CRUD_Type;
   end record;
   type App_Access is access all App_Data;

   procedure Print (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Window.Print;
   end Print;

   procedure CRUD_Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Value  :        Integer)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Dict.Insert (Key, Value);
   end CRUD_Set;

   function CRUD_Get
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
      return Integer
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Crud_Dict.Element (Key);
   end CRUD_Get;

   procedure Header_Set
     (Key   : UXString;
      Value : Integer)
   is
   begin
      Header_Dict.Insert (Key, Value);
   end Header_Set;

   function Header_Get
     (Key : UXString)
      return Integer
   is
   begin
      return Header_Dict.Element (Key);
   end Header_Get;

   -----------------------------------------------------------------------------
   --  CRUD Handlers
   -----------------------------------------------------------------------------

   procedure On_CRUD_Callback (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Element_Click (Object);
   end On_CRUD_Callback;

   procedure On_Key_Pressed
     (Object : in out Base.Base_Type'Class;
      Char   :        Character)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Key_Pressed (Char);
   end On_Key_Pressed;

   -----------------------------------------------------------------------------
   --  Menu Handlers
   -----------------------------------------------------------------------------

   procedure On_Logo (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.Header_Content.Is_Menu_Open then
         App.Header_Content.Close_Menu;
      else
         App.CRUD_Instance.Clear; --  The reason why this function is here
         App.Header_Content.Open_Menu (ID_Main);
         App.Header_Content.Close_User_Menu;
      end if;
   end On_Logo;

   procedure On_User (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.Header_Content.Is_User_Menu_Open then
         App.Header_Content.Close_User_Menu;
      else
         App.Header_Content.Open_User_Menu;
         App.Header_Content.Close_Menu;
      end if;
   end On_User;

   -----------------------------------------------------------------------------
   --  Tool Bar expand button
   -----------------------------------------------------------------------------

   procedure On_Tool_Bar_Expand (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Resize;
   end On_Tool_Bar_Expand;

   -----------------------------------------------------------------------------
   --  On_Connect
   -----------------------------------------------------------------------------

   procedure On_Connect
     (Screen     : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Screen.Connection_Data (App);
      Screen.Buffer_Connection (True);
      App.Window := Screen'Unchecked_Access;
      App.Window.On_Character_Handler (On_Key_Pressed'Unrestricted_Access);
      App.Container.Create (Screen);

      --  Containers
      App.Container.Style ("width", "100%");
      App.Container.Style ("height", "100%");

      App.Header_Parent.Create (App.Container);
      App.Header_Parent.Class_Name ("header");

      App.Content.Create (App.Container);
      App.Content.Class_Name ("content-container");

      App.CRUD_Parent.Create (App.Container);
      App.CRUD_Parent.Class_Name ("crud");

      App.Footer_Parent.Create (App.Container);
      App.Footer_Parent.Class_Name ("footer");

      --  Content
      App.Content_Header.Create (App.Content);
      App.Content_Header.Class_Name ("content-header");
      App.Content_Text.Create (App.Content);

      --  Header
      App.Header_Content.Create (App.Header_Parent, On_Logo'Unrestricted_Access, On_User'Unrestricted_Access);
      App.Header_Content.Set_App_Icon (Browse_Icon_SRC);
      App.Header_Content.Set_User_Icon (User_Icon_SRC);

      --  Footer
      App.Footer_Content.Create (App.Footer_Parent);

      --  CRUD
      App.CRUD_Instance.Create
        (App.CRUD_Parent, On_Tool_Bar_Expand'Unrestricted_Access, On_CRUD_Callback'Unrestricted_Access);

      App.Header_Content.Set_Menu (ID_Main); --  Needs to be called when everything is created

      On_Custom_Connect (App.Container);
   end On_Connect;

   procedure Setup
     (On_User_Connect       : Base.Action_Event;
      Title                 : UXString;
      Server_Closed_Content : UXString)
   is
   begin
      Gnoga.Application.Title (Title);
      Gnoga.Application.HTML_On_Close (Server_Closed_Content);
      Gnoga.Application.Multi_Connect.Initialize (Boot => "boot_jqueryui.html");
      Gnoga.Application.Multi_Connect.On_Connect_Handler (On_Connect'Unrestricted_Access);
      On_Custom_Connect := On_User_Connect;
   end Setup;

   procedure Set_App_Title (Title : UXString) is
   begin
      Gnoga.Application.Title (Title);
   end Set_App_Title;

   procedure Set_App_Icon (Icon_SRC : UXString) is
   begin
      Gnoga.Application.Favicon (Icon_SRC);
   end Set_App_Icon;

   procedure Set_Browse_Icon (Icon_SRC : UXString) is
   begin
      Browse_Icon_SRC := Icon_SRC;
   end Set_Browse_Icon;

   procedure Set_User_Icon (Icon_SRC : UXString) is
   begin
      User_Icon_SRC := Icon_SRC;
   end Set_User_Icon;

   procedure Set_User_Name
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Set_User_Name (Name);
   end Set_User_Name;

   function Content_Parent
     (Object : in out Base.Base_Type'Class)
      return View.View_Access
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Content_Text'Access;
   end Content_Parent;

   procedure Clear_Content (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Text.Inner_HTML ("");
   end Clear_Content;

   -----------------------------------------------------------------------------
   --  Header
   -----------------------------------------------------------------------------

   procedure Header_Set_Root
     (Key      : UXString;
      Name     : UXString;
      On_Click : Base.Action_Event)
   is
      Unique_ID : Integer;
   begin
      Unique_ID := Header.Set_Root (Name, On_Click);
      Header_Set (Key, Unique_ID);
      ID_Main := Unique_ID;
   end Header_Set_Root;

   procedure Header_Add_Child
     (Key        : UXString;
      Name       : UXString;
      Parent_Key : UXString;
      On_Click   : Base.Action_Event)
   is
      Unique_ID : Integer;
   begin
      Unique_ID := Header.Add_Child (Header_Get (Parent_Key), Name, On_Click);
      Header_Set (Key, Unique_ID);
   end Header_Add_Child;

   procedure Header_Add_Dialog
     (Title        : UXString;
      Content      : UXString          := "";
      Confirm_Text : UXString          := "";
      Cancel_Text  : UXString          := "";
      On_Confirm   : Base.Action_Event := null;
      On_Cancel    : Base.Action_Event := null)
   is
   begin
      Header.Add_Dialog (Title, Content, Confirm_Text, Cancel_Text, On_Confirm, On_Cancel);
   end Header_Add_Dialog;

   procedure Header_Add_Web
     (Title : UXString;
      URL   : UXString)
   is
   begin
      Header.Add_Web (Title, URL);
   end Header_Add_Web;

   procedure Header_Add_Button
     (Title    : UXString;
      On_Click : Base.Action_Event)
   is
   begin
      Header.Add_Button (Title, On_Click);
   end Header_Add_Button;

   -----------------
   --  Callbacks  --
   -----------------

   procedure Header_Notify_Menu_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (Header_Get (Key));
      App.Content_Header.Inner_HTML ("");
      App.Content_Text.Inner_HTML ("");
      App.CRUD_Instance.Clear;
      App.Crud_Dict.Clear;
   end Header_Notify_Menu_Click;

   -----------------------------------------------------------------------------
   --  CRUD
   -----------------------------------------------------------------------------

   procedure CRUD_Load (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Load;
   end CRUD_Load;

   procedure CRUD_Add_Element
     (Object   : in out Base.Base_Type'Class;
      Key      :        UXString;
      Name     :        UXString;
      Icon_SRC :        UXString)
   is
      App       : constant App_Access := App_Access (Object.Connection_Data);
      Unique_ID : Integer;
   begin
      Unique_ID := App.CRUD_Instance.Add_Element (Name, Icon_SRC);
      CRUD_Set (Object, Key, Unique_ID);
   end CRUD_Add_Element;

   procedure CRUD_Add_Sub_Element
     (Object     : in out Base.Base_Type'Class;
      Key        :        UXString;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Click   :        Base.Action_Event)
   is
      App       : constant App_Access := App_Access (Object.Connection_Data);
      Unique_ID : Integer;
   begin
      Unique_ID := App.CRUD_Instance.Add_Sub_Element (Name, CRUD_Get (Object, Parent_Key), On_Click);
      CRUD_Set (Object, Key, Unique_ID);
   end CRUD_Add_Sub_Element;

   procedure CRUD_Add_Delimiter_Above
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Add_Delimiter_Above (CRUD_Get (Object, Key));
   end CRUD_Add_Delimiter_Above;

   procedure CRUD_Set_Unclickable
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Set_Unclickable (CRUD_Get (Object, Key));
   end CRUD_Set_Unclickable;

   procedure CRUD_Set_Clickable
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Set_Clickable (CRUD_Get (Object, Key));
   end CRUD_Set_Clickable;

   procedure CRUD_Notify_Sub_Element_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Sub_Element_Click (CRUD_Get (Object, Key));
   end CRUD_Notify_Sub_Element_Click;

   procedure CRUD_Enable_Shortcuts (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      CRUD.Enable_Shortcuts (App.CRUD_Instance);
   end CRUD_Enable_Shortcuts;

   procedure CRUD_Disable_Shortcuts (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      CRUD.Disable_Shortcuts (App.CRUD_Instance);
   end CRUD_Disable_Shortcuts;

   -----------------------------------------------------------------------------
   --  Content
   -----------------------------------------------------------------------------

   procedure Content_Set_Title
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Header.Text (Title);
   end Content_Set_Title;

   procedure Content_Clear_Title (Object : in out Base.Base_Type'Class) is
   begin
      Content_Set_Title (Object, "");
   end Content_Clear_Title;

   procedure Content_Set_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Text.Text (Text);
   end Content_Set_Text;

   procedure Content_Clear_Text (Object : in out Base.Base_Type'Class) is
   begin
      Content_Set_Text (Object, "");
   end Content_Clear_Text;

   -----------------------------------------------------------------------------
   --  Footer
   -----------------------------------------------------------------------------

   procedure Footer_Set_State_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "")
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Footer_Content.Set_State_Text (Text);
   end Footer_Set_State_Text;

   procedure Footer_Set_Permanent_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "")
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Footer_Content.Set_Permanent_Text (Text);
   end Footer_Set_Permanent_Text;

   -----------------------------------------------------------------------------
   --  User relative data
   -----------------------------------------------------------------------------

   procedure Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Value  :        Integer)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.User_Dict.Insert (Key, Value);
   end Set;

   function Get
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
      return Integer
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.User_Dict.Element (Key);
   end Get;

   -----------------------------------------------------------------------------
end Framework;
