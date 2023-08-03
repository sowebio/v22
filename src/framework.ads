with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

with UXStrings.Hash;

package Framework is

   package Base renames Gnoga.Gui.Base;
   package View renames Gnoga.Gui.View;

   Register_Group_Key : constant UXString := "Créer un compte";

   procedure Print (Object : in out Base.Base_Type'Class);
   --  Call system print on current web view

   procedure Setup
     (On_User_Connect       : Base.Action_Event;
      Title                 : UXString := "";
      Server_Closed_Content : UXString := "Server closed.");
   --  Set connection elements
   --  On_User_Connect is called every time a user launches the webpage

   procedure Setup_Access
     (On_User_Login           : Base.Action_Event := null;
      On_User_Register        : Base.Action_Event := null;
      On_User_Register_Create : Base.Action_Event := null);
   --  Set the webpage accessible only by logging in
   --  On_User_Register_Create allows to customize the register form,
   --  using Register_Group_Key

   procedure Add_Root_User;
   --  Add default user
   --  email : root@root
   --  password : password
   --  User name : Root User

   procedure Disconnect_User (Object : in out Base.Base_Type'Class);
   --  This function assumes Setup_Access was called

   procedure Set_App_Title
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString);
      --  Set website title (in tab)

   procedure Set_App_Icon (Icon_SRC : UXString);
   --  Should theorically work but GNOGA refuses to update the icon

   procedure Set_Browse_Icon (Icon_SRC : UXString);
   --  Set the icon which displays (on click) the menu

   procedure Set_Default_User_Icon (Icon_SRC : UXString);
   --  Set the default user icon which (on click) displays the user menu,
   --  can be overwritten with Set_User_Icon

   procedure Set_User_Icon
     (Object   : in out Base.Base_Type'Class;
      Icon_SRC :        UXString);
      --  Set the user icon which (on click) displays the user menu

   procedure Set_User_Name
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString);
      --  Set user name, displayed next to the user icon

      -----------------------------------------------------------------------------
      --  Header
      -----------------------------------------------------------------------------

   procedure Header_Set_Root
     (Key      : UXString;
      Name     : UXString;
      On_Click : Base.Action_Event);
      --  Set default menu (root of browsing menu)

   procedure Header_Add_Child
     (Key        : UXString;
      Name       : UXString;
      Parent_Key : UXString;
      On_Click   : Base.Action_Event);
      --  Add child to a child or root in menu

   procedure Header_Add_Dialog
     (Title        : UXString;
      Content      : UXString          := "";
      Confirm_Text : UXString          := "";
      Cancel_Text  : UXString          := "";
      On_Confirm   : Base.Action_Event := null;
      On_Cancel    : Base.Action_Event := null);
      --  Function to create a button on the user menu, creating a jQuery dialog
      --  Two default buttons are available : Cancel and Confirm
      --  On_Cancel is fired on default exit
      --  Buttons are not displayed on dialog if corresponding handler is null

   procedure Header_Add_Web
     (Title : UXString;
      URL   : UXString);
      --  Function to create a button on the user menu, which opens a new web page

   procedure Header_Add_Button
     (Title    : UXString;
      On_Click : Base.Action_Event);
      --  Function to create a button with customized click handler

      -----------------
      --  Callbacks  --
      -----------------

   procedure Header_Notify_Menu_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

      -----------------------------------------------------------------------------
      --  CRUD
      -----------------------------------------------------------------------------

   procedure CRUD_Load (Object : in out Base.Base_Type'Class);

   procedure CRUD_Add_Element
     (Object   : in out Base.Base_Type'Class;
      Key      :        UXString;
      Name     :        UXString;
      Icon_SRC :        UXString);

   procedure CRUD_Add_Sub_Element
     (Object     : in out Base.Base_Type'Class;
      Key        :        UXString;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Click   :        Base.Action_Event);

   procedure CRUD_Add_Delimiter_Above
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

   procedure CRUD_Set_Unclickable
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

   procedure CRUD_Set_Clickable
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

   procedure CRUD_Notify_Sub_Element_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

   procedure CRUD_Enable_Shortcuts (Object : in out Base.Base_Type'Class);

   procedure CRUD_Disable_Shortcuts (Object : in out Base.Base_Type'Class);

   -----------------------------------------------------------------------------
   --  Content
   -----------------------------------------------------------------------------

   function Content_Parent
     (Object : in out Base.Base_Type'Class)
      return View.View_Access;

   procedure Content_Clear (Object : in out Base.Base_Type'Class);
   --  Removes any content inside content parent

   procedure Content_Set_Title
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString);

   procedure Content_Clear_Title (Object : in out Base.Base_Type'Class);

   procedure Content_Set_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString);

   procedure Content_Clear_Text (Object : in out Base.Base_Type'Class);

   --------------
   --  Groups  --
   --------------
   procedure Content_Group_Create
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString);

   procedure Content_Group_Add_Title
     (Object     : in out Base.Base_Type'Class;
      Title      :        UXString;
      Parent_Key :        UXString);

   procedure Content_Group_Add_Space
     (Object     : in out Base.Base_Type'Class;
      Parent_Key :        UXString;
      Height     :        Integer := 8);

   procedure Content_Group_Item_Lock
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString);

   procedure Content_Group_Item_Unlock
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString);

   procedure Content_Group_Item_Place_Holder
     (Object       : in out Base.Base_Type'Class;
      Name         :        UXString;
      Place_Holder :        UXString);

      -----------------
      --  Edit Text  --
      -----------------
   procedure Content_Group_Text_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Text_Set
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString;
      Text   :        UXString);

   function Content_Group_Text_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString;

      -----------------
      --  Text Area  --
      -----------------
   procedure Content_Group_Text_Area_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Text_Area_Set
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString;
      Text   :        UXString);

   function Content_Group_Text_Area_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString;

      -----------------
      --  Check Box  --
      -----------------
   procedure Content_Group_Check_Box_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Check_Box_Checked
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Is_Checked :        Boolean);

   function Content_Group_Check_Box_Is_Checked
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return Boolean;

      --------------
      --  Number  --
      --------------
   procedure Content_Group_Number_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Number_Set
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString;
      Value  :        Integer);

   function Content_Group_Number_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return Integer;

      -----------------
      --  Selection  --
      -----------------
   procedure Content_Group_Selection_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Selection_Add_Option
     (Object  : in out Base.Base_Type'Class;
      Name    :        UXString;
      Option  :        UXString;
      Enabled :        Boolean := False);

   function Content_Group_Selection_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString;

      ------------
      --  Date  --
      ------------
   procedure Content_Group_Date_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   function Content_Group_Date_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString;

      -------------
      --  Email  --
      -------------
   procedure Content_Group_Email_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   function Content_Group_Email_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString;

      ----------------
      --  Password  --
      ----------------
   procedure Content_Group_Password_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   function Content_Group_Password_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString;

      -------------
      --  Phone  --
      -------------
   procedure Content_Group_Phone_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null);

   function Content_Group_Phone_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString;

      ---------------
      --  Warning  --
      ---------------
   procedure Content_Group_Warning_Add
     (Object     : in out Base.Base_Type'Class;
      Text       :        UXString;
      Key        :        UXString;
      Parent_Key :        UXString);

   procedure Content_Group_Warning_Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Text   :        UXString);

      -----------------------------------------------------------------------------
      --  Footer
      -----------------------------------------------------------------------------

   procedure Footer_Set_State_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "");

   procedure Footer_Set_Permanent_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "");

      -----------------------------------------------------------------------------
      --  User relative data
      -----------------------------------------------------------------------------

   procedure Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Value  :        UXString);

   function Get
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
      return UXString;

private

   procedure Setup_Login_Form (Object : in out Base.Base_Type'Class);
   procedure Setup_Login_Buttons (Object : in out Base.Base_Type'Class);
   procedure Set_Login_Error_Message
     (Object : in out Base.Base_Type'Class;
      Error  :        UXString);

   procedure Setup_Register_Form (Object : in out Base.Base_Type'Class);
   procedure Setup_Register_Buttons (Object : in out Base.Base_Type'Class);
   procedure Set_Register_Error_Message
     (Object : in out Base.Base_Type'Class;
      Error  :        UXString);

end Framework;
