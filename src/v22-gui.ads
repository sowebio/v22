-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-gui.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface
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

with Gnoga.Gui.Base;
with Gnoga.Gui.View;

package v22.Gui is

   package Base renames Gnoga.Gui.Base;
   package View renames Gnoga.Gui.View;

   Register_Group_Key : constant String := "Créer un compte";

   -----------------------------------------------------------------------------
   --  Setup
   -----------------------------------------------------------------------------
   procedure Setup
     (On_User_Connect       : Base.Action_Event;
      Title                 : String := "";
      Server_Closed_Content : String := "Server closed.");
   --  Set connection elements.
   --  On_User_Connect is called every time a user launches the webpage.

   procedure Set_App_Title
     (Object : in out Base.Base_Type'Class;
      Title  :        String);
   --  Set website title (in tab).

   procedure Set_App_Icon (Icon_SRC : String);
   --  Should theorically work but GNOGA refuses to update the icon.

   procedure Set_Default_User_Icon (Icon_SRC :  String);
   --  Set the default user icon which (on click) displays the user menu,
   --  can be overwritten with Set_User_Icon.

   procedure Set_Navigation_Icon (Icon_SRC :  String);
   --  Set the icon which displays (on click) the navigation menu.

   -----------------------------------------------------------------------------
   --  User connection-relative data
   -----------------------------------------------------------------------------
   procedure Set_Data
     (Object : in out Base.Base_Type'Class;
      Key    :         String;
      Value  :         String);
   --  Set connection data in a  String -  String dictionary.

   function Get_Data
     (Object : in out Base.Base_Type'Class;
      Key    :         String)
      return  String;
   --  Get connection data in a  String -  String dictionary.

   procedure Clear_Data (Object : in out Base.Base_Type'Class);
   --  Clear connection data (could be used when user logouts).

   procedure Set_User_Icon
     (Object   : in out Base.Base_Type'Class;
      Icon_SRC :         String);
   --  Set the user icon which (on click) displays the navigation user menu.

   procedure Set_User_Name
     (Object : in out Base.Base_Type'Class;
      Name   :         String);
   --  Set user name, displayed next to the user icon.

   -----------------------------------------------------------------------------
   --  Security
   -----------------------------------------------------------------------------
   type Register_Function is access function
     (Object : in out Base.Base_Type'Class;
      Email  :         String)
      return Boolean;

   procedure Setup_Access
     (On_User_Login           : Base.Action_Event := null;
      On_User_Register_Create : Base.Action_Event := null;
      On_User_Register        : Register_Function := null);
   --  Set the webpage accessible only by logging in
   --  On_User_Register_Create allows to customize the register form,
   --  using Register_Group_Key, and return a boolean to accept or not form

   procedure Set_Register_Error_Message
     (Object : in out Base.Base_Type'Class;
      Error  :         String);
   --  Set an error on register form.

   procedure Add_User (Email, Password :  String);
   --  Add user with Email and clear password.

   procedure Delete_User (Email :  String);
   --  Delete user from known identities.

   procedure Disconnect_User (Object : in out Base.Base_Type'Class);
   --  This function assumes Setup_Access was called

   function Get_User_Email
     (Object : in out Base.Base_Type'Class)
      return  String;
   --  Return current user email.

   -----------------------------------------------------------------------------
   --  Header
   -----------------------------------------------------------------------------
   procedure Header_Set_Root
     (Key      :  String;
      Name     :  String;
      On_Click : Base.Action_Event);
   --  Set default menu (root of navigation menu).
   --  On_Click handler needs to call Header_Notify_Menu_Click.
   --  IMPORTANT: Key must be UNIQUE /!\

   procedure Header_Add_Child
     (Key        :  String;
      Name       :  String;
      Parent_Key :  String;
      On_Click   : Base.Action_Event);
   --  Add child to a child or root in menu.
   --  On_Click handler needs to call Header_Notify_Menu_Click.
   --  IMPORTANT: Key must be UNIQUE /!\

   procedure Header_Add_User_Button
     (Object   : in out Base.Base_Type'Class;
      Name     :         String;
      On_Click :        Base.Action_Event);
   --  On_Click handler needs to call Header_Notify_User_Menu_Click otherwise
   --  user navigation menu can't be closed.

   -----------------
   --  Callbacks  --
   -----------------
   procedure Header_Notify_Menu_Click
     (Object : in out Base.Base_Type'Class;
      Key    :         String);

   procedure Header_Notify_User_Menu_Click (Object : in out Base.Base_Type'Class);

   -----------------------------------------------------------------------------
   --  CRUD
   -----------------------------------------------------------------------------
   procedure CRUD_Load (Object : in out Base.Base_Type'Class);

   procedure CRUD_Add_Element
     (Object   : in out Base.Base_Type'Class;
      Key      :         String;
      Name     :         String;
      Icon_SRC :         String);
   --  IMPORTANT: Key must be UNIQUE /!\

   procedure CRUD_Add_Sub_Element
     (Object     : in out Base.Base_Type'Class;
      Key        :         String;
      Name       :         String;
      Parent_Key :         String;
      On_Click   :        Base.Action_Event);
   --  IMPORTANT: Key must be UNIQUE /!\
   --  If parent is "File", and this element is "Edit", then a unique key
   --  could be set as "File_Edit".

   procedure CRUD_Add_Delimiter_Above
     (Object : in out Base.Base_Type'Class;
      Key    :         String);
   --  Add a delimiter above a sub-element.

   procedure CRUD_Set_Unclickable
     (Object : in out Base.Base_Type'Class;
      Key    :         String);
   --  Set an element (or sub-element) unclickable.

   procedure CRUD_Set_Clickable
     (Object : in out Base.Base_Type'Class;
      Key    :         String);
   --  Set an element (or sub-element) clickable.

   procedure CRUD_Notify_Sub_Element_Click
     (Object : in out Base.Base_Type'Class;
      Key    :         String);
   --  Callback to place in sub-elements' handlers.

   procedure CRUD_Enable_Shortcuts (Object : in out Base.Base_Type'Class);
   --  Enable CRUD shortcuts, allowing both elements and sub-elements shortcuts.

   procedure CRUD_Disable_Shortcuts (Object : in out Base.Base_Type'Class);
   --  Disable CRUD shortcuts, for both elements and sub-elements.

   -----------------------------------------------------------------------------
   --  Content
   -----------------------------------------------------------------------------
   function Content_Parent
     (Object : in out Base.Base_Type'Class)
      return View.View_Access;
   --  Return the element below menu title, containing all content.

   procedure Content_Clear (Object : in out Base.Base_Type'Class);
   --  Removes any HTML content inside content parent.

   procedure Content_Set_Title
     (Object : in out Base.Base_Type'Class;
      Title  :         String);
   --  Set title above content.

   procedure Content_Clear_Title (Object : in out Base.Base_Type'Class);
   --  Set title above content to "".

   procedure Content_Set_Text
     (Object : in out Base.Base_Type'Class;
      Text   :         String);
   --  Set text in content parent.
   --  NOTE: should be used when the only content of the menu is this text,
   --  otherwise, should use Content_Parent instead.

   procedure Content_Clear_Text (Object : in out Base.Base_Type'Class);
   --  Set text in content parent to "".
   --  NOTE: should be used when the only content of the menu is this text.
   --  otherwise, should use Content_Parent instead.

   -----------------------------------------------------------------------------
   --  Groups
   -----------------------------------------------------------------------------
   procedure Content_Group_Create
     (Object : in out Base.Base_Type'Class;
      Title  :         String);
   --  Create a container for forms. This element is displayed in the
   --  content container. Does not need any clearing apart from Content_Clear
   --  to delete this element.

   procedure Content_Group_Add_Title
     (Object     : in out Base.Base_Type'Class;
      Title      :         String;
      Parent_Key :         String);
   --  Add a title to a form group.
   --  NOTE: Title from arguments of Content_Group_Create is already on screen
   --  and has a higher emphasis.

   procedure Content_Group_Add_Space
     (Object     : in out Base.Base_Type'Class;
      Parent_Key :         String;
      Height     :        Integer := 8);
   --  Add space between rows in form group.

   procedure Content_Group_Item_Lock
     (Object : in out Base.Base_Type'Class;
      Name   :         String);
   --  Lock a form in a form group so the user can't write anything.

   procedure Content_Group_Item_Unlock
     (Object : in out Base.Base_Type'Class;
      Name   :         String);
   --  Unlock a form in a form group.

   procedure Content_Group_Item_Place_Holder
     (Object       : in out Base.Base_Type'Class;
      Name         :         String;
      Place_Holder :         String);
   --  Set a place-holder for field-type forms.

   procedure Content_Group_Add_Button
     (Object     : in out Base.Base_Type'Class;
      Text       :         String;
      On_Click   :        Base.Action_Event;
      Parent_Key :         String);
   --  Add a button with a callback. Can be use as a submit button.

   ----------------------
   --  Group Elements  --
   ----------------------
   --  Most element come with an On_Change callback, fired once the user stops
   --  focusing the given element.
   --  Every item (except button and warning), have an explaining text next to
   --  it in the container.

   -----------------
   --  Edit Text  --
   -----------------
   --  This element allows to enter and modify text.

   procedure Content_Group_Text_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Text_Set
     (Object : in out Base.Base_Type'Class;
      Name   :         String;
      Text   :         String);

   function Content_Group_Text_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return  String;

   -----------------
   --  Text Area  --
   -----------------
   --  Same as Edit Text, but the user can define the height of the view.

   procedure Content_Group_Text_Area_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Text_Area_Set
     (Object : in out Base.Base_Type'Class;
      Name   :         String;
      Text   :         String);

   function Content_Group_Text_Area_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return  String;

   -----------------
   --  Check Box  --
   -----------------
   --  This element can be checked and unchecked by the user.

   procedure Content_Group_Check_Box_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Check_Box_Checked
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Is_Checked :        Boolean);

   function Content_Group_Check_Box_Is_Checked
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return Boolean;

   --------------
   --  Number  --
   --------------
   --  Same as Edit Text, but only numbers are allowed.

   procedure Content_Group_Number_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Number_Set
     (Object : in out Base.Base_Type'Class;
      Name   :         String;
      Value  :        Integer);

   function Content_Group_Number_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return Integer;

   -----------------
   --  Selection  --
   -----------------
   --  Element showing a list of items, user can only select one.

   procedure Content_Group_Selection_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Selection_Add_Option
     (Object  : in out Base.Base_Type'Class;
      Name    :         String;
      Option  :         String;
      Enabled :        Boolean := False);
   --  If parameter Enabled is True, this element will be the selected by default.

   function Content_Group_Selection_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return  String;

   ------------
   --  Date  --
   ------------
   --  Element allowing the user to select a date.
   --  NOTE: format for getter and setter is YYYY-MM-DD.

   procedure Content_Group_Date_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Date_Set
     (Object : in out Base.Base_Type'Class;
      Name   :         String;
      Date   :         String);

   function Content_Group_Date_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return  String;

   -------------
   --  Email  --
   -------------
   --  Element allowing the user to write an email address.

   procedure Content_Group_Email_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Email_Set
     (Object : in out Base.Base_Type'Class;
      Name   :         String;
      Email  :         String);

   function Content_Group_Email_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return  String;

   ----------------
   --  Password  --
   ----------------
   --  Element allowing the user to safely write a password.

   procedure Content_Group_Password_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   function Content_Group_Password_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return  String;

   -------------
   --  Phone  --
   -------------
   --  Element to type a phone number.

   procedure Content_Group_Phone_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :         String;
      Parent_Key :         String;
      On_Change  :        Base.Action_Event := null);

   procedure Content_Group_Phone_Set
     (Object : in out Base.Base_Type'Class;
      Name   :         String;
      Phone  :         String);

   function Content_Group_Phone_Get
     (Object : in out Base.Base_Type'Class;
      Name   :         String)
      return  String;

   ---------------
   --  Warning  --
   ---------------
   --  Custom (non-form) element to potentially warn the user about something.
   --  NOTE: Can be used with Text = "" at first, and using the setter.

   procedure Content_Group_Warning_Add
     (Object     : in out Base.Base_Type'Class;
      Text       :         String;
      Key        :         String;
      Parent_Key :         String);

   procedure Content_Group_Warning_Set
     (Object : in out Base.Base_Type'Class;
      Key    :         String;
      Text   :         String);

   -----------------------------------------------------------------------------
   --  Lists
   -----------------------------------------------------------------------------
   procedure Content_List_Create
     (Object : in out Base.Base_Type'Class;
      Title  :         String);
   --  Create a table for a list of items. This element is displayed in the
   --  content container. Does not need any clearing apart from Content_Clear
   --  to delete this element.
   --  Elements can be selected, and the index can be known using
   --  Content_List_Selected_Row.

   procedure Content_List_Add_Column
     (Object     : in out Base.Base_Type'Class;
      Variable   :         String;
      Parent_Key :         String);
   --  Add a column in the table. Must be called as much as needed before using
   --  Content_List_Add_Item.

   function Content_List_Add_Item
     (Object     : in out Base.Base_Type'Class;
      Parent_Key :         String)
      return Integer;
   --  Returned value is the index of the item in list.
   --  NOTE: Columns must be added before, using Content_List_Add_Column.

   procedure Content_List_Add_Text
     (Object     : in out Base.Base_Type'Class;
      Value      :         String;
      Index      :        Integer;
      Parent_Key :         String);
   --  NOTE: The corresponding item must be created before using Content_List_Add_Item.
   --  This function just place a new column in the table, at a given row.

   function Content_List_Selected_Row
     (Object     : in out Base.Base_Type'Class;
      Parent_Key :         String)
      return Integer;
   --  Return the selected row index.

   --------------------------------------------------------
   --  EXAMPLE: the following List is created like this:
   --------------------------------------------------------
   --            > Content_List_Create (*, key);
   --  ┌───┬───┐ > Content_List_Add_Column (*, "A", key);
   --  │ A │ B │ > Content_List_Add_Column (*, "B", key);
   --  ├───┼───┤ > R1 := Content_List_Add_Item (*, key);
   --  │ 1 | 2 | > Content_List_Add_Text (*, "1", R1, key);
   --  ├───┼───┤ > Content_List_Add_Text (*, "2", R1, key);
   --  │ 3 | 4 | > R2 := Content_List_Add_Item (*, key);
   --  └───┴───┘ > Content_List_Add_Text (*, "3", R2, key);
   --            > Content_List_Add_Text (*, "4", R2, key);
   --------------------------------------------------------

   -----------------------------------------------------------------------------
   --  Footer
   -----------------------------------------------------------------------------
   procedure Footer_Set_State_Text
     (Object : in out Base.Base_Type'Class;
      Text   :         String := "");
   --  Set state text on the left side of the footer.

   procedure Footer_Set_Permanent_Text
     (Object : in out Base.Base_Type'Class;
      Text   :         String := "");
   --  Set permanent text on the right side of the footer.

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   procedure Launch_Dialog
     (Object       : in out Base.Base_Type'Class;
      Title        :         String;
      Content      :         String;
      Confirm_Text :         String          := "";
      Cancel_Text  :         String          := "";
      On_Confirm   :        Base.Action_Event := null;
      On_Cancel    :        Base.Action_Event := null);
   --  Create a jQuery dialog, with two potential buttons.
   --  Buttons are displayed if their corresponding handler is not null.
   --  Confirm button is focused.
   --  It is advised to use Close_Dialog in On_Confirm and On_Cancel handlers.

   procedure Close_Dialog (Object : in out Base.Base_Type'Class);
   --  Close current jQuery dialog, removes it from HTML.

   procedure Launch_Web
     (Object : in out Base.Base_Type'Class;
      URL    :         String);
   --  Open a new tab in browser to URL.
   --  NOTE: user's browser might not accept redirection at first, in this case
   --  the user needs to enable this.

   procedure Print (Object : in out Base.Base_Type'Class);
   --  Call system print on current web view.
   --  /!\ Tends to stop client-server communication.

private

   procedure Setup_Login_Form (Object : in out Base.Base_Type'Class);
   procedure Setup_Login_Buttons (Object : in out Base.Base_Type'Class);
   procedure Set_Login_Error_Message
     (Object : in out Base.Base_Type'Class;
      Error  :         String);

   procedure Setup_Register_Form (Object : in out Base.Base_Type'Class);
   procedure Setup_Register_Buttons (Object : in out Base.Base_Type'Class);

-------------------------------------------------------------------------------
end v22.Gui;
-------------------------------------------------------------------------------
