-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui.ads
--  @copyright See authors list below and README.md file
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
--  Xavier Petit - xp - developpement@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------


with Gnoga.Gui.Base;

with Gnoga.Gui.View;
with Gnoga.Gui.Window;
with Gnoga.Server.Database;

with UXStrings; use UXStrings;

with v22.Cfg;
with v22.Msg;
with v22.Net;
with v22.Prg;
with v22.Sql; use v22.Sql;
with v22.Uxs; use v22.Uxs;

package v22.Gui is

   package GS renames Gnoga.Server;
   package GGB renames Gnoga.Gui.Base;
   package GGV renames Gnoga.Gui.View;
   package GSD renames Gnoga.Server.Database;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Close_Dialog (Object : in out GGB.Base_Type'Class);
   --  Close current jQuery dialog, removes it from HTML.

   procedure Connection_Data_Clear (Object : in out GGB.Base_Type'Class);
   --  Clear all Connection_Data entries. Should be used when user disconnects.
   --  Connection_Data is a free to use dictionary unique to each connection.

   procedure Connection_Data_Delete (Object : in out GGB.Base_Type'Class; Key : String);
   --  Delete a Connection_Data entry. Key not found is silently ignored.
   --  Connection_Data is a free to use dictionary unique to each connection.

   function Connection_Data_Get (Object : in out GGB.Base_Type'Class; Key : String) return  String;
   --  Retrieve a Connection_Data entry. Returns an empty string if key does not exist.
   --  Connection_Data is a free to use dictionary unique to each connection.

   procedure Connection_Data_List (Object : in out GGB.Base_Type'Class);
   --  List all Connection_Data entries on debug log.
   --  Connection_Data is a free to use dictionary unique to each connection.

   procedure Connection_Data_Set (Object : in out GGB.Base_Type'Class; Key : String; Value : String);
   --  Create a new Connection_Data entry. If Key already exists, the value is replaced.
   --  Connection_Data is a free to use dictionary unique to each connection.

   procedure Content_Clear (Object : in out GGB.Base_Type'Class);
   --  Removes any content inside content parent.

   procedure Content_Clear_HTML (Object : in out GGB.Base_Type'Class);
   --  Clear HTML in content parent.

   procedure Content_Clear_Text (Object : in out GGB.Base_Type'Class);
   --  Clear text in content parent.
   --  NOTE: should be used when the only content of the menu is this text.
   --  otherwise, should use Content_Parent instead.

   procedure Content_Clear_Title (Object : in out GGB.Base_Type'Class);
   --  Clear title above content.

   procedure Content_Group_Add_Button (Object : in out GGB.Base_Type'Class; Text : String;
                                       On_Click : GGB.Action_Event; Parent_Key : String);
   --  Add a button with a callback. Can be use as a submit button.

   procedure Content_Group_Add_Space (Object : in out GGB.Base_Type'Class; Parent_Key : String; Height : Integer := 8);
   --  Add space between rows in form group.

   procedure Content_Group_Add_Title (Object : in out GGB.Base_Type'Class; Title : String; Parent_Key : String);
   --  Add a title to a form group.
   --  Title from arguments of Content_Group_Create is already on screen
   --  and has a higher emphasis.

   procedure Content_Group_Check_Box_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                          On_Change : GGB.Action_Event := null);
   --  Add a check box in a group.

   procedure Content_Group_Check_Box_Checked (Object : in out GGB.Base_Type'Class; Name : String; Is_Checked : Boolean);
   --  Set a check box state in a group.

   function Content_Group_Check_Box_Is_Checked (Object : in out GGB.Base_Type'Class; Name : String) return Boolean;
   --  Get a check box state in a group.

   procedure Content_Group_Create (Object : in out GGB.Base_Type'Class; Title : String);
   --  Create a container for forms. This element is displayed in the
   --  content container. Does not need any clearing apart from Content_Clear
   --  to delete this element.

   procedure Content_Group_Date_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                     On_Change : GGB.Action_Event := null);
   --  Add a date in a group.

   function Content_Group_Date_Get (Object : in out GGB.Base_Type'Class; Name : String) return String;
   --  Get a date in a group. Format is YYYY-MM-DD.

   procedure Content_Group_Date_Set (Object : in out GGB.Base_Type'Class; Name : String; Date : String);
   --  Set a date in a group. Format is YYYY-MM-DD.

   --  Drop-down menu  --
   procedure Content_Group_Drop_Down_Menu_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                               On_Change : GGB.Action_Event := null);
   --  Add a drop-down menu, with customizable default item, in a group.

   procedure Content_Group_Drop_Down_Menu_Add_Option (Object : in out GGB.Base_Type'Class; Name : String; Option : String;
                                                 Enabled : Boolean := False);
   --  Add an item to a drop-down menu. If Enabled is True, this item will be the displayed by default.

   procedure Content_Group_Drop_Down_Menu_Add_Option_From_DB_By_Key (Object : in out GGB.Base_Type'Class;
                                                                     Key : String;
                                                                     DB : in out GSD.Connection'Class;
                                                                     Table : String;
                                                                     Column_Key : String;
                                                                     Column_Display : String;
                                                                     Match_Value : String := "";
                                                                     No_Display_Raw_One : Boolean := False);
   --  Registers all Column_Key and Column_Display lines from Table and
   --  select by default (for further display) the one where Column_Key = Column_ID.

   procedure Content_Group_Drop_Down_Menu_Empty_Options (Object : in out GGB.Base_Type'Class; Name : String);
   --  Clear items in a drop-down menu.

   function Content_Group_Drop_Down_Menu_Get (Object : in out GGB.Base_Type'Class; Name : String) return String;
   --  Get the selected item in a drop-down menu.

   function Content_Group_Drop_Down_Menu_Get_Key_From_DB (Object : in out GGB.Base_Type'Class;
                                                          Key : String;
                                                          DB : in out GSD.Connection'Class;
                                                          Table : String;
                                                          Column_Key : String;
                                                          Column_Display : String) return String;
   --  Returns the Column_Key from Table where Column_Display = Selected value from Key drop down menu.

   procedure Content_Group_Drop_Down_Menu_Set_Selected (Object : in out GGB.Base_Type'Class; Name : String; Index : in Positive;
                                                       Value : in Boolean := True);
   --  Set selected or un-selected item in a drop-down menu.

   procedure Content_Group_Email_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                      On_Change :  GGB.Action_Event := null);
   --  Add an email in a group.

   function Content_Group_Email_Get (Object : in out GGB.Base_Type'Class; Name : String) return  String;
   --  Get an email in a group.

   procedure Content_Group_Email_Set (Object : in out GGB.Base_Type'Class; Name : String; Email : String);
   --  Set an email in a group.

   procedure Content_Group_Item_Lock (Object : in out GGB.Base_Type'Class; Name : String);
   --  Lock a form in a form group so the user can't write anything.

   procedure Content_Group_Item_Unlock (Object : in out GGB.Base_Type'Class; Name : String);
   --  Unlock a form in a form group.

   procedure Content_Group_Item_Place_Holder (Object : in out GGB.Base_Type'Class; Name : String; Place_Holder : String);
   --  Set a place-holder for field-type forms.

   procedure Content_Group_Number_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                       On_Change : GGB.Action_Event := null);
   --  Add a number in a group.

   function Content_Group_Number_Get (Object : in out GGB.Base_Type'Class; Name : String) return Integer;
   --  Get a number value in a group.

   procedure Content_Group_Number_Set (Object : in out GGB.Base_Type'Class; Name : String; Value : Integer);
   --  Set a number in a group.

   procedure Content_Group_Password_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                         Parent_Key : String; On_Change : GGB.Action_Event := null);
   --  Add a password in a group.

   function Content_Group_Password_Get (Object : in out GGB.Base_Type'Class; Name : String) return String;
   --  Get a password in a group.

   procedure Content_Group_Password_Set (Object : in out GGB.Base_Type'Class; Name : String; Password : String);
   --  Set a password in a group to simulate an existing password in update form.

   procedure Content_Group_Phone_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                      On_Change : GGB.Action_Event := null);
   --  Add a phone number in a group.

   function Content_Group_Phone_Get (Object : in out GGB.Base_Type'Class; Name : String) return  String;
   --  Get a phone number in a group.

   procedure Content_Group_Phone_Set (Object : in out GGB.Base_Type'Class; Name : String; Phone : String);
   --  Set a phone number in a group.

   procedure Content_Group_Text_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                     On_Change : GGB.Action_Event := null);
   --  Add text entry in a group.

   function Content_Group_Text_Get (Object : in out GGB.Base_Type'Class; Name : String) return String;
   --  Get text content in a group.

   procedure Content_Group_Text_Set (Object : in out GGB.Base_Type'Class; Name : String; Text : String);
   --  Set text content in a group.

   procedure Content_Group_Text_Area_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                          On_Change : GGB.Action_Event := null);
   --  Add text area with customizable height in a group.

   function Content_Group_Text_Area_Get (Object : in out GGB.Base_Type'Class; Name : String) return String;
   --  Get text area content in a group.

   procedure Content_Group_Text_Area_Set (Object : in out GGB.Base_Type'Class; Name : String; Text : String);
   --  Set text area content in a group.

   procedure Content_Group_Warning_Add (Object : in out GGB.Base_Type'Class; Text : String; Key : String; Parent_Key : String);
   --  Add a custom (non form) warning area in a group to display a warning message.
   --  Can be used with an empty Text at first, then later using the setter.

   procedure Content_Group_Warning_Set (Object : in out GGB.Base_Type'Class; Key : String; Text : String);
   --  Display a warning message in a group.

   --------------------------------------------------------
   --  Creation list example
   --------------------------------------------------------
   --            > Content_List_Create (*, key);
   --  ┌───┬───┐ > Content_List_Add_Column (*, "A", key);
   --  │ A │ B │ > Content_List_Add_Column (*, "B", key);
   --  ├───┼───┤ > R1 := Content_List_Add_Item (*, key, 1);
   --  │ 1 | 2 | > Content_List_Add_Text (*, "1", R1, key);
   --  ├───┼───┤ > Content_List_Add_Text (*, "2", R1, key);
   --  │ 3 | 4 | > R2 := Content_List_Add_Item (*, key, 2);
   --  └───┴───┘ > Content_List_Add_Text (*, "3", R2, key);
   --            > Content_List_Add_Text (*, "4", R2, key);
   --------------------------------------------------------

   procedure Content_List_Add_Column (Object : in out GGB.Base_Type'Class; Variable : String; Parent_Key : String);
   --  Add a column in the table.
   --  Must be called as much as needed before using Content_List_Add_Item.


   function Content_List_Add_Item (Object : in out GGB.Base_Type'Class; Parent_Key : String; Row_Index : Integer; Style : String := "") return Integer;
   --  Returns the item index in list.
   --  Columns must be added before using Content_List_Add_Column.
   --  Style controls the row text alignment (left, center, right). Center is the default.

   procedure Content_List_Add_Text (Object : in out GGB.Base_Type'Class;
                                    Value : String;
                                    Index : Integer;
                                    Parent_Key : String;
                                    Style : String := "");
   --  This function place a new column in the table at a given row.
   --  The corresponding item must be created before using Content_List_Add_Item.
   --  Style controls the column text alignment (left, center, right). Center is the default.

   procedure Content_List_Create (Object : in out GGB.Base_Type'Class; Title : String);
   --  Create a table for a list of items. This element is displayed in the
   --  content container. Does not need any clearing apart from Content_Clear
   --  to delete this element.
   --  Elements can be selected, and the index obtained using Content_List_Selected_Row.

   function Content_List_Selected_Row (Object : in out GGB.Base_Type'Class; Parent_Key : String) return Integer;
   --  Return the selected row index.

   procedure Content_Load_HTML (Object : in out GGB.Base_Type'Class; Text : String);
   --  Displays a HTML encoded file.

   function Content_Parent (Object : in out GGB.Base_Type'Class) return GGV.View_Access;
   --  Return the element below menu title, containing all content.

   procedure Content_Put_HTML (Object : in out GGB.Base_Type'Class; Text : String);
   --  Displays a HTML encoded string.

   procedure Content_Put_Text (Object : in out GGB.Base_Type'Class; Text : String);
   --  Set text in content parent.
   --  Should be used when the only content of the menu is this text,
   --  otherwise use Content_Parent instead.

   procedure Content_Put_Title (Object : in out GGB.Base_Type'Class; Title : String);
   --  Set title above content.

   procedure Dialog_Buttons (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                             Key : String;
                             Left_Text : String := "";
                             Left_Handler : GGB.Action_Event := null;
                             Right_Text : String := "";
                             Right_Handler : GGB.Action_Event := null);
   --  Display a one or two Buttons dialog, for user to quit a screen,
   --  user choices like Quit/Update or Cancel/Validate, etc.

   procedure Dialog_Popup (Object : in out GGB.Base_Type'Class;
                          Title : String; Content : String;
                          Left_Text : String := "";
                          Left_Handler : GGB.Action_Event := null;
                          Right_Text : String := "";
                          Right_Handler : GGB.Action_Event := null;
                          Height : Natural := 150;
                          Width : Natural := 300;
                          Position_My : String := "center top+40";
                          Position_At: String := "center top+40");
   --  Create a jQuery dialog, with two potential buttons. Buttons are displayed
   --  if their corresponding handler is not null. Confirm button is focused.
   --  It is advised to use Close_Dialog in On_Confirm and On_Cancel handlers.
   --  See https://api.jqueryui.com/position  for Position_My and Position_At information.

   procedure Footer_Set_Left_Text (Object : in out GGB.Base_Type'Class; Text : String := "");
   --  Displays text on the left side of the footer.

   procedure Footer_Set_Right_Text (Object : in out GGB.Base_Type'Class; Text : String := "");
   --  Displays text on the right side of the footer.

   function Get_Display_In_Progress (Object : in out GGB.Base_Type'Class) return Boolean;
   --  Get display in progress status (to block user input when user display in progress on purpose).

   procedure Header_Application_Menu_Add (Key : String; Name : String; Parent_Key :  String; On_Click : GGB.Action_Event);
   --  Add menu and cascaded menu to application menu.
   --  Key must be unique and On_Click handler must call Header_Notify_Menu_Click.

   procedure Header_Notify_Menu_Click (Object : in out GGB.Base_Type'Class; Key : String);
   --  Header module menu callback.

   procedure Header_Notify_User_Menu_Click (Object : in out GGB.Base_Type'Class);
   --  Header user menu callback.

   procedure Header_Set_Root (Key :  String; Name : String; On_Click : GGB.Action_Event);
   --  Set default root module menu.
   --  Key must be unique and On_Click handler must call Header_Notify_Menu_Click.

   procedure Header_User_Menu_Add (Object : in out GGB.Base_Type'Class; Name : String; On_Click : GGB.Action_Event);
   --  Add menu to user menu.
   --  On_Click handler must call Header_Notify_User_Menu_Click otherwise.
   --  User navigation menu can't be closed.

   procedure Launch_Web (Object : in out GGB.Base_Type'Class; URL : String);
   --  Open a new tab in browser to URL.
   --  User's browser might not accept redirection at first, in this case
   --  the user needs to enable this.

   procedure List (Object : in out GGB.Base_Type'Class;
                   Db_Name : String;
                   Db_Table : String;
                   List_Name : String;
                   List_Key : String;
                   List_Header : String;
                   List_Columns : String;
                   Condition : String := "");
   procedure List (Object : in out GGB.Base_Type'Class;
                   DB : in out GSD.Connection'Class;
                   Db_Table : String;
                   List_Name : String;
                   List_Key : String;
                   List_Header : String;
                   List_Columns : String;
                   Condition : String := "");
   --  List Table columns listed in Columns_Names (comma separated) with
   --  header table titles Columns_Titles (comma separated) and Condition.
   --  Key is the table list unique identifier
   --  The column tagged with "~" (constant ND) in Columns_Names is the
   --  column key for further seeking the record.
   --  Title is the list title. See extended example in manual.

   procedure Main_Menu_Add_Delimiter_Above (Object : in out GGB.Base_Type'Class; Key : String);
   --  Add a delimiter above a sub-element.

   procedure Main_Menu_Add_Element (Object : in out GGB.Base_Type'Class;
                                    Key : String; Name : String; Icon_SRC : String; On_Click : GGB.Action_Event := null);
   --  Add an element to a menu.
   --  IMPORTANT: Key must be UNIQUE /!\

   procedure Main_Menu_Add_Sub_Element (Object : in out GGB.Base_Type'Class;
                                        Key : String; Name : String; Parent_Key : String; On_Click : GGB.Action_Event);
   --  Add an element to a sub menu. If parent is "File", and this element is "Edit",
   --  then a unique key should be set as "File_Edit".
   --  IMPORTANT: Key must be UNIQUE /!\
   --  IMPORTANT: On_Click event procedure must in turn call Main_Menu_Notify_Sub_Element_Click /!\

   procedure Main_Menu_Clear (Object : in out GGB.Base_Type'Class);
   --  Clear Main_Menu content

   procedure Main_Menu_Disable_Shortcuts (Object : in out GGB.Base_Type'Class);
   --  Disable Main_Menu shortcuts, for both elements and sub-elements.

   procedure Main_Menu_Enable_Shortcuts (Object : in out GGB.Base_Type'Class);
   --  Enable Main_Menu shortcuts, allowing both elements and sub-elements shortcuts.

   procedure Main_Menu_Load (Object : in out GGB.Base_Type'Class);
   --  Load Main_Menu after elements has been added.

   procedure Main_Menu_Notify_Sub_Element_Click (Object : in out GGB.Base_Type'Class; Key : String);
   --  Callback to place in sub-elements' handlers.

   procedure Main_Menu_Set_Clickable (Object : in out GGB.Base_Type'Class; Key : String);
   --  Set an element (or sub-element) clickable.

   procedure Main_Menu_Set_Unclickable (Object : in out GGB.Base_Type'Class; Key : String);
   --  Set an element (or sub-element) unclickable.

   procedure Pop_Up (Object : in out Gnoga.Gui.Base.Base_Type'Class; Text : String; Title : String := "Message");
   -- Display a pop-up message with a title defaulted to "Message".

   procedure Print (Object : in out GGB.Base_Type'Class);
   --  Call system printing on current web view.
   --  /!\ Tends to stop client-server communication.

   procedure Put_User_Icon (Object : in out GGB.Base_Type'Class);
   --  Display the user icon registered by Set_User_Icon.

   procedure Set_Application_Icon (Icon_File :  String);
   --  Set the application icon when clicked displays the modules menu.

   procedure Set_Browser_Icon (Icon_File : String);
   --  Set application icon in browser tab. Don't work, to be fixed.

   procedure Set_Browser_Title (Object : in out GGB.Base_Type'Class; Title : String);
   --  Set application title in browser tab.

   procedure Set_Debug (Switch : On_Off);
   --  Set Gnoga debug mode. This mode must be set before calling Setup to take effect.

   procedure Set_Display_In_Progress (Object : in out GGB.Base_Type'Class; Status : Boolean := True);
   --  Set Display in progress status.

   procedure Set_Login (Switch : On_Off);
   --  Application access control.

   procedure Set_User_Icon (Icon_File : String);
   --  Set the user icon when clicked displays the user menu.

   procedure Set_User_Name (Object : in out GGB.Base_Type'Class; Name : String);
   --  Set user name, displayed next to the user icon.

   procedure Setup (On_User_Connect : GGB.Action_Event;
                    Host : String := "";
                    Port : Integer := 8_080;
                    Boot : in String := "boot_jqueryui.html";
                    Title : String := "";
                    Server_Closed_Content : String := "Server closed.");
   --  Set application connection parameters
   --  On_User_Connect is called every time a user calls a web page.

   procedure User_Logout (Object : in out GGB.Base_Type'Class);
   --  Disconnect user. This function assumes Setup was called.

-------------------------------------------------------------------------------
private

   procedure Test_Plot (Object : in out GGB.Base_Type'Class);
   -- Plot test routine

   Access_Control : On_Off := Off;
   Debug_Control : On_Off := Off;
   Application_Icon_File : String := "";
   User_Icon_File : String := "";
   Image_Gnoga_Root : String := "/img/";

-------------------------------------------------------------------------------
end v22.Gui;
-------------------------------------------------------------------------------
