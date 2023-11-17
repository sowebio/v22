-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
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

with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Hashed_Maps;

with GNAT.SHA512;

with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.Plugin;
with Gnoga.Gui.Plugin.jQueryUI;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.Window;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Types;

with UXStrings.Hash;
with UXStrings.Conversions; use UXStrings.Conversions;

with v22.Gui.Header;
with v22.Gui.Main_Menu;
with v22.Gui.Footer;

package body v22.Gui is

   use all type Gnoga.String;

   package AC renames Ada.Calendar;
   package GGE renames Gnoga.Gui.Element;
   package GGPJ renames Gnoga.Gui.Plugin.jQueryUI;
   package GGPJW renames Gnoga.Gui.Plugin.jQueryUI.Widget;

   ----------------------------------------------------------------------------
   --  Private declarations
   ----------------------------------------------------------------------------

   package Integer_Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => String, Element_Type => Integer, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   package Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => String, Element_Type => String, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   ID_Main : Integer; --  Root menu ID
   On_Custom_Connect : GGB.Action_Event;

   Login_Group_Key : constant String := "Se connecter";

   Header_Dict : Integer_Dictionary.Map;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      User_Logged_In : Boolean  := False;
      --  Connection login flag
      User_Logged_Since : AC.Time;
      --  Connection time
      Custom_Data : Dictionary.Map;
      --  Connection level free to use dictionnary
      Window : Gnoga.Gui.Window.Pointer_To_Window_Class;
      --
      Container : GGV.View_Type;
      --
      Header_Parent : GGV.View_Type;
      Header_Instance : v22.Gui.Header.Header_Type;
      Header_Dict : Integer_Dictionary.Map;
      --
      Main_Menu_Parent : GGV.View_Type;
      Main_Menu_Instance : v22.Gui.Main_Menu.Main_Menu_Type;
      Main_Menu_Dict : Integer_Dictionary.Map;
      --
      Footer_Instance : v22.Gui.Footer.Footer_Type;
      Footer_Parent : GGV.View_Type;
      --
      Content : GGV.View_Type;
      Content_Header : GGV.View_Type;
      Content_Text : aliased GGV.View_Type;
      Content_HTML : aliased GGV.View_Type;
   end record;

   type App_Access is access all App_Data;

   procedure Content_Group_Item_Add (Object : in out GGB.Base_Type'Class; Item : GGE.Pointer_To_Element_Class;
                                     Name : String; Parent_Key : String; On_Change : GGB.Action_Event := null);
   procedure Content_List_Click_Handler (Object : in out GGB.Base_Type'Class);
   procedure Put_Login_Form (Object : in out GGB.Base_Type'Class);
   procedure Put_Login_Buttons (Object : in out GGB.Base_Type'Class);
   procedure Put_Login_Message (Object : in out GGB.Base_Type'Class; Error : String);
   function Header_Get (Key : String) return Integer;
   procedure Header_Set (Key : String; Value : Integer);

   function Int_Value is new Integer_Value (Integer); --  Error: generic subprogram cannot be called

   function Main_Menu_Get (Object : in out GGB.Base_Type'Class; Key : String) return Integer;
   procedure Main_Menu_Set (Object : in out GGB.Base_Type'Class; Key : String; Value : Integer);
   procedure On_Connect (Screen : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection : access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Key_Down (Object : in out GGB.Base_Type'Class; Event : Keyboard_Event_Record);

   procedure On_Key_Pressed (Object : in out GGB.Base_Type'Class; Char : Character);
   procedure On_Main_Menu_Callback (Object : in out GGB.Base_Type'Class);
   procedure On_Logo (Object : in out GGB.Base_Type'Class);
   procedure On_Tool_Bar_Expand (Object : in out GGB.Base_Type'Class);
   procedure On_User (Object : in out GGB.Base_Type'Class);
   procedure Password_Forgotten (Object : in out GGB.Base_Type'Class);
   procedure User_Login (Object : in out GGB.Base_Type'Class);

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Clear_Connection_Data (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Custom_Data.Clear;
   end Clear_Connection_Data;

   ----------------------------------------------------------------------------
   procedure Close_Dialog (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Dialog_Class : constant GGE.Pointer_To_Element_Class := App.Container.Element ("dialog");
      Dialog : constant GGPJW.Dialog_Access := GGPJW.Dialog_Access (Dialog_Class);
   begin
      Dialog.Remove;
   end Close_Dialog;

   ----------------------------------------------------------------------------
   procedure Content_Clear (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Text.Inner_HTML ("");
   end Content_Clear;

   ----------------------------------------------------------------------------
   procedure Content_Clear_HTML (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_HTML.Text ("");
   end Content_Clear_HTML;

   ----------------------------------------------------------------------------
   procedure Content_Clear_Text (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Content_Put_Text (Object, "");
   end Content_Clear_Text;

   ----------------------------------------------------------------------------
   procedure Content_Clear_Title (Object : in out GGB.Base_Type'Class) is
   begin
      Content_Put_Title (Object, "");
   end Content_Clear_Title;

   ----------------------------------------------------------------------------
   procedure Content_Group_Add_Button (Object : in out GGB.Base_Type'Class; Text : String;
                                       On_Click : GGB.Action_Event; Parent_Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);

      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;
      First_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Second_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;

      Submit_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
   begin
      Row.Dynamic;
      First_Column.Dynamic;
      Second_Column.Dynamic;
      Submit_Button.Dynamic;

      Row.Create (Table.all);
      First_Column.Create (Row.all);
      Second_Column.Create (Row.all);

      Submit_Button.Create (Second_Column.all, Text);
      Submit_Button.Style ("width", "100%");
      Submit_Button.Style ("box-sizing", "border-box");
      Submit_Button.On_Click_Handler (On_Click);
   end Content_Group_Add_Button;

   ----------------------------------------------------------------------------
   procedure Content_Group_Add_Space (Object : in out GGB.Base_Type'Class; Parent_Key : String; Height :  Integer := 8) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;
   begin
      Row.Dynamic;
      Row.Create (Table.all);
      Row.Style ("height", From_UTF_8 (Height'Image) & "px");
   end Content_Group_Add_Space;

   ----------------------------------------------------------------------------
   procedure Content_Group_Add_Title (Object : in out GGB.Base_Type'Class; Title : String; Parent_Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;
      Data : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Span : constant GGE.Common.Span_Access := new GGE.Common.Span_Type;
   begin
      Row.Dynamic;
      Data.Dynamic;
      Span.Dynamic;

      Content_Group_Add_Space (Object, Parent_Key, 8);
      Row.Create (Table.all);
      Data.Create (Row.all, Column_Span => 2);
      Data.Style ("text-align", "center");
      Span.Create (Data.all, Title);
      Span.Class_Name ("content-group-title");
   end Content_Group_Add_Title;

   ----------------------------------------------------------------------------
   procedure Content_Group_Check_Box_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                            Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Check_Box : constant GGE.Pointer_To_Element_Class := new GGE.Form.Check_Box_Type;
   begin
      Check_Box.Dynamic;
      GGE.Form.Check_Box_Access (Check_Box).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Check_Box, Name, Parent_Key, On_Change);
   end Content_Group_Check_Box_Add;

   ----------------------------------------------------------------------------
   procedure Content_Group_Check_Box_Checked (Object : in out GGB.Base_Type'Class; Name : String; Is_Checked : Boolean) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Check_Box_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Check_Box : constant GGE.Form.Check_Box_Access := GGE.Form.Check_Box_Access (Check_Box_Element);
   begin
      Check_Box.Checked (Is_Checked);
   end Content_Group_Check_Box_Checked;

   ----------------------------------------------------------------------------
   function Content_Group_Check_Box_Is_Checked (Object : in out GGB.Base_Type'Class; Name : String) return Boolean is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Check_Box_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Check_Box : constant GGE.Form.Check_Box_Access := GGE.Form.Check_Box_Access (Check_Box_Element);
   begin
      return Check_Box.Checked;
   end Content_Group_Check_Box_Is_Checked;

   ----------------------------------------------------------------------------
   procedure Content_Group_Create (Object : in out GGB.Base_Type'Class; Title : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent : constant GGE.Pointer_To_Element_Class := new GGE.Form.Form_Type;

      Table_Element : constant GGE.Pointer_To_Element_Class := new GGE.Table.Table_Type;
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);

      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;
      Data : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
   begin
      Parent.Dynamic;
      Table_Element.Dynamic;
      Row.Dynamic;
      Data.Dynamic;

      GGE.Form.Form_Access (Parent).Create (App.Content_Text);
      Parent.Class_Name ("content-group");
      App.Content_Text.Add_Element (Title, Parent);

      Table.Dynamic;
      Table.Create (Parent.all);
      Table.Style ("width", "100%");
      App.Content_Text.Add_Element ("Table_" & Title, Table_Element);

      Row.Create (Table.all);
      Data.Create (Row.all, Content => Title, Column_Span => 2);
      Data.Class_Name ("content-group-header");

      Content_Group_Add_Space (Object, Title, 8);

   end Content_Group_Create;

   ----------------------------------------------------------------------------
   procedure Content_Group_Date_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                       Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App  : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Date : constant GGE.Pointer_To_Element_Class := new GGE.Form.Date_Type;
   begin
      Date.Dynamic;
      GGE.Form.Date_Access (Date).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Date, Name, Parent_Key, On_Change);
   end Content_Group_Date_Add;

   ----------------------------------------------------------------------------
   function Content_Group_Date_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Date_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Date : constant GGE.Form.Date_Access := GGE.Form.Date_Access (Date_Element);
   begin
      return Date.Value;
   end Content_Group_Date_Get;

   ----------------------------------------------------------------------------
   procedure Content_Group_Date_Set (Object : in out GGB.Base_Type'Class; Name : String; Date : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Date_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Date_Form : constant GGE.Form.Date_Access := GGE.Form.Date_Access (Date_Element);
   begin
      Date_Form.Value (Date);
   end Content_Group_Date_Set;

   ----------------------------------------------------------------------------
   procedure Content_Group_Drop_Down_Menu_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                            Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Selection : constant GGE.Pointer_To_Element_Class := new GGE.Form.Selection_Type;
   begin
      Selection.Dynamic;
      GGE.Form.Selection_Access (Selection).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Selection, Name, Parent_Key, On_Change);
   end Content_Group_Drop_Down_Menu_Add;

   ----------------------------------------------------------------------------
   procedure Content_Group_Drop_Down_Menu_Add_Option (Object : in out GGB.Base_Type'Class; Name : String;
                                                     Option : String; Enabled : Boolean := False) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Selection_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Selection : constant GGE.Form.Selection_Access := GGE.Form.Selection_Access (Selection_Element);
   begin
      Selection.Add_Option (Value => Option, Text => Option, Selected => Enabled);
   end Content_Group_Drop_Down_Menu_Add_Option;

   ----------------------------------------------------------------------------
   function Content_Group_Drop_Down_Menu_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Selection_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Selection : constant GGE.Form.Selection_Access := GGE.Form.Selection_Access (Selection_Element);
   begin
      return Selection.Value;
   end Content_Group_Drop_Down_Menu_Get;

   ----------------------------------------------------------------------------
   procedure Content_Group_Email_Add (Object : in out GGB.Base_Type'Class; Name : String; Parent_Key : String;
                                      On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Email : constant GGE.Pointer_To_Element_Class := new GGE.Form.Email_Type;
   begin
      Email.Dynamic;
      GGE.Form.Email_Access (Email).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Email, Name, Parent_Key, On_Change);
   end Content_Group_Email_Add;

   ----------------------------------------------------------------------------
   function Content_Group_Email_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Email_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Email : constant GGE.Form.Email_Access := GGE.Form.Email_Access (Email_Element);
   begin
      return Email.Value;
   end Content_Group_Email_Get;

   ----------------------------------------------------------------------------
   procedure Content_Group_Email_Set (Object : in out GGB.Base_Type'Class; Name : String; Email : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Email_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Email_Form : constant GGE.Form.Email_Access := GGE.Form.Email_Access (Email_Element);
   begin
      Email_Form.Value (Email);
   end Content_Group_Email_Set;

   ----------------------------------------------------------------------------
   procedure Content_Group_Item_Lock (Object : in out GGB.Base_Type'Class; Name : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Form_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Form : constant GGE.Form.Form_Element_Access := GGE.Form.Form_Element_Access (Form_Element);
   begin
      Form.Disabled;
   end Content_Group_Item_Lock;

   ----------------------------------------------------------------------------
   procedure Content_Group_Item_Unlock (Object : in out GGB.Base_Type'Class; Name : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Form_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Form : constant GGE.Form.Form_Element_Access := GGE.Form.Form_Element_Access (Form_Element);
   begin
      Form.Disabled (False);
   end Content_Group_Item_Unlock;

   ----------------------------------------------------------------------------
   procedure Content_Group_Item_Place_Holder (Object : in out GGB.Base_Type'Class; Name : String; Place_Holder : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Form_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Form : constant GGE.Form.Text_Area_Access := GGE.Form.Text_Area_Access (Form_Element);
   begin
      Form.Place_Holder (Place_Holder);
   end Content_Group_Item_Place_Holder;

   ----------------------------------------------------------------------------
   procedure Content_Group_Number_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                         Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Number : constant GGE.Pointer_To_Element_Class := new GGE.Form.Number_Type;
   begin
      Number.Dynamic;
      GGE.Form.Number_Access (Number).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Number, Name, Parent_Key, On_Change);
   end Content_Group_Number_Add;

   ----------------------------------------------------------------------------
   function Content_Group_Number_Get (Object : in out GGB.Base_Type'Class; Name : String) return Integer is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Number_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Number : constant GGE.Form.Number_Access := GGE.Form.Number_Access (Number_Element);
   begin
      return Number.Value;
   end Content_Group_Number_Get;
   ----------------------------------------------------------------------------

   procedure Content_Group_Number_Set (Object : in out GGB.Base_Type'Class; Name : String; Value  : Integer) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Number_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Number : constant GGE.Form.Number_Access := GGE.Form.Number_Access (Number_Element);
   begin
      Number.Value (Value);
   end Content_Group_Number_Set;

   ----------------------------------------------------------------------------
   procedure Content_Group_Password_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                            Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Password : constant GGE.Pointer_To_Element_Class := new GGE.Form.Password_Type;
   begin
      Password.Dynamic;
      GGE.Form.Password_Access (Password).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Password, Name, Parent_Key, On_Change);
   end Content_Group_Password_Add;

   ----------------------------------------------------------------------------
   function Content_Group_Password_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Password_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Password : constant GGE.Form.Password_Access := GGE.Form.Password_Access (Password_Element);
   begin
      return Password.Value;
   end Content_Group_Password_Get;

   ----------------------------------------------------------------------------
   procedure Content_Group_Password_Set (Object : in out GGB.Base_Type'Class; Name : String; Password : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Password_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Password_Form : constant GGE.Form.Password_Access := GGE.Form.Password_Access (Password_Element);
   begin
      Password_Form.Value (Password);
   end Content_Group_Password_Set;

   ----------------------------------------------------------------------------
   procedure Content_Group_Phone_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                        Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Tel : constant GGE.Pointer_To_Element_Class := new GGE.Form.Tel_Type;
   begin
      Tel.Dynamic;
      GGE.Form.Tel_Access (Tel).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Tel, Name, Parent_Key, On_Change);
   end Content_Group_Phone_Add;

   ----------------------------------------------------------------------------
   function Content_Group_Phone_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Tel_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Tel : constant GGE.Form.Tel_Access := GGE.Form.Tel_Access (Tel_Element);
   begin
      return Tel.Value;
   end Content_Group_Phone_Get;

   ----------------------------------------------------------------------------
   procedure Content_Group_Phone_Set (Object : in out GGB.Base_Type'Class; Name : String; Phone : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Tel_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Tel : constant GGE.Form.Tel_Access  := GGE.Form.Tel_Access (Tel_Element);
   begin
      Tel.Value (Phone);
   end Content_Group_Phone_Set;

  ----------------------------------------------------------------------------
   procedure Content_Group_Text_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                     Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Edit_Text : constant GGE.Pointer_To_Element_Class := new GGE.Form.Text_Type;
   begin
      Edit_Text.Dynamic;
      GGE.Form.Text_Access (Edit_Text).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Edit_Text, Name, Parent_Key, On_Change);
   end Content_Group_Text_Add;

   ----------------------------------------------------------------------------
   function Content_Group_Text_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Edit_Text_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Edit_Text : constant GGE.Form.Text_Access := GGE.Form.Text_Access (Edit_Text_Element);
   begin
      return Edit_Text.Value;
   end Content_Group_Text_Get;

   ----------------------------------------------------------------------------
   procedure Content_Group_Text_Set (Object : in out GGB.Base_Type'Class; Name : String; Text : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Edit_Text_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Edit_Text : constant GGE.Form.Text_Access := GGE.Form.Text_Access (Edit_Text_Element);
   begin
      Edit_Text.Value (Text);
   end Content_Group_Text_Set;

   ----------------------------------------------------------------------------
   procedure Content_Group_Text_Area_Add (Object : in out GGB.Base_Type'Class; Name : String;
                                            Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent : constant GGE.Form.Form_Access := GGE.Form.Form_Access (Parent_Element);
      Text_Area : constant GGE.Pointer_To_Element_Class := new GGE.Form.Text_Area_Type;
   begin
      Text_Area.Dynamic;
      GGE.Form.Text_Area_Access (Text_Area).Create (Form => Parent.all, Name => Name);
      Text_Area.Style ("resize", "vertical");
      Text_Area.Style ("height", "38px");
      Text_Area.Style ("min-height", "38px");
      Content_Group_Item_Add (Object, Text_Area, Name, Parent_Key, On_Change);
   end Content_Group_Text_Area_Add;

   ----------------------------------------------------------------------------
   function Content_Group_Text_Area_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Text_Area_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Text_Area : constant GGE.Form.Text_Area_Access := GGE.Form.Text_Area_Access (Text_Area_Element);
   begin
      return Text_Area.Value;
   end Content_Group_Text_Area_Get;

   ----------------------------------------------------------------------------
   procedure Content_Group_Text_Area_Set (Object : in out GGB.Base_Type'Class; Name : String; Text : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Text_Area_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Text_Area : constant GGE.Form.Text_Area_Access := GGE.Form.Text_Area_Access (Text_Area_Element);
   begin
      Text_Area.Value (Text);
   end Content_Group_Text_Area_Set;

   ----------------------------------------------------------------------------
   procedure Content_Group_Warning_Add (Object : in out GGB.Base_Type'Class; Text : String; Key : String; Parent_Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row  : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;
      Data : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Span_Element : constant GGE.Pointer_To_Element_Class := new GGE.Common.Span_Type;
      Span : constant GGE.Common.Span_Access := GGE.Common.Span_Access (Span_Element);
   begin
      Row.Dynamic;
      Data.Dynamic;
      Span_Element.Dynamic;

      Row.Create (Table.all);
      Data.Create (Row.all, Column_Span => 2);
      Data.Style ("text-align", "center");
      Span.Create (Data.all, Text);
      Span.Class_Name ("content-group-warning");

      App.Content_Text.Add_Element (Key, Span_Element);
   end Content_Group_Warning_Add;

   ----------------------------------------------------------------------------
   procedure Content_Group_Warning_Set (Object : in out GGB.Base_Type'Class; Key : String; Text : String)  is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Span_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Key);
      Span : constant GGE.Common.Span_Access := GGE.Common.Span_Access (Span_Element);
   begin
      Span.Text (Text);
   end Content_Group_Warning_Set;

   ----------------------------------------------------------------------------
   procedure Content_List_Add_Column (Object : in out GGB.Base_Type'Class; Variable : String; Parent_Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Row_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("List_Header_" & Parent_Key);
      Row : constant GGE.Table.Table_Row_Access := GGE.Table.Table_Row_Access (Row_Element);
      Column_Element : constant GGE.Pointer_To_Element_Class := new GGE.Table.Table_Heading_Type;
      Column : constant GGE.Table.Table_Heading_Access := GGE.Table.Table_Heading_Access (Column_Element);
   begin
      Column_Element.Dynamic;
      Column.Create (Row.all, Content => Variable);
   end Content_List_Add_Column;

   ----------------------------------------------------------------------------
   function Content_List_Add_Item (Object : in out GGB.Base_Type'Class; Parent_Key : String) return Integer is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("List_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row_Element : constant GGE.Pointer_To_Element_Class := new GGE.Table.Table_Row_Type;
      Row : constant GGE.Table.Table_Row_Access := GGE.Table.Table_Row_Access (Row_Element);
      Row_Index : constant Integer := Int_Value (Table.jQuery_Execute ("data('last_index')")) + 1;
   begin
      Row_Element.Dynamic;
      Row.Create (Table.all);
      Row.Class_Name ("content-list-item");

      Row.On_Click_Handler (Content_List_Click_Handler'Unrestricted_Access);
      Row.jQuery_Execute ("data('parent_key', """ & Parent_Key & """)");
      Row.jQuery_Execute ("data('row_index', " & To_String_Unsigned (Row_Index) & ")");

      App.Content_Text.Add_Element ("List_Item_" & Parent_Key & To_String_Unsigned (Row_Index), Row_Element);

      Table.jQuery_Execute ("data('last_index', " & To_String_Unsigned (Row_Index) & ")");

      return Row_Index;
   end Content_List_Add_Item;

   ----------------------------------------------------------------------------
   procedure Content_List_Add_Text (Object : in out GGB.Base_Type'Class; Value : String; Index : Integer; Parent_Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Current_Row_Element : constant GGE.Pointer_To_Element_Class :=
                                            App.Content_Text.Element ("List_Item_" & Parent_Key & To_String_Unsigned (Index));
      Current_Row : constant GGE.Table.Table_Row_Access := GGE.Table.Table_Row_Access (Current_Row_Element);

      Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
   begin
      Column.Dynamic;
      Column.Create (Current_Row.all, Content => Value);
   end Content_List_Add_Text;

   ----------------------------------------------------------------------------
   procedure Content_List_Create (Object : in out GGB.Base_Type'Class; Title : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent : constant GGE.Common.DIV_Access := new GGE.Common.DIV_Type;
      Table_Element : constant GGE.Pointer_To_Element_Class := new GGE.Table.Table_Type;
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row_Element : constant GGE.Pointer_To_Element_Class := new GGE.Table.Table_Row_Type;
      Row : constant GGE.Table.Table_Row_Access := GGE.Table.Table_Row_Access (Row_Element);
   begin
      Parent.Dynamic;
      Table_Element.Dynamic;
      Row_Element.Dynamic;

      Parent.Create (App.Content_Text);
      Parent.Class_Name ("content-list");

      Table.Create (Parent.all);
      Table.Class_Name ("content-list-table");
      Table.jQuery_Execute ("data('last_index', 0)");
      Table.jQuery_Execute ("data('selected_row', 0)");
      App.Content_Text.Add_Element ("List_" & Title, Table_Element);

      Row.Create (Table.all);
      App.Content_Text.Add_Element ("List_Header_" & Title, Row_Element);
      Row.Class_Name ("content-list-header");
   end Content_List_Create;

   ----------------------------------------------------------------------------
   function Content_List_Selected_Row (Object : in out GGB.Base_Type'Class; Parent_Key : String) return Integer is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("List_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Result : constant Integer := Int_Value (Table.jQuery_Execute ("data('selected_row')"));
   begin
      return Result;
   end Content_List_Selected_Row;

   ----------------------------------------------------------------------------
   procedure Content_Load_HTML (Object : in out GGB.Base_Type'Class; Text : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_HTML.Load_HTML (GS.HTML_Directory & Text);
   end Content_Load_HTML;

   ----------------------------------------------------------------------------
   function Content_Parent (Object : in out GGB.Base_Type'Class) return GGV.View_Access is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Content_Text'Access;
   end Content_Parent;

   ----------------------------------------------------------------------------
   procedure Content_Put_HTML (Object : in out GGB.Base_Type'Class; Text : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Text.Put_HTML (Text);
   end Content_Put_HTML;

   ----------------------------------------------------------------------------
   procedure Content_Put_Text (Object : in out GGB.Base_Type'Class; Text : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Text.Text (Text);
   end Content_Put_Text;

   ----------------------------------------------------------------------------
   procedure Content_Put_Title (Object : in out GGB.Base_Type'Class; Title : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Gui.Content_Clear_Text (Object);
      Gui.Content_Clear_HTML (Object);
      App.Content_Header.Text (Title);
   end Content_Put_Title;

   ----------------------------------------------------------------------------
   procedure Dialog_Buttons (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                             Key : String;
                             Button_Left_Text : String := "";
                             Button_Left_Handler : GGB.Action_Event := null;
                             Button_Right_Text : String := "";
                             Button_Right_Handler : GGB.Action_Event := null) is
      App : constant Gui.App_Access := Gui.App_Access (Object.Connection_Data);

      Table_Element : constant Gnoga.Gui.Element.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Key);
      Table : constant Gnoga.Gui.Element.Table.Table_Access := Gnoga.Gui.Element.Table.Table_Access (Table_Element);

      Row : constant Gnoga.Gui.Element.Table.Table_Row_Access := new Gnoga.Gui.Element.Table.Table_Row_Type;
      Column_Left : constant Gnoga.Gui.Element.Table.Table_Column_Access := new Gnoga.Gui.Element.Table.Table_Column_Type;
      Column_Right : constant Gnoga.Gui.Element.Table.Table_Column_Access := new Gnoga.Gui.Element.Table.Table_Column_Type;

      Button_Left : constant Gnoga.Gui.Element.Common.Button_Access := new Gnoga.Gui.Element.Common.Button_Type;
      Button_Right : constant Gnoga.Gui.Element.Common.Button_Access := new Gnoga.Gui.Element.Common.Button_Type;
   begin
      Row.Dynamic;
      Column_Left.Dynamic;
      Button_Left.Dynamic;
      if not Is_Empty (Button_Right_Text) then
         Column_Right.Dynamic;
         Button_Right.Dynamic;
      end if;

      Gui.Content_Group_Add_Space (Object, Key);
      Gui.Content_Group_Warning_Add (Object, "", "register-error", Key);

      Row.Create (Table.all);
      Column_Left.Create (Row.all);
      if not Is_Empty (Button_Right_Text) then
         Column_Right.Create (Row.all);
      end if;

      Button_Left.Create (Column_Left.all, Button_Left_Text);
      Button_Left.Style ("width", "80%");
      Button_Left.Style ("box-sizing", "border-box");
      Button_Left.On_Click_Handler (Button_Left_Handler);

      if not Is_Empty (Button_Right_Text) then
         Button_Right.Create (Column_Right.all, Button_Right_Text);
         Button_Right.Style ("width", "80%");
         Button_Right.Style ("box-sizing", "border-box");
         Button_Right.On_Click_Handler (Button_Right_Handler);
      end if;

   end Dialog_Buttons;

   ----------------------------------------------------------------------------
   procedure Dialog_Popup (Object : in out GGB.Base_Type'Class; Title : String; Content : String; Confirm_Text : String := "";
                       Cancel_Text : String := ""; On_Confirm : GGB.Action_Event := null; On_Cancel : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Dialog_Class : constant GGE.Pointer_To_Element_Class := new GGPJW.Dialog_Type;
      Dialog : constant GGPJW.Dialog_Access := GGPJW.Dialog_Access (Dialog_Class);
      Button : GGE.Pointer_To_Element_Class;
   begin
      Dialog.Create (Object.Parent.all, Title, Content, Width => 400, Height => 300);
      Dialog_Class.Dynamic;

      if On_Cancel /= null then
         Button := new GGE.Common.Button_Type;
         GGE.Common.Button_Access (Button).Create (Dialog.all, Cancel_Text);
         Button.On_Click_Handler (On_Cancel);
         Button.Dynamic;
         Button.Class_Name ("ui-button ui-corner-all");
         GGPJ.Position (Button.all, Target => Dialog.all, Using_My => "bottom", At_Target => "left+70 bottom-10");
      end if;

      if On_Confirm /= null then
         Button := new GGE.Common.Button_Type;
         GGE.Common.Button_Access (Button).Create (Dialog.all, Confirm_Text);
         Button.Dynamic;
         Button.Focus;
         Button.On_Click_Handler (On_Confirm);
         Button.Class_Name ("ui-button ui-corner-all");
         GGPJ.Position (Button.all, Target => Dialog.all, Using_My => "bottom", At_Target => "right-70 bottom-10");
      end if;

      Dialog.On_Close_Handler (Close_Dialog'Unrestricted_Access);
      App.Container.Add_Element ("dialog", Dialog_Class);
   end Dialog_Popup;

   ----------------------------------------------------------------------------
   procedure Put_User_Icon (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Instance.Set_User_Icon (User_Icon_File);
   end Put_User_Icon;

   ----------------------------------------------------------------------------
   procedure Footer_Set_Left_Text (Object : in out GGB.Base_Type'Class; Text : String := "") is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Footer_Instance.Set_Left_Text (Text);
   end Footer_Set_Left_Text;

   ----------------------------------------------------------------------------
   procedure Footer_Set_Right_Text (Object : in out GGB.Base_Type'Class; Text : String := "") is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Footer_Instance.Set_Right_Text (Text);
   end Footer_Set_Right_Text;

   ----------------------------------------------------------------------------
   function Get_Connection_Data (Object : in out GGB.Base_Type'Class; Key : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Custom_Data.Element (Key);
   end Get_Connection_Data;

   ----------------------------------------------------------------------------
   procedure Header_Application_Menu_Add (Key : String; Name : String; Parent_Key : String; On_Click : GGB.Action_Event) is
      Unique_ID : Integer;
   begin
      Unique_ID := v22.Gui.Header.Add_Child (Header_Get (Parent_Key), Name, On_Click);
      Header_Set (Key, Unique_ID);
   end Header_Application_Menu_Add;

   ----------------------------------------------------------------------------
   procedure Header_User_Menu_Add (Object : in out GGB.Base_Type'Class; Name : String; On_Click : GGB.Action_Event) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Instance.Add_Element (Name, On_Click);
   end Header_User_Menu_Add;

   ----------------------------------------------------------------------------
   procedure Header_Notify_Menu_Click (Object : in out GGB.Base_Type'Class; Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Instance.Notify_Menu_Click (Header_Get (Key));
      App.Content_Header.Inner_HTML ("");
      App.Content_Text.Inner_HTML ("");
      App.Main_Menu_Instance.Clear;
      App.Main_Menu_Dict.Clear;
   end Header_Notify_Menu_Click;

   ----------------------------------------------------------------------------
   procedure Header_Set_Root (Key : String; Name : String; On_Click : GGB.Action_Event) is
      Unique_ID : Integer;
   begin
      Unique_ID := v22.Gui.Header.Set_Root (Name, On_Click);
      Header_Set (Key, Unique_ID);
      ID_Main := Unique_ID;
   end Header_Set_Root;

   ----------------------------------------------------------------------------
   procedure Header_Notify_User_Menu_Click (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Instance.Notify_User_Menu_Click;
   end Header_Notify_User_Menu_Click;

   ----------------------------------------------------------------------------
   procedure Launch_Web (Object : in out GGB.Base_Type'Class; URL : String) is
   begin
      Object.jQuery_Execute ("gnoga_web = open('" & URL & "', '_blank')");
   end Launch_Web;

   ---------------------------------------------------------------------------
   procedure List (Object : in out GGB.Base_Type'Class;
                   DB : in out GSD.Connection'Class;
                   Key : String;
                   Title : String;
                   Table : String;
                   Columns_Names : String;
                   Columns_Titles : String;
                   Condition : String := "") is
      Query : String := "SELECT " & Replace (Columns_Names, ND, "") & " FROM " & Table & " " & Condition;
      Columns_Names_Count : constant Natural := Field_Count (Columns_Names, VD);
      Columns_Titles_Count : constant Natural := Field_Count (Columns_Titles, VD);
      Column_Name : String;
      Column_Key_Last_Value : String := "";
      Lines_Count : Natural := 0;
   begin
      Msg.Debug ("Gui.List > Start");
      Gui.Content_Put_Title (Object, Title);
      Gui.Content_List_Create (Object, Key);
      --  Table header
      if (Columns_Names_Count = Columns_Titles_Count) then
         for Index_Columns in 1..Columns_Names_Count loop
            Gui.Content_List_Add_Column (Object, Field_By_Index (Columns_Titles, Index_Columns, VD), Key);
         end loop;
      else
          Msg.Error ("Gui.List > Fields number mismatch between Columns_Names and Column_Titles");
      end if;
      --  Table elements
      declare
         package GSD renames Gnoga.Server.Database;
         RS : GSD.Recordset'Class := DB.Query (Query);
         Index_Lines : Integer := 0;
      begin
         --  Initialize _First key for empty list
         Gui.Set_Connection_Data (Object, Key & "_First", "");
         while RS.Next loop
            Lines_Count := Lines_Count + 1;
            Index_Lines := Gui.Content_List_Add_Item (Object, Key);
            --  Msg.Debug ("Index_Lines: " & To_String (Index_Lines));
            for Index_Columns in 1..RS.Number_Of_Fields loop
               Column_Name := Field_By_Index (Columns_Names, Index_Columns, VD);
               if Index  (Column_Name, ND) > 0 then
                  Column_Key_Last_Value := RS.Field_Value (Index_Columns);
                  -- Save the tagged (ND) column key for further seeking the record
                  Gui.Set_Connection_Data (Object, Key & "_" & To_String_Unsigned (Index_Lines), Column_Key_Last_Value);
                  Msg.Debug ("Key/Value: " & Key & "_" & To_String_Unsigned (Index_Lines) & "/" & Column_Key_Last_Value);
                  --  Save the first displayed column key for browsing list
                  if Lines_Count = 1 then
                     Gui.Set_Connection_Data (Object, Key & "_First", Column_Key_Last_Value);
                     Msg.Debug ("Key/First: " & Key & "_First/" & Column_Key_Last_Value);
                  end if;
               end if;
               Gui.Content_List_Add_Text (Object, RS.Field_Value (Index_Columns), Index_Lines, Key);
               --  Msg.Debug (RS.Field_Value (Index_Columns));
               --  Msg.Debug ("Index_Columns: " & To_String (Index_Columns));
            end loop;
         end loop;
         RS.Close;
         --  Save the last displayed column key for browsing list
         Gui.Set_Connection_Data (Object, Key & "_Last", Column_Key_Last_Value);
         Msg.Debug ("Key/Last: " & Key & "_Last/" & Column_Key_Last_Value);
         --  Gui.Set_Connection_Data (Object, Key & "_Max", To_String_Unsigned (Lines_Count));
         --  Msg.Debug ("Key/Max: " & Key & "_Max/" & To_String_Unsigned (Lines_Count));
      end;
   end List;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Add_Delimiter_Above (Object : in out GGB.Base_Type'Class; Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Add_Delimiter_Above (Main_Menu_Get (Object, Key));
   end Main_Menu_Add_Delimiter_Above;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Add_Element (Object : in out GGB.Base_Type'Class;
                                    Key : String; Name : String; Icon_SRC : String;
                                    On_Click : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Unique_ID : Integer;
   begin
      Unique_ID := App.Main_Menu_Instance.Add_Element (Name, Image_Gnoga_Root & Icon_SRC, On_Click);
      Main_Menu_Set (Object, Key, Unique_ID);
   end Main_Menu_Add_Element;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Add_Sub_Element (Object : in out GGB.Base_Type'Class;
                                        Key : String; Name : String; Parent_Key : String; On_Click : GGB.Action_Event) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Unique_ID : Integer;
   begin
      Unique_ID := App.Main_Menu_Instance.Add_Sub_Element (Name, Main_Menu_Get (Object, Parent_Key), On_Click);
      Main_Menu_Set (Object, Key, Unique_ID);
   end Main_Menu_Add_Sub_Element;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Clear (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Clear;
      --App.Header_Instance.Open_Menu (ID_Main);
      App.Header_Instance.Close_User_Menu;
      App.Main_Menu_Instance.Close_Menu;
   end Main_Menu_Clear;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Disable_Shortcuts (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      v22.Gui.Main_Menu.Disable_Shortcuts (App.Main_Menu_Instance);
   end Main_Menu_Disable_Shortcuts;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Enable_Shortcuts (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      v22.Gui.Main_Menu.Enable_Shortcuts (App.Main_Menu_Instance);
   end Main_Menu_Enable_Shortcuts;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Load (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Load;
   end Main_Menu_Load;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Notify_Sub_Element_Click (Object : in out GGB.Base_Type'Class; Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Notify_Sub_Element_Click (Main_Menu_Get (Object, Key));
   end Main_Menu_Notify_Sub_Element_Click;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Set_Clickable (Object : in out GGB.Base_Type'Class; Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Set_Clickable (Main_Menu_Get (Object, Key));
   end Main_Menu_Set_Clickable;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Set_Unclickable (Object : in out GGB.Base_Type'Class; Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Set_Unclickable (Main_Menu_Get (Object, Key));
   end Main_Menu_Set_Unclickable;

   ----------------------------------------------------------------------------
   procedure Print (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Window.Print;
   end Print;

   ----------------------------------------------------------------------------
   procedure Set_Application_Icon (Icon_File : String) is
   begin
      Application_Icon_File := Image_Gnoga_Root & Icon_File;
   end Set_Application_Icon;

   ----------------------------------------------------------------------------
   procedure Set_Browser_Icon (Icon_File : String) is
   begin
      Gnoga.Application.Favicon (Icon_File);
   end Set_Browser_Icon;

   ----------------------------------------------------------------------------
   procedure Set_Browser_Title (Object : in out GGB.Base_Type'Class; Title : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Window.Document.Title (Title);
   end Set_Browser_Title;

   ----------------------------------------------------------------------------
   procedure Set_Connection_Data
     (Object : in out GGB.Base_Type'Class; Key : String; Value : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.Custom_Data.Contains (Key) then
         App.Custom_Data.Replace (Key, Value);
      else
         App.Custom_Data.Insert (Key, Value);
      end if;
   end Set_Connection_Data;

   ----------------------------------------------------------------------------
   procedure Set_Debug (Switch : On_Off) is
   begin
      if Switch = On then
         Debug_Control := On;
      else
         Debug_Control := Off;
         Gnoga.Log_To_File (Prg.Start_Dir & "/" & Prg.Name & "-gnoga.log");
      end if;
   end Set_Debug;

   ----------------------------------------------------------------------------
   procedure Set_Login (Switch : On_Off) is
   begin
      Access_Control := Switch;
   end Set_Login;

   ----------------------------------------------------------------------------
   procedure Set_User_Icon (Icon_File : String) is
   begin
      User_Icon_File := Image_Gnoga_Root & Icon_File;
   end Set_User_Icon;

   ----------------------------------------------------------------------------
   procedure Set_User_Icon (Object : in out GGB.Base_Type'Class; Icon_SRC : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Instance.Set_User_Icon (Icon_SRC);
   end Set_User_Icon;

   ----------------------------------------------------------------------------
   procedure Set_User_Name (Object : in out GGB.Base_Type'Class; Name : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Instance.Set_User_Name (Name);
   end Set_User_Name;

   ----------------------------------------------------------------------------
   procedure Setup (On_User_Connect : GGB.Action_Event;
                    Host : String := "";
                    Port : Integer := 8_080;
                    Boot : in String := "boot_jqueryui.html";
                    Title : String := "";
                    Server_Closed_Content : String := "Server closed.") is
   begin
      Gnoga.Application.Title (Title);
      Gnoga.Application.HTML_On_Close (Server_Closed_Content);
      Gnoga.Application.Multi_Connect.Initialize (Host => Host, Port => Port, Boot => Boot, Verbose => (Debug_Control=On));
      Gnoga.Application.Multi_Connect.On_Connect_Handler (On_Connect'Unrestricted_Access);
      On_Custom_Connect := On_User_Connect;
   end Setup;

   ----------------------------------------------------------------------------
   procedure User_Logout (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Msg.Info ("Gui.User_Logout > User " & Get_Connection_Data (Object, "User_Login") &
               " disconnected, connection time: " & Prg.Duration_Stamp (Prg.Start_Time));

      Put_Login_Form (Object);
      App.Header_Instance.Set_User_Name ("");
      App.Header_Instance.Clear;
      App.Header_Instance.Close_Menu;
      App.Header_Instance.Close_User_Menu;
      App.Header_Instance.Clear;
      App.Main_Menu_Instance.Clear;
      App.Main_Menu_Dict.Clear;

      App.User_Logged_In := False;
   end User_Logout;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Content_Group_Item_Add (Object : in out GGB.Base_Type'Class; Item : GGE.Pointer_To_Element_Class;
                                     Name : String; Parent_Key : String; On_Change : GGB.Action_Event := null) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);

      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;
      First_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Second_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
   begin
      Row.Dynamic;
      First_Column.Dynamic;
      Second_Column.Dynamic;

      Row.Create (Table.all);
      First_Column.Create (Row.all, Name);
      Second_Column.Create (Row.all);
      Second_Column.Style ("white-space", "nowrap");

      Item.Place_Inside_Top_Of (Second_Column.all);
      Item.Style ("width", "100%");
      Item.Style ("box-sizing", "border-box");
      Item.On_Focus_In_Handler (Main_Menu_Disable_Shortcuts'Unrestricted_Access);
      Item.On_Focus_Out_Handler (Main_Menu_Enable_Shortcuts'Unrestricted_Access);
      Item.On_Change_Handler (On_Change);

      --  Not its true parent, but a lot easier to access
      App.Content_Text.Add_Element (Name, Item);
   end Content_Group_Item_Add;

   ----------------------------------------------------------------------------
   procedure Content_List_Click_Handler (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent_Key : constant String := Object.jQuery_Execute ("data('parent_key')");
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("List_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row_Index : constant Integer := Int_Value (Object.jQuery_Execute ("data('row_index')"));
      Row : constant GGE.Pointer_To_Element_Class :=  App.Content_Text.Element ("List_Item_" & Parent_Key & To_String_Unsigned (Row_Index));
      Previous_Row_Index : constant Integer := Int_Value (Table.jQuery_Execute ("data('selected_row')"));
      Previous_Row       : GGE.Pointer_To_Element_Class;
   begin
      if Previous_Row_Index = Row_Index then
         Row.Remove_Class ("content-list-item-selected");
         Table.jQuery_Execute ("data('selected_row', 0)");
      else
         Row.Add_Class ("content-list-item-selected");
         Table.jQuery_Execute ("data('selected_row', " & To_String_Unsigned (Row_Index) & ")");
      end if;
      if Previous_Row_Index /= 0 then
         Previous_Row := App.Content_Text.Element ("List_Item_" & Parent_Key & To_String_Unsigned (Previous_Row_Index));
         Previous_Row.Remove_Class ("content-list-item-selected");
      end if;
   end Content_List_Click_Handler;

   ----------------------------------------------------------------------------
   function Header_Get (Key : String) return Integer is
   begin
      return Header_Dict.Element (Key);
   end Header_Get;

   ----------------------------------------------------------------------------
   procedure Header_Set (Key : String; Value : Integer) is
   begin
      Header_Dict.Insert (Key, Value);
   end Header_Set;

   ----------------------------------------------------------------------------
   function Main_Menu_Get (Object : in out GGB.Base_Type'Class; Key : String) return Integer is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Main_Menu_Dict.Element (Key);
   end Main_Menu_Get;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Set (Object : in out GGB.Base_Type'Class; Key : String; Value : Integer) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Dict.Insert (Key, Value);
   end Main_Menu_Set;

   ----------------------------------------------------------------------------
   procedure On_Connect (Screen : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection : access Gnoga.Application.Multi_Connect.Connection_Holder_Type) is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Screen.Connection_Data (App);
      Screen.Buffer_Connection (True);
      App.Window := Screen'Unchecked_Access;

      App.Window.On_Character_Handler (On_Key_Pressed'Unrestricted_Access);
      App.Window.On_Key_Down_Handler (On_Key_Down'Unrestricted_Access);

   --  procedure On_Key_Down_Handler (Object  : in out Base_Type; Handler : in Keyboard_Event);
   --  procedure On_Key_Up_Handler (Object : in out Base_Type; Handler : in Keyboard_Event);
   --  procedure On_Key_Press_Handler (Object : in out Base_Type; Handler : in Keyboard_Event);

   --  procedure Fire_On_Key_Down (Object : in out Base_Type; Event  : in Keyboard_Event_Record);
   --  procedure Fire_On_Key_Up (Object : in out Base_Type; Event  : in Keyboard_Event_Record);
   --  procedure Fire_On_Key_Press (Object : in out Base_Type; Event : in Keyboard_Event_Record);

      App.Container.Create (Screen);
      App.Container.Style ("display", "flex");

      --  Containers
      App.Container.Style ("width", "100%");
      App.Container.Style ("height", "100%");

      App.Header_Parent.Create (App.Container);
      App.Header_Parent.Class_Name ("header");

      App.Main_Menu_Parent.Create (App.Container);
      App.Main_Menu_Parent.Class_Name ("main-menu");

      App.Content.Create (App.Container);
      App.Content.Class_Name ("content-container");

      App.Footer_Parent.Create (App.Container);
      App.Footer_Parent.Class_Name ("footer");

      --  Content
      App.Content_Header.Create (App.Content);
      App.Content_Header.Class_Name ("content-header");

      App.Content_Text.Create (App.Content);
      App.Content_Text.Class_Name ("content-text");

      App.Content_HTML.Create (App.Content);
      App.Content_HTML.Class_Name ("content-html");

      --  Header
      App.Header_Instance.Create (App.Header_Parent, On_Logo'Unrestricted_Access, On_User'Unrestricted_Access);
      App.Header_Instance.Set_App_Icon (Application_Icon_File);
      App.Header_Instance.Set_User_Icon (User_Icon_File);

      --  Footer
      App.Footer_Instance.Create (App.Footer_Parent);

      --  Main_Menu
      App.Main_Menu_Instance.Create (App.Main_Menu_Parent, On_Tool_Bar_Expand'Unrestricted_Access, On_Main_Menu_Callback'Unrestricted_Access);

      if Access_Control = On then
         App.User_Logged_In := False;
         Put_Login_Form (App.Container);
      else
         App.User_Logged_In := True;
         App.Header_Instance.Set_Menu (ID_Main);
      end if;

      On_Custom_Connect (App.Container);
   end On_Connect;

   ----------------------------------------------------------------------------
   procedure On_Key_Down (Object : in out GGB.Base_Type'Class; Event : Keyboard_Event_Record) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Special_Key : String := "";
   begin
      if Event.Alt then
         Special_Key := "[Alt]";
      elsif Event.Control then
         Special_Key := "[Ctrl]";
      elsif Event.Shift then
         Special_Key := "[Shift]";
      elsif Event.Meta then
         Special_Key := "[Meta]";
      end if;

      Msg.Debug ("Message: " & From_Latin_1 (Keyboard_Message_Type'Image (Event.Message)));
      Msg.Debug ("Special_Key: " & Special_Key);
      Msg.Debug ("Key_Code: " & To_String (Event.Key_Code));

   end On_Key_Down;

   --  type Keyboard_Message_Type is (Unknown, Key_Down, Key_Up, Key_Press);
   --
   --  type Keyboard_Event_Record is record
   --     Message  : Keyboard_Message_Type := Unknown;
   --     Key_Code : Integer;
   --     Key_Char : Wide_Character; (1)
   --     Alt      : Boolean               := False;
   --     Control  : Boolean               := False;
   --     Shift    : Boolean               := False;
   --     Meta     : Boolean               := False;
   --  end record;
   --
   -- (1) Allways return NUL with From_Latin_1 (Wide_Character'Image (Event.Key_Char))

   ----------------------------------------------------------------------------
   procedure On_Key_Pressed (Object : in out GGB.Base_Type'Class; Char : Character) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Msg.Debug ("Key_Ascii: " & To_String (Char));
      Msg.Debug ("Key_Ascii_Val: " & From_Latin_1 (Integer'Image (Character'Pos (Char))));
      App.Main_Menu_Instance.Notify_Key_Pressed (Char);
      App.Header_Instance.Close_Menu;
      App.Header_Instance.Close_User_Menu;
   end On_Key_Pressed;

   ----------------------------------------------------------------------------
   procedure On_Main_Menu_Callback (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Notify_Element_Click (Object);
      App.Header_Instance.Close_Menu;
      App.Header_Instance.Close_User_Menu;
   end On_Main_Menu_Callback;

   ----------------------------------------------------------------------------
   procedure On_Logo (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.User_Logged_In then
         if App.Header_Instance.Is_Menu_Open then
            App.Header_Instance.Close_Menu;
         else
            App.Main_Menu_Instance.Clear;
            App.Header_Instance.Open_Menu (ID_Main);
            App.Header_Instance.Close_User_Menu;
            App.Main_Menu_Instance.Close_Menu;
         end if;
      end if;
   end On_Logo;

   ----------------------------------------------------------------------------
   procedure On_Tool_Bar_Expand (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Notify_Resize;
   end On_Tool_Bar_Expand;

   ----------------------------------------------------------------------------
   procedure On_User (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.User_Logged_In then
         if App.Header_Instance.Is_User_Menu_Open then
            App.Header_Instance.Close_User_Menu;
         else
            App.Header_Instance.Open_User_Menu;
            App.Header_Instance.Close_Menu;
            App.Main_Menu_Instance.Close_Menu;
         end if;
      end if;
   end On_User;

   ----------------------------------------------------------------------------
   procedure Password_Forgotten (Object : in out GGB.Base_Type'Class) is
      App  : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Msg.Info ("Gui.Password_Forgotten > Sending email with temporary and time limited password");
   end Password_Forgotten;

   ----------------------------------------------------------------------------
   procedure Put_Login_Form (Object : in out GGB.Base_Type'Class) is
   --  Display login form
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Content_Clear_Title (Object);
      Content_Clear_Text (Object);

      Content_Group_Create (Object, Login_Group_Key);
      Content_Group_Add_Space (Object, Login_Group_Key);
      Content_Group_Text_Add (Object, "Identifiant", Login_Group_Key);
      Content_Group_Password_Add (Object, "Mot de passe", Login_Group_Key);

      Put_Login_Buttons (Object);
   end Put_Login_Form;

   ----------------------------------------------------------------------------
   procedure Put_Login_Buttons (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Login_Group_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;

      First_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Second_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;

      Password_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
      Submit_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
   begin
      Row.Dynamic;
      First_Column.Dynamic;
      Second_Column.Dynamic;
      Password_Button.Dynamic;
      Submit_Button.Dynamic;

      Content_Group_Add_Space (Object, Login_Group_Key);
      Content_Group_Warning_Add (Object, "", "login-error", Login_Group_Key);

      Row.Create (Table.all);
      First_Column.Create (Row.all);
      Second_Column.Create (Row.all);

      Password_Button.Create (First_Column.all, "Mot de passe oublié...");
      Password_Button.Class_Name ("content-group-link");
      Password_Button.Style ("white-space", "nowrap");
      Password_Button.On_Click_Handler (Password_Forgotten'Unrestricted_Access);

      Submit_Button.Create (Second_Column.all, "Connexion");
      Submit_Button.Style ("width", "100%");
      Submit_Button.Style ("box-sizing", "border-box");
      Submit_Button.On_Click_Handler (User_Login'Unrestricted_Access);
   end Put_Login_Buttons;

   ----------------------------------------------------------------------------
   procedure Put_Login_Message (Object : in out GGB.Base_Type'Class; Error : String) is
   begin
      Content_Group_Warning_Set (Object, "login-error", Error);
   end Put_Login_Message;

   ----------------------------------------------------------------------------
   procedure User_Login (Object : in out GGB.Base_Type'Class) is
      App  : constant App_Access := App_Access (Object.Connection_Data);
      Login : constant String := Gui.Content_Group_Text_Get (Object, "Identifiant");
      Password : constant String := Content_Group_Password_Get (Object, "Mot de passe");
      --Identity : User_Data;
      DBT : Sql.Database_Line_Type;

      DB_Result : String;
      DB_First_Name : String;
      DB_Last_Name : String;
      DB_Password : String;
   begin
      Msg.Info ("Gui.User_Login > New user on logging screen");

      DBT := Sql.Properties (Sql.Get_Database_Main);
      if DBT.Brand = Sql.MySQL then
         DB_Result := Sql.Read (DBT.DBM, "Sys_Users", "First_Name,Last_Name,Password", "WHERE Login = '" & Login & "'");
      elsif DBT.Brand = Sql.SQLite then
         DB_Result := Sql.Read (DBT.DBS, "Sys_Users", "First_Name,Last_Name,Password", "WHERE Login = '" & Login & "'");
      else
          Msg.Error ("Gui.User_Login > Properties not found for " & Sql.Get_Database_Main);
      end if;

      if not Is_Empty (DB_Result) then
         DB_First_Name := Field_By_Index (DB_Result, 1, CD);
         DB_Last_Name := Field_By_Index (DB_Result, 2, CD);
         DB_Password := Field_By_Index (DB_Result, 3, CD);
         if DB_Password = From_Latin_1 (GNAT.SHA512.Digest (To_Latin_1 (Password))) then
            --  Load user properties
            App.Header_Instance.Set_Menu (ID_Main);
            App.User_Logged_In := True;
            App.User_Logged_Since := AC.Clock;
            Set_Connection_Data (Object, "User_Login", Login);
            Set_Connection_Data (Object, "First_Name", DB_First_Name);
            Set_Connection_Data (Object, "Last_Login", DB_Last_Name);

            Gui.Set_User_Name (Object, DB_First_Name & " " & DB_Last_Name);
            Msg.Info ("Gui.User_Login > User " & Login & " connected");
         else
            Put_Login_Message (Object, "Mot de passe incorrect");
            Msg.Info ("Gui.User_Login > User " & Login & " found but password not match, connection rejected");
         end if;
      else
         Put_Login_Message (Object, "Identifiant incorrect");
         Msg.Info ("Gui.User_Login > User " & Login & " not found, connection rejected");
      end if;

   end User_Login;

-------------------------------------------------------------------------------
end v22.Gui;
-------------------------------------------------------------------------------
