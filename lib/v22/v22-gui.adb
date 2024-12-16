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

with Ada.Calendar; use Ada.Calendar;
with Ada.Containers.Hashed_Maps;
with Ada.Numerics.Elementary_Functions; -- For plotting tests

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
with Gnoga.Gui.Element.Canvas.Context_2D.Plotting;
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
   package GGECCP renames Gnoga.Gui.Element.Canvas.Context_2D.Plotting;

   package Math renames Ada.Numerics.Elementary_Functions;  -- For plotting tests

   ----------------------------------------------------------------------------
   --  PRIVATE DECLARATIONS
   ----------------------------------------------------------------------------

   package Integer_Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => String, Element_Type => Integer, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   package Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => String, Element_Type => String, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   ID_Main : Integer; --  Root menu ID
   On_Custom_Connect : GGB.Action_Event;
   Header_Dict : Integer_Dictionary.Map;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      User_Logged_In : Boolean  := False;           --  Connection login flag
      User_Logged_Since : AC.Time;                  --  Connection time
      User_Display_In_Progress : Boolean := False;  --  Flag to block user input when user display in progress
      Custom_Data : Dictionary.Map;                 --  Connection level free to use dictionnary
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

   ----------------------------------------------------------------------------
   --  PROCEDURES & FUNCTIONS
   ----------------------------------------------------------------------------

   package Connection is
      procedure Put_Login_Form (Object : in out GGB.Base_Type'Class);
      procedure Put_Login_Buttons (Object : in out GGB.Base_Type'Class);
      procedure Put_Login_Message (Object : in out GGB.Base_Type'Class; Error : String);
      procedure On_Login_Connection (Object : in out GGB.Base_Type'Class);

      procedure Put_Password_Forgotten_Form (Object : in out GGB.Base_Type'Class);
      procedure Put_Password_Forgotten_Buttons (Object : in out GGB.Base_Type'Class);
      procedure Put_Password_Forgotten_Message (Object : in out GGB.Base_Type'Class; Error : String);
      procedure On_Password_Forgotten_Send_Email (Object : in out GGB.Base_Type'Class);

      procedure Put_New_Password_Form (Object : in out GGB.Base_Type'Class);
      procedure Put_New_Password_Buttons (Object : in out GGB.Base_Type'Class);
      procedure Put_New_Password_Message (Object : in out GGB.Base_Type'Class; Error : String);
      procedure On_New_Password_Update (Object : in out GGB.Base_Type'Class);

      procedure On_Dialog_Confirm (Object : in out GGB.Base_Type'Class);
   end Connection;
   package body Connection is separate;

   ----------------------------------------------------------------------------
   procedure Content_Group_Item_Add (Object : in out GGB.Base_Type'Class; Item : GGE.Pointer_To_Element_Class;
                                     Name : String; Parent_Key : String; On_Change : GGB.Action_Event := null);
   procedure Content_List_Click_Handler (Object : in out GGB.Base_Type'Class);

   function Header_Get (Key : String) return Integer;
   procedure Header_Set (Key : String; Value : Integer);

   function Int_Value is new Integer_Value (Integer); --  Error: generic subprogram cannot be called

   function Main_Menu_Get (Object : in out GGB.Base_Type'Class; Key : String) return Integer;
   procedure Main_Menu_List (Object : in out GGB.Base_Type'Class);
   procedure Main_Menu_Set (Object : in out GGB.Base_Type'Class; Key : String; Value : Integer);
   procedure On_Connect (Screen : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection : access Gnoga.Application.Multi_Connect.Connection_Holder_Type);

   procedure On_Key_Down (Object : in out GGB.Base_Type'Class; Event : Keyboard_Event_Record);
   procedure On_Key_Pressed (Object : in out GGB.Base_Type'Class; Char : Character);

   procedure On_Main_Menu_Callback (Object : in out GGB.Base_Type'Class);
   procedure On_Logo (Object : in out GGB.Base_Type'Class);
   procedure On_Tool_Bar_Expand (Object : in out GGB.Base_Type'Class);
   procedure On_User (Object : in out GGB.Base_Type'Class);

   procedure On_Pop_Up_Ok (Object : in out GGB.Base_Type'Class);

   --pragma Warnings (Off, "subunit * in file", Reason => "TBD");
   --pragma Warnings (On, "subunit * in file");

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Close_Dialog (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Dialog_Class : constant GGE.Pointer_To_Element_Class := App.Container.Element ("dialog");
      Dialog : constant GGPJW.Dialog_Access := GGPJW.Dialog_Access (Dialog_Class);
   begin
      Dialog.Remove;
   end Close_Dialog;

   ----------------------------------------------------------------------------
   procedure Connection_Data_Clear (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Custom_Data.Clear;
   end Connection_Data_Clear;

   ----------------------------------------------------------------------------
   procedure Connection_Data_Delete (Object : in out GGB.Base_Type'Class; Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Custom_Data.Exclude (Key);
      --  if App.Custom_Data.Contains (Key) then
      --     App.Custom_Data.Delete (Key);
      --  end if;
   end Connection_Data_Delete;

   ----------------------------------------------------------------------------
   function Connection_Data_Get (Object : in out GGB.Base_Type'Class; Key : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Custom_Data.Element (Key);
   exception
      --  raised CONSTRAINT_ERROR : v22.Gui.Dictionary.Element: no element available because key not in map
      when Constraint_Error =>
         return "";
   end Connection_Data_Get;

   ----------------------------------------------------------------------------
   procedure Connection_Data_List (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      for M in App.Custom_Data.Iterate loop
         Msg.Debug (Dictionary.Key (M) & ": " & App.Custom_Data (M));
      end loop;
   end Connection_Data_List;

   ----------------------------------------------------------------------------
   procedure Connection_Data_Set
     (Object : in out GGB.Base_Type'Class; Key : String; Value : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.Custom_Data.Contains (Key) then
         App.Custom_Data.Replace (Key, Value);
      else
         App.Custom_Data.Insert (Key, Value);
      end if;
   end Connection_Data_Set;

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

   --     procedure Add_Option
   --    (Element  : in out Selection_Type;
   --     Value    : in     String;
   --     Text     : in     String;
   --     Index    : in     Natural := 0;
   --     Selected : in     Boolean := False;
   --     Disabled : in     Boolean := False;
   --     ID       : in     String  := "");
   --  --  Call to add options to the Selection_Type Element. If Index is 0
   --  --  adds to end of list. Otherwise inserts at Index.

   ---------------------------------------------------------------------------
   procedure Content_Group_Drop_Down_Menu_Add_Option_From_DB_By_Key (Object : in out GGB.Base_Type'Class;
                                                   Key : String;
                                                   DB : in out GSD.Connection'Class;
                                                   Table : String;
                                                   Column_Key : String;
                                                   Column_Display : String;
                                                   Match_Value : String := "";
                                                   No_Display_Raw_One : Boolean := False) is
      DB_Query : String := "SELECT " & Column_Key & ", " & Column_Display & " FROM " & Table;
   begin
      Msg.Debug ("Gui.Content_Group_Drop_Down_Menu_Add_Option_From_DB_By_Key > DB_Query: " & DB_Query);
      Msg.Debug ("Gui.Content_Group_Drop_Down_Menu_Add_Option_From_DB_By_Key > Column_ID: " & Match_Value);
      declare
         package GSD renames Gnoga.Server.Database;
         RS : GSD.Recordset'Class := DB.Query (DB_Query);
      begin
         while RS.Next loop
            -- Don't display if No_Display_Raw = True and Id = 1
            if (No_Display_Raw_One and RS.Field_Value (1) /= "1") or not No_Display_Raw_One then
               Msg.Debug ((No_Display_Raw_One));
               Msg.Debug ((No_Display_Raw_One and RS.Field_Value (1) /= "1"));
               Msg.Debug ("Content_Group_Drop_Down_Menu_Add_Option_From_DB_By_Key > RS.Field_Value (1 & 2): " & RS.Field_Value (1) & " " & RS.Field_Value (2));
               Gui.Content_Group_Drop_Down_Menu_Add_Option (Object, Key, RS.Field_Value (2), Enabled => (RS.Field_Value (1) = Match_Value));
            end if;
         end loop;
         RS.Close;
      end;
   end Content_Group_Drop_Down_Menu_Add_Option_From_DB_By_Key;

   ---------------------------------------------------------------------------
   function Content_Group_Drop_Down_Menu_Get_Key_From_DB (Object : in out GGB.Base_Type'Class;
                                                         Key : String;
                                                         DB : in out GSD.Connection'Class;
                                                         Table : String;
                                                         Column_Key : String;
                                                         Column_Display : String) return String is
      Menu_Value : String := Content_Group_Drop_Down_Menu_Get (Object, Key);
      DB_Query : String := Column_Key;
      DB_Where : String := "WHERE " & Column_Display & " = '" & Sql.Escape_String (Menu_Value) & "'";
      DB_Result : String;
   begin
      Msg.Debug ("Gui.Content_Group_Drop_Down_Menu_Get_Key_From_DB > DB_Query: " & DB_Query);
      DB_Result := Sql.Read (DB, Table, DB_Query, DB_Where);
      Msg.Debug ("Gui.Content_Group_Drop_Down_Menu_Get_Key_From_DB > DB_Result: " & DB_Result);
      return DB_Result;
   end Content_Group_Drop_Down_Menu_Get_Key_From_DB;

   ----------------------------------------------------------------------------
   procedure Content_Group_Drop_Down_Menu_Empty_Options (Object : in out GGB.Base_Type'Class; Name : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Selection_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Selection : constant GGE.Form.Selection_Access := GGE.Form.Selection_Access (Selection_Element);
   begin
      Selection.Empty_Options;
   end Content_Group_Drop_Down_Menu_Empty_Options;

   ----------------------------------------------------------------------------
   function Content_Group_Drop_Down_Menu_Get (Object : in out GGB.Base_Type'Class; Name : String) return String is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Selection_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Selection : constant GGE.Form.Selection_Access := GGE.Form.Selection_Access (Selection_Element);
   begin
      return Selection.Value;
   end Content_Group_Drop_Down_Menu_Get;

   --  procedure Value
   --    (Element : in out Selection_Type;
   --     Index   : in     Positive;
   --     Value   : in     String);

   ----------------------------------------------------------------------------
   procedure Content_Group_Drop_Down_Menu_Set_Selected (Object : in out GGB.Base_Type'Class; Name : String; Index : in Positive;
                                                       Value : in Boolean := True) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Selection_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Selection : constant GGE.Form.Selection_Access := GGE.Form.Selection_Access (Selection_Element);
   begin
      Selection.Selected (Index => Index, Value => Value);
   end Content_Group_Drop_Down_Menu_Set_Selected;

   --   procedure Selected
   --  (Element : in out Selection_Type;
   --   Index   : in     Positive;
   --   Value   : in     Boolean := True);

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
   function Content_List_Add_Item (Object : in out GGB.Base_Type'Class; Parent_Key : String; Row_Index : Integer; Style : String := "") return Integer is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("List_" & Parent_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row_Element : constant GGE.Pointer_To_Element_Class := new GGE.Table.Table_Row_Type;
      Row : constant GGE.Table.Table_Row_Access := GGE.Table.Table_Row_Access (Row_Element);
      -- Row_Index : constant Integer := Int_Value (Table.jQuery_Execute ("data('last_index')")) + 1;
   begin
      Row_Element.Dynamic;
      Row.Create (Table.all);

      Row.Class_Name ("content-list-item");
      if not Is_Empty (Style) then
         Row.Style ("text-align", Style);
      end if;

      Row.On_Click_Handler (Content_List_Click_Handler'Unrestricted_Access);
      Row.jQuery_Execute ("data('parent_key', """ & Parent_Key & """)");
      Row.jQuery_Execute ("data('row_index', " & To_String_Unsigned (Row_Index) & ")");

      App.Content_Text.Add_Element ("List_Item_" & Parent_Key & To_String_Unsigned (Row_Index), Row_Element);
      Table.jQuery_Execute ("data('last_index', " & To_String_Unsigned (Row_Index) & ")");

      return Row_Index;
   end Content_List_Add_Item;

   ----------------------------------------------------------------------------
   procedure Content_List_Add_Text (Object : in out GGB.Base_Type'Class;
                                    Value : String;
                                    Index : Integer;
                                    Parent_Key : String;
                                    Style : String := "") is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Current_Row_Element : constant GGE.Pointer_To_Element_Class :=
                                            App.Content_Text.Element ("List_Item_" & Parent_Key & To_String_Unsigned (Index));
      Current_Row : constant GGE.Table.Table_Row_Access := GGE.Table.Table_Row_Access (Current_Row_Element);
      Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
   begin
      Column.Dynamic;
      Column.Create (Current_Row.all, Content => Value);
      if not Is_Empty (Style) then
         Column.Style ("text-align", Style);
      end if;
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
      --Table.Hidden;
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
                             Left_Text : String := "";
                             Left_Handler : GGB.Action_Event := null;
                             Right_Text : String := "";
                             Right_Handler : GGB.Action_Event := null) is
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

      if not Is_Empty (Right_Text) then
         Column_Right.Dynamic;
         Button_Right.Dynamic;
      end if;

      Gui.Content_Group_Add_Space (Object, Key);
      Gui.Content_Group_Warning_Add (Object, "", "register-error", Key);

      Row.Create (Table.all);
      Column_Left.Create (Row.all);

      if not Is_Empty (Right_Text) then
         Column_Right.Create (Row.all);
      end if;

      if not Is_Empty (Left_Text) then
         Button_Left.Create (Column_Left.all, Left_Text);
         Button_Left.Class_Name ("button-standard");
         --Button_Left.Style ("width", "80%");
         Button_Left.Style ("box-sizing", "border-box");
         Button_Left.On_Click_Handler (Left_Handler);
      end if;

      if not Is_Empty (Right_Text) then
         Button_Right.Create (Column_Right.all, Right_Text);
         Button_Right.Class_Name ("button-standard");
         --Button_Right.Style ("width", "80%");
         Button_Right.Style ("box-sizing", "border-box");
         Button_Right.On_Click_Handler (Right_Handler);
      end if;

   end Dialog_Buttons;

   ----------------------------------------------------------------------------
   procedure Dialog_Popup (Object : in out GGB.Base_Type'Class;
                          Title : String; Content : String;
                          Left_Text : String := "";
                          Left_Handler : GGB.Action_Event := null;
                          Right_Text : String := "";
                          Right_Handler : GGB.Action_Event := null;
                          Height : Natural := 150;
                          Width : Natural := 300;
                          Position_My : String := "center top+40";
                          Position_At: String := "center top+40") is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Dialog_Class : constant GGE.Pointer_To_Element_Class := new GGPJW.Dialog_Type;
      Dialog : constant GGPJW.Dialog_Access := GGPJW.Dialog_Access (Dialog_Class);
      Button : GGE.Pointer_To_Element_Class;
   begin

      -- (Dialog          : in out Dialog_Type;
      --  Parent          : in out Gnoga.Gui.Base.Base_Type'Class;
      --  Title           : in     String;
      --  Content         : in     String  := "";
      --  Height          : in     Natural := 0;
      --  Width           : in     Natural := 0;
      --  Position_My     : in     String  := "center";
      --  Position_At     : in     String  := "center";
      --  Resizable       : in     Boolean := False;
      --  Minimum_Height  : in     Natural := 150;
      --  Minimum_Width   : in     Natural := 150;
      --  Maximum_Height  : in     Natural := 0;
      --  Maximum_Width   : in     Natural := 0;
      --  Modal           : in     Boolean := True;
      --  Close_On_Escape : in     Boolean := True;
      --  Draggable       : in     Boolean := True;
      --  ID              : in     String  := "")

      Dialog.Create (Parent => Object.Parent.all,
                     Title => Title,
                     Content => Content,
                     Height => Height,
                     Width => Width,
                     Position_My => "center top+40",
                     Position_At => "center top+40");

      Dialog_Class.Dynamic;

      if Left_Handler /= null then
         Button := new GGE.Common.Button_Type;
         GGE.Common.Button_Access (Button).Create (Dialog.all, Left_Text);
         Button.On_Click_Handler (Left_Handler);
         Button.Dynamic;
         Button.Class_Name ("ui-button ui-corner-all");
         GGPJ.Position (Button.all, Target => Dialog.all, Using_My => "bottom", At_Target => "left+70 bottom-10");
      end if;

      if Right_Handler /= null then
         Button := new GGE.Common.Button_Type;
         GGE.Common.Button_Access (Button).Create (Dialog.all, Right_Text);
         Button.Dynamic;
         Button.Focus;
         Button.On_Click_Handler (Right_Handler);
         Button.Class_Name ("ui-button ui-corner-all");
         GGPJ.Position (Button.all, Target => Dialog.all, Using_My => "bottom", At_Target => "right-70 bottom-10");
      end if;

      Dialog.On_Close_Handler (Close_Dialog'Unrestricted_Access);
      App.Container.Add_Element ("dialog", Dialog_Class);
   end Dialog_Popup;

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
   function Get_Display_In_Progress (Object : in out GGB.Base_Type'Class) return Boolean is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.User_Display_In_Progress;
   end Get_Display_In_Progress;

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
                   Db_Name : String;
                   Db_Table : String;
                   List_Name : String;
                   List_Key : String;
                   List_Header : String;
                   List_Columns : String;
                   Condition : String := "") is
      DBT : Sql.Database_Line_Type;
   begin
      DBT := Sql.Properties (Db_Name);
      if DBT.Brand = Sql.MySQL then
         List (Object, DBT.DBM, Db_Table, List_Name, List_Key, List_Header, List_Columns, Condition);
      elsif DBT.Brand = Sql.SQLite then
         List (Object, DBT.DBS, Db_Table, List_Name, List_Key, List_Header, List_Columns, Condition);
      else
         Msg.Error ("Gui.List > Properties not found for: " & (if Is_Empty (Db_Name) then From_Latin_1 ("<Empty>") else Db_Name));
      end if;
   end List;

   ---------------------------------------------------------------------------
   procedure List (Object : in out GGB.Base_Type'Class;
                   DB : in out GSD.Connection'Class;
                   Db_Table : String;
                   List_Name : String;
                   List_Key : String;
                   List_Header : String;
                   List_Columns : String;
                   Condition : String := "") is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("List_" & List_Key);
      Table : GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);

      -- Add hidden Id column for development ease
      Column_Id : String := "#" & Connection_Data_Get (Object, "Db_Table") & ".Id";
      List_Columns_In : String := List_Columns & "," & Column_Id;

      Query : String := "SELECT " & Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    Replace (
                                    List_Columns_In,
                                    HC, ""), --  # Hide column in list
                                    IC, ""), --  ! Indexed column in list
                                    "*L", ""),
                                    "*R", ""),
                                    "M$", ""),
                                    "M€", ""),
                                    "M%", ""),
                                    "DT/", ""),
                                    "DT.", ""),
                                    "D/", ""),
                                    "D.", "")
                                    &
                        " FROM " & Db_Table & " " & Trim_Both (Condition);
      Columns_Header_Count : constant Natural := Field_Count (List_Header, VD) +1;
      Columns_Name_Count : constant Natural := Field_Count (List_Columns_In, VD);
      Column_Name : String;
      Column_Key_Last_Value : String := "";
      First_Id, Last_Id : String;

      Lines_Count : Natural := 0;
   begin

      if App.User_Display_In_Progress then
         Msg.Error ("Gui.List > Display already in progress");
         delay 2.0; --  Avoid display collision
         App.User_Display_In_Progress := True; --  Avoid interlock
      else
         App.User_Display_In_Progress := True;

         Msg.Debug ("Gui.List > Start");

         App.Window.Buffer_Connection (True);
         Gui.Content_Put_Title (Object, List_Name);
         App.Content_Text.Auto_Place (False);
         --App.Content_Text.Hidden;

         Gui.Content_List_Create (Object, List_Key);

         --  Table header
         if (Columns_Name_Count = Columns_Header_Count) then
            for Index_Columns in 1..Columns_Name_Count loop
               Column_Name := Field_By_Index (List_Columns_In, Index_Columns, VD);
               --  Display or hide column
               if Index  (Column_Name, HC) = 0 then
                  Gui.Content_List_Add_Column (Object, Field_By_Index (List_Header, Index_Columns, VD), List_Key);
               end if;
            end loop;
         else
            Msg.Error ("Gui.List > Fields number mismatch between Columns_Names and Column_Titles");
         end if;
         --  Table elements
         Msg.Debug ("Gui.List > Query: " & Query);
         declare
            RS : GSD.Recordset'Class := DB.Query (Query);
            Index_Lines : Integer := 0;
         begin
            --  Initialize _First key for empty list
            Gui.Connection_Data_Set (Object, List_Key & "_First", "");
            while RS.Next loop
               Lines_Count := Lines_Count + 1;
               Index_Lines := Gui.Content_List_Add_Item (Object, List_Key, Index_Lines + 1);
               --  Msg.Debug ("Index_Lines: " & To_String (Index_Lines));
               for Index_Columns in 1..RS.Number_Of_Fields loop
                  Column_Name := Field_By_Index (List_Columns_In, Index_Columns, VD);
                  --  Indexed column detected
                  if Index  (Column_Name, IC) > 0 then
                     Column_Key_Last_Value := RS.Field_Value (Index_Columns);
                     -- Save the tagged (ND) column key for further seeking the record
                     Gui.Connection_Data_Set (Object, List_Key & "_" & To_String_Unsigned (Index_Lines), Column_Key_Last_Value);
                     --  Save the first displayed column key for browsing list
                     if Lines_Count = 1 then
                        Gui.Connection_Data_Set (Object, List_Key & "_First", Column_Key_Last_Value);
                        Msg.Debug ("Gui.List > Key/First: " & List_Key & "_First/" & Column_Key_Last_Value);
                     end if;
                     --Msg.Debug ("Gui.List > Key/Value: " & List_Key & "_" & To_String_Unsigned (Index_Lines) & "/" & Column_Key_Last_Value);
                  end if;
                  --  Store Id
                  if To_Upper (Column_Name) = To_Upper (Column_Id) then
                     Gui.Connection_Data_Set (Object, List_Key & "_Id" & "_" & To_String_Unsigned (Index_Lines), RS.Field_Value (Index_Columns));
                     Last_Id := RS.Field_Value (Index_Columns);
                     --Msg.Debug ("Gui.List > Key_Id_n/Value: " & List_Key & "_Id" & "_" & To_String_Unsigned (Index_Lines) &
                     --                                                                     "/" & RS.Field_Value (Index_Columns));
                  end if;
                  --  Display/Hide column control
                  if Index  (Column_Name, HC) = 0 then
                     if Index  (Column_Name, "M$") > 0 then
                        Gui.Content_List_Add_Text (Object, "$" & From_DB_To_Money_String (RS.Field_Value (Index_Columns)),
                                                           Index_Lines, List_Key, "right");
                     elsif Index  (Column_Name, "M€") > 0 then
                        Gui.Content_List_Add_Text (Object, From_DB_To_Money_String (RS.Field_Value (Index_Columns)) & "€",
                                                           Index_Lines, List_Key, "right");
                     elsif Index  (Column_Name, "M%") > 0 then
                        Gui.Content_List_Add_Text (Object, From_DB_To_Money_String (RS.Field_Value (Index_Columns)),
                                                           Index_Lines, List_Key, "right");
                     elsif Index  (Column_Name, "DT/") > 0 then
                        Gui.Content_List_Add_Text (Object, Prg.Date_Time_Stamp_To_Local (RS.Field_Value (Index_Columns)), Index_Lines, List_Key);
                     elsif Index  (Column_Name, "DT.") > 0 then
                        Gui.Content_List_Add_Text (Object, Prg.Date_Time_Stamp_To_Local (RS.Field_Value (Index_Columns), "."), Index_Lines, List_Key);
                     elsif Index  (Column_Name, "D/") > 0 then
                        Gui.Content_List_Add_Text (Object, From_DB_To_Date_String (RS.Field_Value (Index_Columns)), Index_Lines, List_Key);
                     elsif Index  (Column_Name, "D.") > 0 then
                        Gui.Content_List_Add_Text (Object, From_DB_To_Date_String (RS.Field_Value (Index_Columns), "."), Index_Lines, List_Key);
                     elsif Index  (Column_Name, "*L") > 0 then
                        Gui.Content_List_Add_Text (Object, RS.Field_Value (Index_Columns), Index_Lines, List_Key, "left");
                     elsif Index  (Column_Name, "*R") > 0 then
                        Gui.Content_List_Add_Text (Object, RS.Field_Value (Index_Columns), Index_Lines, List_Key, "right");
                     else
                        Gui.Content_List_Add_Text (Object, RS.Field_Value (Index_Columns), Index_Lines, List_Key);
                     end if;
                  end if;
                  --  Msg.Debug (RS.Field_Value (Index_Columns));
                  --  Msg.Debug ("Index_Columns: " & To_String (Index_Columns));
               end loop;
            end loop;
            RS.Close;
            --  Save the last displayed column key for browsing list
            Gui.Connection_Data_Set (Object, List_Key & "_Last", Column_Key_Last_Value);
            Msg.Debug ("Gui.List > Key/Last: " & List_Key & "_Last/" & Column_Key_Last_Value);
         end;

         --  List status display
         First_Id := Connection_Data_Get (Object, Connection_Data_Get (Object, "List_Key") & "_Id_1");
         Msg.Debug ("Gui.List > First_Id: " & First_Id);
         Msg.Debug ("Gui.List > Last_Id: " & Last_Id);
         Msg.Debug ("Gui.List > Displayed_List_First_Id: " & Connection_Data_Get (Object, Connection_Data_Get (Object, "List_Key") & "_First_Id"));
         Msg.Debug ("Gui.List > Displayed_List_Last_Id: " & Connection_Data_Get (Object, Connection_Data_Get (Object, "List_Key") & "_Last_Id"));

         Footer_Set_Right_Text (Object, "");
         if Lines_Count = 0 then
            Footer_Set_Right_Text (Object, "Liste vide");
         else
            if Connection_Data_Get (Object, Connection_Data_Get (Object, "List_Key") & "_Last_Id") = Last_Id then
               Footer_Set_Right_Text (Object, "Fin de liste");
            end if;
            if Connection_Data_Get (Object, Connection_Data_Get (Object, "List_Key") & "_First_Id") = First_Id then
               Footer_Set_Right_Text (Object, "Début de liste");
            end if;
         end if;

         --App.Content_Text.Hidden (Value => False);
         App.Content_Text.Auto_Place (True);
         App.Window.Buffer_Connection (False);

         App.User_Display_In_Progress := False;

      end if;

   end List;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Add_Delimiter_Above (Object : in out GGB.Base_Type'Class; Key : String) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Main_Menu_Instance.Add_Delimiter_Above (Main_Menu_Get (Object, Key));
   end Main_Menu_Add_Delimiter_Above;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Add_Element (Object : in out GGB.Base_Type'Class;
                                    Key : String; Name :
                                    String;
                                    Icon_SRC : String;
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
      App.Main_Menu_Dict.Clear; --  <<<240314>>>
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
   procedure Pop_Up (Object : in out Gnoga.Gui.Base.Base_Type'Class; Text : String; Title : String := "Message") is
   begin
      Gui.Dialog_Popup (Object,
       Title => Title,
       Content => "<br>" & Text & "<br>",
       Right_Text => "OK",
       Right_Handler => On_Pop_Up_Ok'Unrestricted_Access);
       --  Height => 400, -- <-- Don't forget to adjust
       --  Width => 350);
   end Pop_Up;

   ----------------------------------------------------------------------------
   procedure Progress_Bar_Create (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent : constant GGE.Pointer_To_Element_Class := new GGE.Form.Form_Type;

      X_Limit : constant := 8.0;
      Y_Limit : constant := 2.0;

      package GGECCP renames Gnoga.Gui.Element.Canvas.Context_2D.Plotting;
      Plot : GGECCP.Plot_Info;
      Quit : GGE.Common.Button_Type;
      List : GGECCP.Point_List (1 .. Integer (20 * X_Limit) + 5);
      Last : Positive := List'Last;
   begin

      GGE.Form.Form_Access (Parent).Create (App.Content_Text);

      Plot.Create (Parent => Parent.all, Width => 500, Height => 500,
                   X_Min => -X_Limit, X_Max => X_Limit, Y_Min => -Y_Limit, Y_Max  => Y_Limit);

      --App.Container.Text_Alignment (Value => GGE.Center);
      Plot.Border;
      --App.Container.New_Line;

      Plot.Axes (Interval => 1.0, Length => 10);
      Plot.Point (Position => (X => -1.0, Y => 1.5), Color => "green");
      Plot.Point (Position => (X => 0.0, Y => 1.5), Color => "green");
      Plot.Point (Position => (X => 1.0, Y => 1.5), Color => "green");
      Plot.Line (From => (X => -X_Limit, Y => Y_Limit), To => (X => X_Limit, Y => -Y_Limit), Color => "blue");
      List (List'First) := (X => -X_Limit, Y => Math.Sin (-X_Limit) / (-X_Limit));

      All_Points :
      for I in List'First + 1 .. List'Last loop
         List (I).X := List (I - 1).X + 0.1;

         if List (I).X = 0.0 then
            List (I).Y := 1.0;
         else
            List (I).Y := Math.Sin (List (I).X) / List (I).X;
         end if;

         --  -1 Y Offset
         Plot.Point (Position => (X => List (I).X, Y => List (I).Y - 1.0) , Color => "green");

         if List (I).X > X_Limit then
            Last := I;
            exit All_Points;
         end if;
      end loop All_Points;

      -- No Y offset
      Plot.Graph (List => List (List'First .. Last), Color => "red");

   end Progress_Bar_Create;

   ----------------------------------------------------------------------------
   procedure Print (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Window.Print;
   end Print;

   ----------------------------------------------------------------------------
   procedure Put_User_Icon (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Instance.Set_User_Icon (User_Icon_File);
   end Put_User_Icon;

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
   procedure Set_Display_In_Progress (Object : in out GGB.Base_Type'Class; Status : Boolean := True) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.User_Display_In_Progress := Status;
   end Set_Display_In_Progress;

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
      Msg.Info ("Gui.User_Logout > User " & Connection_Data_Get (Object, "User_Login") &
               " disconnected, connection time: " & Prg.Duration_Stamp (Prg.Start_Time));

      Gui.Connection.Put_Login_Form (Object);
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
   procedure Main_Menu_List (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      for M in App.Main_Menu_Dict.Iterate loop
         Msg.Debug (Integer_Dictionary.Key (M) & ": " & From_Latin_1 (Integer'Image (App.Main_Menu_Dict (M))));
      end loop;
   end Main_Menu_List;

   ----------------------------------------------------------------------------
   procedure Main_Menu_Set (Object : in out GGB.Base_Type'Class; Key : String; Value : Integer) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Msg.Debug ("Gui.Main_Menu_Set > Key: " & Key & " Value: " & To_String_Unsigned (Value));
      App.Main_Menu_Dict.Insert (Key, Value);
      --  if Msg.Is_Debug = On then
      --     Main_Menu_List (Object);
      --  end if;
   end Main_Menu_Set;

   ----------------------------------------------------------------------------
   procedure On_Connect (Screen : in out Gnoga.Gui.Window.Window_Type'Class;
                         Connection : access Gnoga.Application.Multi_Connect.Connection_Holder_Type) is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;
   begin
      Screen.Connection_Data (Data => App, Dynamic => True); -- App will be unallocated when Screeen is finalized
      Screen.Buffer_Connection (True);
      App.Window := Screen'Unchecked_Access;

      App.Window.On_Character_Handler (On_Key_Pressed'Unrestricted_Access);
      App.Window.On_Key_Down_Handler (On_Key_Down'Unrestricted_Access);
   --  procedure On_Key_Up_Handler (Object : in out Base_Type; Handler : in Keyboard_Event);
   --  procedure On_Key_Press_Handler (Object : in out Base_Type; Handler : in Keyboard_Event);
   --  procedure Fire_On_Key_Down (Object : in out Base_Type; Event : in Keyboard_Event_Record);
   --  procedure Fire_On_Key_Up (Object : in out Base_Type; Event : in Keyboard_Event_Record);
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
         Gui.Connection.Put_Login_Form (App.Container);
      else
         --  Fake user bypassing login to ease development
         App.User_Logged_In := True;
         App.User_Logged_Since := AC.Clock;
         App.Custom_Data.Insert ("User_Login", "Dummy");
         App.Custom_Data.Insert ("First_Name", "Dummy_First");
         App.Custom_Data.Insert ("Last_Name", "Dummy_Last");
         App.Custom_Data.Insert ("Grants", GRANTS_ROLE_ADMINISTRATOR & ":" & GRANTS_RIGHTS_FULL);
         App.Header_Instance.Set_User_Name ("D. Dummy");
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

      --  Msg.Debug ("Gui.On_Key_Down > Message: " & From_Latin_1 (Keyboard_Message_Type'Image (Event.Message)));
      --  Msg.Debug ("Gui.On_Key_Down > Special_Key: " & Special_Key);
      --  Msg.Debug ("Gui.On_Key_Down > Key_Code: " & To_String (Event.Key_Code));

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
      if App.User_Display_In_Progress then
         Msg.Debug ("Gui.On_Key_Pressed > Display in progress, action deleted");
         delay 2.0; --  Avoid display collision
         App.User_Display_In_Progress := False;
      else
         --  Msg.Debug ("Gui.On_Key_Pressed > Key_Ascii: " & To_String (Char));
         --  Msg.Debug ("Gui.On_Key_Pressed > Key_Ascii_Val: " & From_Latin_1 (Integer'Image (Character'Pos (Char))));
         if App.User_Logged_In then
            --App.Main_Menu_Instance.Notify_Key_Pressed (Char);
            App.Header_Instance.Close_Menu;
            App.Header_Instance.Close_User_Menu;
         else
            null;
            -- CR Key handling in login window
            --  if To_String (Char) = CR then
            --     --  Msg.Debug ("Key_Ascii: " & To_String (Char));
            --     --  Msg.Debug ("Key_Ascii_Val: " & From_Latin_1 (Integer'Image (Character'Pos (Char))));
            --     Gui.Connection.On_Login_Connection (Object);
            --  end if;
         end if;
      end if;
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
   procedure On_Pop_Up_Ok (Object : in out GGB.Base_Type'Class) is
   begin
      Gui.Close_Dialog (Object);
   end On_Pop_Up_Ok;

   ----------------------------------------------------------------------------
   procedure Test_Plot (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Parent : constant GGE.Pointer_To_Element_Class := new GGE.Form.Form_Type;

      X_Limit : constant := 8.0;
      Y_Limit : constant := 2.0;

      package GGECCP renames Gnoga.Gui.Element.Canvas.Context_2D.Plotting;
      Plot : GGECCP.Plot_Info;
      Quit : GGE.Common.Button_Type;
      List : GGECCP.Point_List (1 .. Integer (20 * X_Limit) + 5);
      Last : Positive := List'Last;
   begin

      GGE.Form.Form_Access (Parent).Create (App.Content_Text);

      Plot.Create (Parent => Parent.all, Width => 500, Height => 500,
                   X_Min => -X_Limit, X_Max => X_Limit, Y_Min => -Y_Limit, Y_Max  => Y_Limit);

      --App.Container.Text_Alignment (Value => GGE.Center);
      Plot.Border;
      --App.Container.New_Line;

      Plot.Axes (Interval => 1.0, Length => 10);
      Plot.Point (Position => (X => -1.0, Y => 1.5), Color => "green");
      Plot.Point (Position => (X => 0.0, Y => 1.5), Color => "green");
      Plot.Point (Position => (X => 1.0, Y => 1.5), Color => "green");
      Plot.Line (From => (X => -X_Limit, Y => Y_Limit), To => (X => X_Limit, Y => -Y_Limit), Color => "blue");
      List (List'First) := (X => -X_Limit, Y => Math.Sin (-X_Limit) / (-X_Limit));

      All_Points :
      for I in List'First + 1 .. List'Last loop
         List (I).X := List (I - 1).X + 0.1;

         if List (I).X = 0.0 then
            List (I).Y := 1.0;
         else
            List (I).Y := Math.Sin (List (I).X) / List (I).X;
         end if;

         --  -1 Y Offset
         Plot.Point (Position => (X => List (I).X, Y => List (I).Y - 1.0) , Color => "green");

         if List (I).X > X_Limit then
            Last := I;
            exit All_Points;
         end if;
      end loop All_Points;

      -- No Y offset
      Plot.Graph (List => List (List'First .. Last), Color => "red");

   end Test_Plot;

-------------------------------------------------------------------------------
end v22.Gui;
-------------------------------------------------------------------------------
