-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testgui.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - GUI test program
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

with Ada.Calendar;
with Ada.Containers.Hashed_Maps;

with GNAT.Calendar.Time_IO;
with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;

with Gnoga.Types;
with Gnoga.Application.Multi_Connect;
with Gnoga.Server.Database.MySQL;
with Gnoga.Server.Database.SQLite;
with Gnoga.Gui.Base;

with UXStrings; use UXStrings;
with UXStrings.Conversions; use UXStrings.Conversions;
with UXStrings.Hash;

with v22; use v22;
with v22.Cfg;
--  with v22.Crl; use v22.Crl; -- for operators
with v22.Fls;
with v22.Gui;
with v22.Msg;
with v22.Uxs; use v22.Uxs;
--  with v22.Net;
with v22.Prg;
with v22.Sql; use v22.Sql; -- for operators
with v22.Sys;
with v22.Tio;

procedure TestGui is

   package AC  renames Ada.Calendar;
   package GCT renames GNAT.Calendar.Time_IO;
   package GCL renames GNAT.Command_Line;
   package GOL renames GNAT.OS_Lib;
   package GS renames GNAT.Strings;
   package GGB renames Gnoga.Gui.Base;
   package GSD renames Gnoga.Server.Database;

   ----------------------------------------------------------------------------
   --  PUBLIC TYPES
   ----------------------------------------------------------------------------

   subtype String is UXString;
   use all type Gnoga.String; -- https://learn.adacore.com/courses/advanced-ada/parts/modular_prog/packages.html#use-type

   type App_Connexion_Type is record
      Connection_Domain : String;
      Connection_Port : Integer;
      Connection_Tls_Certificate : String;
      Connection_Tls_Private_Key : String;
      Database_Type : String;
      Database_Host : String;
      Database_Port : Integer;
      Database_Name : String;
      Database_User : String;
      Database_Password : String;
   end record;

   ----------------------------------------------------------------------------
   --  PUBLIC VARIABLES
   ----------------------------------------------------------------------------

   Config : GCL.Command_Line_Configuration;
   Long_Option : aliased Integer := 0;
   Exception_Test : aliased Boolean := False;
   Package_Test : aliased Boolean := False;

   Memory_Reports : aliased Boolean := False;

   App_Name_Gui : String := "TestGui";
   App_Config_File : String := Prg.Name & ".cfg";
   App_Config_Datas : App_Connexion_Type;

   --DB : GSD.MySQL.Connection;
   DB : GSD.SQLite.Connection;

   Users_Key_Main : constant UXString := "Login";
   Users_List_Key : constant UXString := "Users_List";
   Users_List_Length : constant Natural := 10;

   Lorem_Ipsum : constant String := "Lorem ipsum dolor sit amet. Quo autem eaque ut sint molestias eos voluptate minus.";

   ----------------------------------------------------------------------------
   --  PROCEDURES & FUNCTIONS
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   package Adm_Users is
      procedure Main_Menu (Object : in out GGB.Base_Type'Class);
   end Adm_Users;
   package body Adm_Users is separate;

   ----------------------------------------------------------------------------
   package Ini is
      function App return Boolean;
      --  Initialize application
   end Ini;
   package body Ini is separate;

   ----------------------------------------------------------------------------
   package Mgt is
      procedure Demo_Complex_Form_Create (Object : in out GGB.Base_Type'Class);
      procedure Demo_Complex_Form_Edit (Object : in out GGB.Base_Type'Class);
      procedure Demo_Complex_Form_Delete (Object : in out GGB.Base_Type'Class);
      procedure Demo_Mode_1 (Object : in out GGB.Base_Type'Class);
   end Mgt;
   package body Mgt is separate;

   ----------------------------------------------------------------------------
   package Usr is
      procedure Connect (Object : in out GGB.Base_Type'Class);
      --  Connection screen and user menu setup
      procedure Help (Object : in out GGB.Base_Type'Class);
      --  Help
      procedure Information (Object : in out GGB.Base_Type'Class);
      --  User, application and system information
   end Usr;
   package body Usr is separate;

   ----------------------------------------------------------------------------
   --  Navigation Handlers
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure On_Mgt (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("User > On_Mgt");
      Gui.Header_Notify_Menu_Click (Object, "Mgt");
      Gui.Content_Put_Title (Object, "Gestion");
      Gui.Content_Put_Text (Object, Lorem_Ipsum);

      Gui.Main_Menu_Add_Element (Object, "Dispensers", "Mode n°1", "ico-apps.png", Mgt.Demo_Mode_1'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Lockers", "Casiers", "ico-dataset.png");
      Gui.Main_Menu_Add_Element (Object, "Events", "Évènements", "ico-event_list.png");
      Gui.Main_Menu_Add_Element (Object, "Preferences", "Mode n°2", "ico-settings_account_box.png");
      Gui.Main_Menu_Add_Sub_Element (Object, "Preferences_Create", "Créer", "Preferences", Mgt.Demo_Complex_Form_Create'Unrestricted_Access);
      Gui.Main_Menu_Add_Sub_Element (Object, "Preferences_Edit", "Modifier", "Preferences", Mgt.Demo_Complex_Form_Edit'Unrestricted_Access);
      Gui.Main_Menu_Add_Sub_Element (Object, "Preferences_Delete", "Supprimer", "Preferences", Mgt.Demo_Complex_Form_Delete'Unrestricted_Access);
      Gui.Main_Menu_Load (Object);

   end On_Mgt;

   ----------------------------------------------------------------------------
   procedure On_Adm (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("User > On_Adm");
      Gui.Header_Notify_Menu_Click (Object, "Adm");
      Gui.Content_Put_Title (Object, "Administration");
      Gui.Content_Put_Text (Object, Lorem_Ipsum);

      Gui.Main_Menu_Add_Element (Object, "Users", "Utilisateurs", "ico-group.png", Adm_Users.Main_Menu'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Dealers", "Vendeurs", "ico-local_convenience_store.png");
      Gui.Main_Menu_Add_Element (Object, "Dispensers", "Distributeurs", "ico-apps.png");
      Gui.Main_Menu_Add_Element (Object, "Lockers", "Casiers", "ico-dataset.png");
      Gui.Main_Menu_Add_Element (Object, "Events", "Évènements", "ico-event_list.png");
      Gui.Main_Menu_Add_Element (Object, "Parameters", "Paramètres", "ico-settings.png");
      Gui.Main_Menu_Load (Object);

   end On_Adm;

   ----------------------------------------------------------------------------
   --  function Image is new UXStrings.Conversions.Fixed_Point_Image (Duration);
   --
   --  task SQL_Ping is
   --     entry Start;
   --  end SQL_Ping;
   --
   --  task body SQL_Ping is
   --     Delay_Value : Duration := 3600.0;  -- Wait 1 hour between pings
   --  begin
   --     accept Start;
   --     Msg.Info ("TestGui.SQL_Ping > Armed for " & Trim_Left (Field_By_Index (Image (Delay_Value), 1, ".")) & "s cycles");
   --     loop
   --        delay Delay_Value;
   --        Sql.Ping;
   --     end loop;
   --  end SQL_Ping;

-------------------------------------------------------------------------------
begin

   if Ini.App then

      Gnoga.Log (72 * "-"); -- To ease gnoga log file reading
      Gnoga.Log ("Starting Gnoga server");
      Msg.Info ("TestGui.On_Connect > Starting Gnoga server");

      Gui.Setup (On_User_Connect => Usr.Connect'Unrestricted_Access,
                 Host => App_Config_Datas.Connection_Domain,
                 Port => App_Config_Datas.Connection_Port,
                 Boot => "boot_jqueryui.html",
                 Title => App_Name_Gui,
                 Server_Closed_Content => "<b>" & App_Name_Gui & " > Déconnexion serveur</b>");

      Gui.Set_Application_Icon ("ico-widget.png");
      Gui.Set_User_Icon ("ico-user.png");

      Gui.Set_Login (On); -- Application access is protected by a login screen. Set-it to Off to ease tests and debugging

      --  Application menu hierarchy
      Gui.Header_Set_Root ("App_Menu", App_Name_Gui, On_Mgt'Unrestricted_Access);
      Gui.Header_Application_Menu_Add ("Mgt", "Gestion", "App_Menu", On_Mgt'Unrestricted_Access);
      Gui.Header_Application_Menu_Add ("Adm", "Administration", "App_Menu", On_Adm'Unrestricted_Access);

      -- Start Sql_Ping task
      -- SQL_Ping.Start;

      --  Application message loop
      Gnoga.Application.Multi_Connect.Message_Loop;

   end if;

-------------------------------------------------------------------------------
exception

   --  -h or --help switches
   when GCL.Exit_From_Command_Line =>
      Msg.New_Line;
      GOL.OS_Exit (EXIT_CODE_AFTER_HELP);

   --  Invalid switches
   when GCL.Invalid_Switch =>
      Msg.New_Line;
      GOL.OS_Exit (EXIT_CODE_INVALID_PARAMETER);

   --  Non permissive runtime error handling with trace (application stops)
   when Error : others =>
      v22.Exception_Handling (Error);

   --  Permissive runtime error handling with trace (application continues)
   --  when Error : others =>
   --      Msg.Error (Error);
   --     Gnoga.Log (Error);

-------------------------------------------------------------------------------
end TestGui;
-------------------------------------------------------------------------------


