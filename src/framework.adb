with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.Plugin;
with Gnoga.Gui.View; use Gnoga.Gui.View;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Section;
with Gnoga.Server.Connection;
with Gnoga.Types;
with UXStrings;      use UXStrings;
with Simple_Form;
with Menu;
with User_Menu;
with Crud;

procedure Framework is

   use all type Gnoga.String;

   App_Name : constant UXString := "ADA Framework";

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Window : Gnoga.Gui.Window.Pointer_To_Window_Class;
      View   : Gnoga.Gui.View.View_Type;

      Menu_Content : Menu.Menu_Type;

      Navigation_Bar : Gnoga.Gui.Element.Common.DIV_Type;
      Tool_Bar       : Gnoga.Gui.View.View_Type;
      Content        : Gnoga.Gui.Element.Common.DIV_Type;
      Bottom_Bar     : Gnoga.Gui.Element.Common.DIV_Type;

      Content_Header : Gnoga.Gui.Element.Section.Section_Type;
      Content_Text   : Gnoga.Gui.Element.Common.P_Type;

      Status    : Gnoga.Gui.Element.Common.DIV_Type;
      Permanent : Gnoga.Gui.Element.Common.DIV_Type;

      App_Icon : Gnoga.Gui.Element.Common.IMG_Type;

      User_Icon      : Gnoga.Gui.Element.Common.IMG_Type;
      User_Name      : Gnoga.Gui.Element.Common.P_Type;
      User_Name_Text : UXString := "Nom d'utilisateur";

      Is_Navigation_User_Opened   : Boolean := False;
      Is_Navigation_Browse_Opened : Boolean := False;

      Navigation_App          : Gnoga.Gui.Element.Common.DIV_Type;
      Navigation_Breadcrumb   : Gnoga.Gui.View.View_Type;
      Navigation_User         : Gnoga.Gui.Element.Common.DIV_Type;
      Navigation_Browse_View  : Gnoga.Gui.View.View_Type;
      Navigation_User_Buttons : Gnoga.Gui.View.View_Type;

      Crud_Instance : Crud.Crud_Type;

      Exit_Button : Gnoga.Gui.Element.Common.Button_Type;

      Form_View_Gestion : Simple_Form.Form_View_Type;
   end record;
   type App_Access is access all App_Data;

   Form_Gestion_Row_Names : constant UXString :=
     "A,B,adresse,date de création,récurrence,durée d'engagement," &
     "tacite reconduction,contrat actif,nature du contrat,prix HT,TVA,prix TTC,prochaine date de facturation," &
     "prochaine date d'échéance,note publique,note privée,référence proposition";

   Last_Parameters : Gnoga.Types.Data_Map_Type;

   Lorem_Ipsum : constant UXString :=
     80 *
     "Lorem ipsum dolor sit amet. Aut consequatur ipsam eos inventore repellat et neque sint id tempora aliquid eos assumenda ullam ut quas nostrum.";

   -----------------------------------------------------------------------------
   --  Browser Handlers
   -----------------------------------------------------------------------------
   ID_Contract : Integer;
   procedure On_Contract (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Contract);
      App.Content_Header.Text ("Contrats");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract;

   ID_Contract_Stats : Integer;
   procedure On_Contract_Stats (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Contract_Stats);
      App.Content_Header.Text ("Statistiques");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract_Stats;

   ID_Contract_Management : Integer;
   procedure On_Contract_Management (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Contract_Management);
      App.Content_Header.Text ("Gestion");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract_Management;

   ID_Administration_Users : Integer;
   procedure On_Administration_Users (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Administration_Users);
      App.Content_Header.Text ("Utilisateurs");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Users;

   ID_Administration_Emails : Integer;
   procedure On_Administration_Emails (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Administration_Emails);
      App.Content_Header.Text ("Emails");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Emails;

   ID_Administration_Gen : Integer;
   procedure On_Administration_Gen (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Administration_Gen);
      App.Content_Header.Text ("Générer des requêtes");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Gen;

   ID_Administration : Integer;
   procedure On_Administration (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Administration);
      App.Content_Header.Text ("Administration");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration;

   ID_Main : Integer;
   procedure On_Main (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Content.Notify_Click (ID_Main);
      App.Content_Header.Text (App_Name);
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Main;

   procedure On_Logo (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if (App.Is_Navigation_Browse_Opened) then
         App.Navigation_Browse_View.Display ("none");
         App.App_Icon.Remove_Class ("active-icon");
      else
         App.Menu_Content.Set_Menu (ID_Main);
         App.App_Icon.Add_Class ("active-icon");
         App.Navigation_Browse_View.Display ("inherit");
         App.Navigation_User_Buttons.Display ("none");
         App.User_Icon.Remove_Class ("active-icon");
         App.Is_Navigation_User_Opened := False;
      end if;
      App.Is_Navigation_Browse_Opened := not App.Is_Navigation_Browse_Opened;
   end On_Logo;

   -----------------------------------------------------------------------------
   --  User Handlers
   -----------------------------------------------------------------------------
   procedure On_User (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if (App.Is_Navigation_User_Opened) then
         App.Navigation_User_Buttons.Display ("none");
         App.User_Icon.Remove_Class ("active-icon");
      else
         App.User_Icon.Add_Class ("active-icon");
         App.Navigation_User_Buttons.Display ("inherit");
         App.Navigation_Browse_View.Display ("none");
         App.App_Icon.Remove_Class ("active-icon");
         App.Is_Navigation_Browse_Opened := False;
      end if;
      App.Is_Navigation_User_Opened := not App.Is_Navigation_User_Opened;
   end On_User;

   --  procedure On_Button_Click (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   --  begin
   --     Gnoga.Log (Object.jQuery_Execute ("text()"));
   --  end On_Button_Click;

   -----------------------------------------------------------------------------
   --  CRUD Handlers
   -----------------------------------------------------------------------------
   ID_Crud_File : Integer;

   ID_Crud_File_Create : Integer;
   procedure On_Crud_File_Create (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_File_Create));
   end On_Crud_File_Create;

   ID_Crud_File_Edit : Integer;
   procedure On_Crud_File_Edit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_File_Edit));
   end On_Crud_File_Edit;

   ID_Crud_File_Delete : Integer;
   procedure On_Crud_File_Delete (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_File_Delete));
   end On_Crud_File_Delete;

   ID_Crud_File_Export : Integer;
   procedure On_Crud_File_Export (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_File_Export));
   end On_Crud_File_Export;

   ID_Crud_File_Import : Integer;
   procedure On_Crud_File_Import (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_File_Import));
   end On_Crud_File_Import;

   ID_Crud_File_Print : Integer;
   procedure On_Crud_File_Print (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_File_Print));
   end On_Crud_File_Print;

   ID_Crud_Edit : Integer;

   ID_Crud_Edit_Copy : Integer;
   procedure On_Crud_Edit_Copy (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_Edit_Copy));
   end On_Crud_Edit_Copy;

   ID_Crud_Edit_Paste : Integer;
   procedure On_Crud_Edit_Paste (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_Edit_Paste));
   end On_Crud_Edit_Paste;

   ID_Crud_Show : Integer;

   ID_Crud_Show_Previous : Integer;
   procedure On_Crud_Show_Previous (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_Show_Previous));
   end On_Crud_Show_Previous;

   ID_Crud_Show_Next : Integer;
   procedure On_Crud_Show_Next (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_Show_Next));
   end On_Crud_Show_Next;

   ID_Crud_Show_Search : Integer;
   procedure On_Crud_Show_Search (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log (Crud.Menu_Name (ID_Crud_Show_Search));
   end On_Crud_Show_Search;

   -----------------------------------------------------------------------------
   --  Tool Bar retract button
   -----------------------------------------------------------------------------
   procedure On_Tool_Bar_Expand (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Resize;
   end On_Tool_Bar_Expand;

   -----------------------------------------------------------------------------
   --  On_Exit
   -----------------------------------------------------------------------------
   procedure On_Exit (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Application.Multi_Connect.End_Application;
   exception
      when E : others =>
         Gnoga.Log (Message => "On_Exit: ", Occurrence => E);
   end On_Exit;

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
      App.View.Create (Screen);

      --------------------------------------------------------------------------
      --  Containers
      --------------------------------------------------------------------------
      App.View.Style ("width", "100%");
      App.View.Style ("height", "100%");

      App.Navigation_Bar.Create (App.View);
      App.Navigation_Bar.Class_Name ("navigation-bar");

      App.Tool_Bar.Create (App.View);
      App.Tool_Bar.Class_Name ("tool-bar");

      App.Content.Create (App.View);
      App.Content.Class_Name ("content-container");
      App.Bottom_Bar.Create (App.View);
      App.Bottom_Bar.Class_Name ("bottom-bar");

      --------------------------------------------------------------------------
      --  Bottom details
      --------------------------------------------------------------------------
      App.Status.Create (App.Bottom_Bar, "Message de statut");
      App.Status.Class_Name ("bottom-bar-status");

      App.Permanent.Create (App.Bottom_Bar, "Informations permanentes");
      App.Permanent.Class_Name ("bottom-bar-permanent");

      --------------------------------------------------------------------------
      --  Navigation bar
      --------------------------------------------------------------------------
      App.Navigation_App.Create (App.Navigation_Bar);
      App.Navigation_App.Class_Name ("logo-container");
      App.App_Icon.Create (App.Navigation_App, URL_Source => "/css/icons/home.png");
      App.App_Icon.Class_Name ("top-icon");
      App.App_Icon.On_Click_Handler (On_Logo'Unrestricted_Access);

      App.Navigation_Breadcrumb.Create (App.Navigation_Bar);
      App.Navigation_Breadcrumb.Class_Name ("breadcrumb-container");

      App.Navigation_Browse_View.Create (App.Navigation_Bar);
      App.Navigation_Browse_View.Class_Name ("navigation-browse-buttons");

      App.Navigation_User_Buttons.Create (App.Navigation_Bar);
      App.Navigation_User_Buttons.Class_Name ("navigation-user-buttons");

      App.Navigation_User.Create (App.Navigation_Bar);
      App.Navigation_User.Class_Name ("user-container");
      App.User_Name.Create (App.Navigation_User, App.User_Name_Text);
      App.User_Name.Class_Name ("user-name");
      App.User_Icon.Create (App.Navigation_User, URL_Source => "/css/icons/user.png");
      App.User_Icon.Class_Name ("top-icon");
      App.User_Icon.On_Click_Handler (On_User'Unrestricted_Access);

      App.Content_Header.Create (App.Content, Gnoga.Gui.Element.Section.H1);
      App.Content_Header.Class_Name ("content-header");

      App.Content_Text.Create (App.Content);

      App.Menu_Content := Menu.Create (App.Navigation_Browse_View, App.Navigation_Breadcrumb);
      App.Menu_Content.Set_Menu (ID_Main);

      User_Menu.Create (App.Navigation_User_Buttons);

      --------------------------------------------------------------------------
      --  Tool bar
      --------------------------------------------------------------------------
      App.Crud_Instance.Create (App.Tool_Bar, On_Tool_Bar_Expand'Unrestricted_Access);
      App.Window.On_Character_Handler (Crud.On_Shortcut_Pressed'Unrestricted_Access);

      --  App.Window.jQuery_Execute ("keydown( function( e ) {if( e.target.nodeName == ""INPUT"" || e.target.nodeName == ""TEXTAREA"" ) return; if( e.target.isContentEditable ) return; }");

      App.Exit_Button.Create (App.Content, "Stopper exécution");
      App.Exit_Button.Style ("width", "140px");
      App.Exit_Button.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.Navigation_User_Buttons.Display ("none");
      App.Navigation_Browse_View.Display ("none");
      Screen.Buffer_Connection (False);

   end On_Connect;

   procedure On_Post_Request
     (URI                 : in     UXString;
      Accepted_Parameters :    out UXString)
   is
      pragma Unreferenced (URI);
   begin
      Accepted_Parameters := Simple_Form.Remove_All_Occurences (Form_Gestion_Row_Names, "'");
   end On_Post_Request;

   procedure On_Post
     (URI        :        UXString;
      Parameters : in out Gnoga.Types.Data_Map_Type)
   is
      pragma Unreferenced (URI);
   begin
      Last_Parameters := Parameters;
   end On_Post;

   procedure Results
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      Result_View : constant Gnoga.Gui.View.View_Access := new Gnoga.Gui.View.View_Type;
   begin
      Result_View.Dynamic;
      Result_View.Create (Main_Window);
      for C in Last_Parameters.Iterate loop
         Result_View.Put_Line
           ("POST parameter: " & Gnoga.Types.Data_Maps.Key (C) & " = " & Gnoga.Types.Data_Maps.Element (C));
      end loop;
      Last_Parameters.Clear;
   end Results;

begin
   Gnoga.Application.Title (App_Name);
   Gnoga.Application.HTML_On_Close ("Server closed.");
   Gnoga.Application.Multi_Connect.Initialize (Boot => "boot_jqueryui.html");
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Results'Unrestricted_Access, "/result");

   ID_Main := Menu.Set_Root (App_Name, On_Main'Unrestricted_Access);

   ID_Contract            := Menu.Add_Child (ID_Main, "Contrats", On_Contract'Unrestricted_Access);
   ID_Contract_Management := Menu.Add_Child (ID_Contract, "Gestion", On_Contract_Management'Unrestricted_Access);
   ID_Contract_Stats      := Menu.Add_Child (ID_Contract, "Statistiques", On_Contract_Stats'Unrestricted_Access);

   ID_Administration       := Menu.Add_Child (ID_Main, "Administration", On_Administration'Unrestricted_Access);
   ID_Administration_Users :=
     Menu.Add_Child (ID_Administration, "Utilisateurs", On_Administration_Users'Unrestricted_Access);
   ID_Administration_Emails :=
     Menu.Add_Child (ID_Administration, "Emails", On_Administration_Emails'Unrestricted_Access);
   ID_Administration_Gen :=
     Menu.Add_Child (ID_Administration, "Gén. requêtes", On_Administration_Gen'Unrestricted_Access);

   User_Menu.Add_Web ("Aide en ligne", "https://google.com");
   User_Menu.Add_Dialog ("Droits d'accès", "Ajouter les droits d'accès");
   User_Menu.Add_Dialog ("Connecté depuis...", "Ajouter durée de la connection");
   User_Menu.Add_Dialog ("Connection précédente", "Ajouter la date de la dernière connection");
   User_Menu.Add_Web ("À propos de...", "http://gnoga.com");

   ID_Crud_File        := Crud.Add_Root ("Fichier", "css/icons/file.png");
   ID_Crud_File_Create := Crud.Add_Child ("Créer", ID_Crud_File, On_Crud_File_Create'Unrestricted_Access);
   ID_Crud_File_Edit   := Crud.Add_Child ("Modifier", ID_Crud_File, On_Crud_File_Edit'Unrestricted_Access);
   ID_Crud_File_Delete := Crud.Add_Child ("Supprimer", ID_Crud_File, On_Crud_File_Delete'Unrestricted_Access);
   ID_Crud_File_Export := Crud.Add_Child ("Exporter", ID_Crud_File, On_Crud_File_Export'Unrestricted_Access);
   ID_Crud_File_Import := Crud.Add_Child ("Importer", ID_Crud_File, On_Crud_File_Import'Unrestricted_Access);
   ID_Crud_File_Print  := Crud.Add_Child ("Imprimer", ID_Crud_File, On_Crud_File_Print'Unrestricted_Access);

   ID_Crud_Edit       := Crud.Add_Root ("Éditer", "css/icons/edit.png");
   ID_Crud_Edit_Copy  := Crud.Add_Child ("Copier", ID_Crud_Edit, On_Crud_Edit_Copy'Unrestricted_Access);
   ID_Crud_Edit_Paste := Crud.Add_Child ("Coller", ID_Crud_Edit, On_Crud_Edit_Paste'Unrestricted_Access);

   ID_Crud_Show          := Crud.Add_Root ("Afficher", "css/icons/browse.png");
   ID_Crud_Show_Previous := Crud.Add_Child ("Précédent", ID_Crud_Show, On_Crud_Show_Previous'Unrestricted_Access);
   ID_Crud_Show_Next     := Crud.Add_Child ("Suivant", ID_Crud_Show, On_Crud_Show_Next'Unrestricted_Access);
   ID_Crud_Show_Search   := Crud.Add_Child ("Rechercher", ID_Crud_Show, On_Crud_Show_Search'Unrestricted_Access);

   Gnoga.Server.Connection.On_Post_Handler (On_Post'Unrestricted_Access);
   Gnoga.Server.Connection.On_Post_Request_Handler (On_Post_Request'Unrestricted_Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end Framework;
