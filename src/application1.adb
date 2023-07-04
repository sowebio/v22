with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View; use Gnoga.Gui.View;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Section;
with Gnoga.Server.Connection;
with Gnoga.Types;
with UXStrings;      use UXStrings;
with Breadcrumb;
with Simple_Form;
with Folders;
with Menu;

procedure application1 is

   use all type Gnoga.String;

   App_Name : constant UXString := "ADA Framework";

   type Button_Set is array (Positive range <>) of Gnoga.Gui.Element.Common.Button_Type;

   type Widget_Set is array (Positive range <>) of aliased Gnoga.Gui.View.View_Type;

   --  Folders
   type Stdr_Folder_Type is new Gnoga.Types.Connection_Data_Type with record
      Folder : aliased Folders.Folder_Type;

      Widget : aliased Widget_Set (1 .. 3);
      Button : aliased Button_Set (1 .. 11);
   end record;

   --  Folders
   type Gestion_Folder_Type is new Gnoga.Types.Connection_Data_Type with record
      Folder : aliased Folders.Folder_Type;

      Widget : aliased Widget_Set (1 .. 5);
      Button : aliased Button_Set (1 .. 18);

      Line : aliased Gnoga.Gui.Element.Common.DIV_Type;
   end record;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Window : Gnoga.Gui.Window.Pointer_To_Window_Class;
      View   : Gnoga.Gui.View.View_Type;

      Navigation_Bar : Gnoga.Gui.Element.Common.DIV_Type;
      Tool_Bar       : Gnoga.Gui.Element.Common.DIV_Type;
      Content        : Gnoga.Gui.Element.Common.DIV_Type;
      Bottom_Bar     : Gnoga.Gui.Element.Common.DIV_Type;

      Content_Header : Gnoga.Gui.Element.Section.Section_Type;
      Content_Text   : Gnoga.Gui.Element.Common.P_Type;

      Status    : Gnoga.Gui.Element.Common.DIV_Type;
      Permanent : Gnoga.Gui.Element.Common.DIV_Type;

      App_Icon : Gnoga.Gui.Element.Common.IMG_Type;

      User_Icon      : Gnoga.Gui.Element.Common.IMG_Type;
      User_Name      : Gnoga.Gui.Element.Common.P_Type;
      User_Name_Text : UXString := "User Name";

      User_Buttons                : Button_Set (1 .. 5);
      Is_Navigation_User_Opened   : Boolean := False;
      Is_Navigation_Browse_Opened : Boolean := False;

      Navigation_App          : Gnoga.Gui.Element.Common.DIV_Type;
      Navigation_Breadcrumb   : Gnoga.Gui.View.View_Type;
      Navigation_User         : Gnoga.Gui.Element.Common.DIV_Type;
      Navigation_Browse_View  : Gnoga.Gui.View.View_Type;
      Navigation_User_Buttons : Gnoga.Gui.Element.Common.DIV_Type;

      BC : Breadcrumb.Breadcrumb_Type;

      Exit_Button : Gnoga.Gui.Element.Common.Button_Type;

      Stdr_Folder    : Stdr_Folder_Type;
      Gestion_Folder : Gestion_Folder_Type;

      Form_View_Gestion : Simple_Form.Form_View_Type;
   end record;
   type App_Access is access all App_Data;

   Form_Gestion_Row_Names : constant UXString :=
     "A,B,adresse,date de création,récurrence,durée d'engagement," &
     "tacite reconduction,contrat actif,nature du contrat,prix HT,TVA,prix TTC,prochaine date de facturation," &
     "prochaine date d'échéance,note publique,note privée,référence proposition";

   Last_Parameters : Gnoga.Types.Data_Map_Type;

   Lorem_Ipsum : constant UXString :=
     50 *
     "Lorem ipsum dolor sit amet. Aut consequatur ipsam eos inventore repellat et neque sint id tempora aliquid eos assumenda ullam ut quas nostrum.";

   -----------------------------------------------------------------------------
   --  Handlers
   -----------------------------------------------------------------------------
   ID_Contract : constant Integer := 100;
   procedure On_Contract (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Contract'Unrestricted_Access, ID_Contract);
      App.Content_Header.Text ("Contrats");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract;

   ID_Contract_Stats : constant Integer := 101;
   procedure On_Contract_Stats (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Contract_Stats'Unrestricted_Access,
         ID_Contract_Stats);
      App.Content_Header.Text ("Statistiques");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract_Stats;

   ID_Contract_Management : constant Integer := 102;
   procedure On_Contract_Management (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Contract_Management'Unrestricted_Access,
         ID_Contract_Management);
      App.Content_Header.Text ("Gestion");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract_Management;

   ID_Administration_Users : constant Integer := 201;
   procedure On_Administration_Users (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Administration_Users'Unrestricted_Access,
         ID_Administration_Users);
      App.Content_Header.Text ("Utilisateurs");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Users;

   ID_Administration_Emails : constant Integer := 202;
   procedure On_Administration_Emails (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Administration_Emails'Unrestricted_Access,
         ID_Administration_Emails);
      App.Content_Header.Text ("Emails");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Emails;

   ID_Administration_Gen : constant Integer := 203;
   procedure On_Administration_Gen (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Administration_Gen'Unrestricted_Access,
         ID_Administration_Gen);
      App.Content_Header.Text ("Générer des requêtes");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Gen;

   ID_Administration : constant Integer := 200;
   procedure On_Administration (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Administration'Unrestricted_Access,
         ID_Administration);
      App.Content_Header.Text ("Administration");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration;

   ID_Main : constant Integer := 0;
   procedure On_Main (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Menu.Notify_Click
        (App.Navigation_Browse_View, App.Navigation_Breadcrumb, App.BC, On_Main'Unrestricted_Access, ID_Main);
      App.Content_Header.Text (App_Name);
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Main;

   procedure On_Logo (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if (App.Is_Navigation_Browse_Opened) then
         App.Navigation_Browse_View.Display ("none");
      else
         On_Main (Object);
         App.Navigation_Browse_View.Display ("inherit");
         App.Navigation_User_Buttons.Display ("none");
         App.Is_Navigation_User_Opened := False;
      end if;
      App.Is_Navigation_Browse_Opened := not App.Is_Navigation_Browse_Opened;
   end On_Logo;

   procedure On_User (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if (App.Is_Navigation_User_Opened) then
         App.Navigation_User_Buttons.Display ("none");
      else
         App.Navigation_User_Buttons.Display ("inherit");
         App.Navigation_Browse_View.Display ("none");
         App.Is_Navigation_Browse_Opened := False;
      end if;
      App.Is_Navigation_User_Opened := not App.Is_Navigation_User_Opened;
   end On_User;

   procedure On_Simple_Button (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   begin
      Gnoga.Log (Object.jQuery_Execute ("text()"));
   end On_Simple_Button;

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

      --  procedure Widget_Create_Accueil
      --    (View   : in out Gnoga.Gui.View.View_Type;
      --     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      --     ID     : in     Gnoga.String := "")
      --  is
      --  begin
      --     Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      --     View.Put_Line ("Accueil");
      --  end Widget_Create_Accueil;
      --
      --  procedure Widget_Create_Contrats_Tab
      --    (View   : in out Gnoga.Gui.View.View_Type;
      --     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      --     ID     : in     Gnoga.String := "")
      --  is
      --  begin
      --     Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      --     View.Put_Line ("Contrats Tableau de bord");
      --  end Widget_Create_Contrats_Tab;
      --
      --  procedure Widget_Create_Contrats_Gestion
      --    (View   : in out Gnoga.Gui.View.View_Type;
      --     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      --     ID     : in     Gnoga.String := "")
      --  is
      --  begin
      --     Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      --     View.Put_Line ("Contrats Gestion");
      --  end Widget_Create_Contrats_Gestion;
      --
      --  procedure Widget_Create_Administration_Tab
      --    (View   : in out Gnoga.Gui.View.View_Type;
      --     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      --     ID     : in     Gnoga.String := "")
      --  is
      --  begin
      --     Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      --     View.Put_Line ("Administration Tableau de bord");
      --  end Widget_Create_Administration_Tab;
      --
      --  procedure Widget_Create_Administration_Utils
      --    (View   : in out Gnoga.Gui.View.View_Type;
      --     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      --     ID     : in     Gnoga.String := "")
      --  is
      --  begin
      --     Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      --     View.Put_Line ("Administration Utilisateurs");
      --  end Widget_Create_Administration_Utils;
      --
      --  procedure Widget_Create_Administration_Emails
      --    (View   : in out Gnoga.Gui.View.View_Type;
      --     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      --     ID     : in     Gnoga.String := "")
      --  is
      --  begin
      --     Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      --     View.Put_Line ("Administration Emails");
      --  end Widget_Create_Administration_Emails;
      --
      --  procedure Widget_Create_Administration_Gen
      --    (View   : in out Gnoga.Gui.View.View_Type;
      --     Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      --     ID     : in     Gnoga.String := "")
      --  is
      --  begin
      --     Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
      --     View.Put_Line ("Administration Génération de requêtes");
      --  end Widget_Create_Administration_Gen;

   begin
      Screen.Connection_Data (App);
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

      App.BC := Menu.Init_Breadcrumb (App.Navigation_Breadcrumb);

      App.Navigation_User.Create (App.Navigation_Bar);
      App.Navigation_User.Class_Name ("user-container");
      App.User_Name.Create (App.Navigation_User, App.User_Name_Text);
      App.User_Name.Class_Name ("user-name");
      App.User_Icon.Create (App.Navigation_User, URL_Source => "/css/icons/user.png");
      App.User_Icon.Class_Name ("top-icon");
      App.User_Icon.On_Click_Handler (On_User'Unrestricted_Access);

      App.User_Buttons (1).Create (App.Navigation_User_Buttons, "Aide en ligne");
      App.User_Buttons (1).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      App.User_Buttons (2).Create (App.Navigation_User_Buttons, "Droits d'accès");
      App.User_Buttons (2).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      App.User_Buttons (3).Create (App.Navigation_User_Buttons, "Connecté depuis");
      App.User_Buttons (3).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      App.User_Buttons (4).Create (App.Navigation_User_Buttons, "Connexion précédente");
      App.User_Buttons (4).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      App.User_Buttons (5).Create (App.Navigation_User_Buttons, "À propos de");
      App.User_Buttons (5).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.Content_Header.Create (App.Content, Gnoga.Gui.Element.Section.H1);
      App.Content_Header.Class_Name ("content-header");

      App.Content_Text.Create (App.Content);

      On_Main (App.View);

      --------------------------------------------------------------------------
      --  Tool bar
      --------------------------------------------------------------------------

      --  App.Stdr_Folder.Folder.Create_Folder (App.Menu_Folder.Widget (2));
      --
      --  --  --  Folder Fichier

      --  App.Stdr_Folder.Folder.Create_Section (Content => "<span class='rouge'>F</span>ichier", Name => "Fichier");
      --  App.Stdr_Folder.Widget (1).Create (App.Stdr_Folder.Folder);
      --
      --  --  --  --  Button Créer
      --
      --  App.Stdr_Folder.Button (1).Create (App.Stdr_Folder.Widget (1), "Créer");
      --  App.Stdr_Folder.Button (1).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Modifier
      --  App.Stdr_Folder.Button (2).Create (App.Stdr_Folder.Widget (1), "Modifier");
      --  App.Stdr_Folder.Button (2).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Supprimer
      --  App.Stdr_Folder.Button (3).Create (App.Stdr_Folder.Widget (1), "Supprimer");
      --  App.Stdr_Folder.Button (3).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Exporter
      --  App.Stdr_Folder.Button (4).Create (App.Stdr_Folder.Widget (1), "Exporter");
      --  App.Stdr_Folder.Button (4).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Importer
      --  App.Stdr_Folder.Button (5).Create (App.Stdr_Folder.Widget (1), "Importer");
      --  App.Stdr_Folder.Button (5).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Imprimer
      --  App.Stdr_Folder.Button (6).Create (App.Stdr_Folder.Widget (1), "Imprimer");
      --  App.Stdr_Folder.Button (6).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Folder Éditer
      --
      --  App.Stdr_Folder.Folder.Create_Section (Content => "<span class='rouge'>É</span>diter", Name => "Editer");
      --  App.Stdr_Folder.Widget (2).Create (App.Stdr_Folder.Folder);
      --
      --  --  --  --  Button Copier
      --  App.Stdr_Folder.Button (7).Create (App.Stdr_Folder.Widget (2), "Copier");
      --  App.Stdr_Folder.Button (7).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Coller
      --  App.Stdr_Folder.Button (8).Create (App.Stdr_Folder.Widget (2), "Coller");
      --  App.Stdr_Folder.Button (8).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Folder Afficher
      --
      --  App.Stdr_Folder.Folder.Create_Section (Content => "<span class='rouge'>A</span>fficher", Name => "Afficher");
      --  App.Stdr_Folder.Widget (3).Create (App.Stdr_Folder.Folder);
      --
      --  --  --  --  Button Précédent
      --  App.Stdr_Folder.Button (9).Create (App.Stdr_Folder.Widget (3), "Précédent");
      --  App.Stdr_Folder.Button (9).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Suivant
      --  App.Stdr_Folder.Button (10).Create (App.Stdr_Folder.Widget (3), "Suivant");
      --  App.Stdr_Folder.Button (10).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Rechercher
      --  App.Stdr_Folder.Button (11).Create (App.Stdr_Folder.Widget (3), "Rechercher");
      --  App.Stdr_Folder.Button (11).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  App.Stdr_Folder.Folder.Render_Folder (True);
      --
      --  --------------------------------------------------------------------------
      --
      --  App.Gestion_Folder.Folder.Create_Folder (App.Menu_Folder.Widget (3));
      --
      --  --  --  --  Folder Fichier
      --
      --  App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>F</span>ichier", Name => "Fichier");
      --  App.Gestion_Folder.Widget (1).Create (App.Gestion_Folder.Folder);
      --
      --  --  --  --  Button Créer
      --  App.Gestion_Folder.Button (1).Create (App.Gestion_Folder.Widget (1), "Créer");
      --  App.Gestion_Folder.Button (1).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Modifier
      --  App.Gestion_Folder.Button (2).Create (App.Gestion_Folder.Widget (1), "Modifier");
      --  App.Gestion_Folder.Button (2).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Supprimer
      --  App.Gestion_Folder.Button (3).Create (App.Gestion_Folder.Widget (1), "Supprimer");
      --  App.Gestion_Folder.Button (3).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Exporter
      --  App.Gestion_Folder.Button (4).Create (App.Gestion_Folder.Widget (1), "Exporter");
      --  App.Gestion_Folder.Button (4).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Importer
      --  App.Gestion_Folder.Button (5).Create (App.Gestion_Folder.Widget (1), "Importer");
      --  App.Gestion_Folder.Button (5).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Imprimer
      --  App.Gestion_Folder.Button (6).Create (App.Gestion_Folder.Widget (1), "Imprimer");
      --  App.Gestion_Folder.Button (6).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Folder Éditer
      --
      --  App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>É</span>diter", Name => "Editer");
      --  App.Gestion_Folder.Widget (2).Create (App.Gestion_Folder.Folder);
      --
      --  --  --  --  Button Copier
      --  App.Gestion_Folder.Button (7).Create (App.Gestion_Folder.Widget (2), "Copier");
      --  App.Gestion_Folder.Button (7).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Coller
      --  App.Gestion_Folder.Button (8).Create (App.Gestion_Folder.Widget (2), "Coller");
      --  App.Gestion_Folder.Button (8).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Folder Afficher
      --
   --  App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>A</span>fficher", Name => "Afficher");
   --  App.Gestion_Folder.Widget (3).Create (App.Gestion_Folder.Folder);
      --
      --  --  --  --  Button Précédent
      --  App.Gestion_Folder.Button (9).Create (App.Gestion_Folder.Widget (3), "Précédent");
      --  App.Gestion_Folder.Button (9).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Suivant
      --  App.Gestion_Folder.Button (10).Create (App.Gestion_Folder.Widget (3), "Suivant");
      --  App.Gestion_Folder.Button (10).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Rechercher
      --  App.Gestion_Folder.Button (11).Create (App.Gestion_Folder.Widget (3), "Rechercher");
      --  App.Gestion_Folder.Button (11).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  App.Gestion_Folder.Line.Create (App.Gestion_Folder.Widget (3));
      --  App.Gestion_Folder.Line.Style ("border-bottom", "2px solid black");
      --
      --  --  --  --  Button Lister
      --  App.Gestion_Folder.Button (12).Create (App.Gestion_Folder.Widget (3), "Lister");
      --  App.Gestion_Folder.Button (12).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Lister Factures
      --  App.Gestion_Folder.Button (13).Create (App.Gestion_Folder.Widget (3), "Lister Factures");
      --  App.Gestion_Folder.Button (13).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Lister SEPA
      --  App.Gestion_Folder.Button (14).Create (App.Gestion_Folder.Widget (3), "Lister SEPA");
      --  App.Gestion_Folder.Button (14).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Folder Valider
      --
      --  App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>V</span>alider", Name => "Valider");
      --  App.Gestion_Folder.Widget (4).Create (App.Gestion_Folder.Folder);
      --
      --  --  --  --  Button Factures
      --  App.Gestion_Folder.Button (15).Create (App.Gestion_Folder.Widget (4), "Factures");
      --  App.Gestion_Folder.Button (15).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button SEPA
      --  App.Gestion_Folder.Button (16).Create (App.Gestion_Folder.Widget (4), "SEPA");
      --  App.Gestion_Folder.Button (16).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Folder Préférences
      --
      --  App.Gestion_Folder.Folder.Create_Section
      --    (Content => "<span class='rouge'>P</span>références", Name => "Preferences");
      --  App.Gestion_Folder.Widget (5).Create (App.Gestion_Folder.Folder);
      --
      --  --  --  --  Button Intervalles SEPA
      --  App.Gestion_Folder.Button (17).Create (App.Gestion_Folder.Widget (5), "Intervalles SEPA");
      --  App.Gestion_Folder.Button (17).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  --  --  --  Button Natures
      --  App.Gestion_Folder.Button (18).Create (App.Gestion_Folder.Widget (5), "Natures");
      --  App.Gestion_Folder.Button (18).On_Click_Handler (On_Simple_Button'Unrestricted_Access);
      --
      --  App.Gestion_Folder.Widget (1).Style ("top", "5px");
      --  App.Gestion_Folder.Widget (2).Style ("top", "45px");
      --  App.Gestion_Folder.Widget (3).Style ("top", "85px");
      --  App.Gestion_Folder.Widget (4).Style ("top", "125px");
      --  App.Gestion_Folder.Widget (5).Style ("top", "165px");
      --
      --  App.Gestion_Folder.Folder.Position (Gnoga.Gui.Element.Absolute);
      --
      --  App.Gestion_Folder.Folder.Render_Folder (True);
      --
      --  App.Menu_Folder.Cards.Add_Card (Name => App.Menu_Folder.Name_Accueil, Card => App.Menu_Folder.Widget (1)'Access, Show => True);
      --
--  App.Menu_Folder.Cards.Add_Card (Name => App.Menu_Folder.Name_Standard, Card => App.Menu_Folder.Widget (2)'Access);
      --
      --  App.Menu_Folder.Cards.Add_Card
      --    (Name => App.Menu_Folder.Name_Contrats_Gestion, Card => App.Menu_Folder.Widget (3)'Access);

      App.Exit_Button.Create (App.Content, "Stopper exécution");
      App.Exit_Button.Style ("width", "140px");
      App.Exit_Button.On_Click_Handler (On_Exit'Unrestricted_Access);

      App.Navigation_User_Buttons.Display ("none");
      App.Navigation_Browse_View.Display ("none");

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
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Results'Unrestricted_Access, "/result");

   Menu.Create_Parent (App_Name, ID_Main, On_Main'Unrestricted_Access);

   Menu.Add_Child (ID_Main, "Contrats", ID_Contract, On_Contract'Unrestricted_Access);
   Menu.Add_Child (ID_Contract, "Gestion", ID_Contract_Management, On_Contract_Management'Unrestricted_Access);
   Menu.Add_Child (ID_Contract, "Statistiques", ID_Contract_Stats, On_Contract_Stats'Unrestricted_Access);

   Menu.Add_Child (ID_Main, "Administration", ID_Administration, On_Administration'Unrestricted_Access);
   Menu.Add_Child
     (ID_Administration, "Utilisateurs", ID_Administration_Users, On_Administration_Users'Unrestricted_Access);
   Menu.Add_Child (ID_Administration, "Emails", ID_Administration_Emails, On_Administration_Emails'Unrestricted_Access);
   Menu.Add_Child
     (ID_Administration, "Gén. requêtes", ID_Administration_Gen, On_Administration_Gen'Unrestricted_Access);

   Gnoga.Server.Connection.On_Post_Handler (On_Post'Unrestricted_Access);
   Gnoga.Server.Connection.On_Post_Request_Handler (On_Post_Request'Unrestricted_Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end application1;
