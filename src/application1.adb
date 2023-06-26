with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.View; use Gnoga.Gui.View;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.View.Docker;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.View.Card;
with Gnoga.Server.Connection;
with Gnoga.Types;
with UXStrings;      use UXStrings;
with Breadcrumb;
with Simple_Form;
with Folders;

procedure application1 is

   use all type Gnoga.String;

   type Button_Set is array (Positive range <>) of Gnoga.Gui.Element.Common.Button_Type;

   type Widget_Set is array (Positive range <>) of aliased Gnoga.Gui.View.View_Type;

   -----------------------------------------------------------------------------
   --  Data
   -----------------------------------------------------------------------------

   --  Accordion Left
   type Menu_Accordion_Type is new Gnoga.Types.Connection_Data_Type with record
      Accordion           : aliased Gnoga.Gui.Plugin.jQueryUI.Widget.Accordion_Type;
      Cards               : aliased Gnoga.Gui.View.Card.Card_View_Type;

      Name_Accueil        : UXString := "Name_Accueil_1";
      Name_Contrats       : UXString := "Name_Contrats";
      Name_Administration : UXString := "Name_Administration";

      Widget : aliased Widget_Set (1 .. 3);
      Button : aliased Button_Set (1 .. 6);
   end record;

   type Menu_Folder_Type is new Gnoga.Types.Connection_Data_Type with record
      Cards                 : aliased Gnoga.Gui.View.Card.Card_View_Type;

      Name_Accueil          : UXString := "Name_Accueil_2";
      Name_Standard         : UXString := "Name_Standard";
      Name_Contrats_Gestion : UXString := "Name_Contrats_Gestion_2";

      Widget                : aliased Widget_Set (1 .. 3);
   end record;

   type Main_Frame_Type is new Gnoga.Types.Connection_Data_Type with record
      Main_Deck                  : aliased Gnoga.Gui.View.Docker.Docker_View_Type;
      Cards                      : aliased Gnoga.Gui.View.Card.Card_View_Type;

      Name_Accueil               : UXString := "Name_Accueil_3";
      Name_Contrats_Tab          : UXString := "Name_Contrats_Tab";
      Name_Contrats_Gestion      : UXString := "Name_Contrats_Gestion_3";
      Name_Administration_Tab    : UXString := "Name_Administration_Tab";
      Name_Administration_Utils  : UXString := "Name_Administration_Utils";
      Name_Administration_Emails : UXString := "Name_Administration_Emails";
      Name_Administration_Gen    : UXString := "Name_Administration_Gen";

      Widget : aliased Widget_Set (1 .. 7);
   end record;

   --  Accordion Right
   type User_Panel_Type is new Gnoga.Types.Connection_Data_Type with record
      Accordion : aliased Gnoga.Gui.Plugin.jQueryUI.Widget.Accordion_Type;
      Deck_User : aliased Gnoga.Gui.View.View_Type;

      Button    : aliased Button_Set (1 .. 5);
   end record;

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
      My_Window : Gnoga.Gui.Window.Pointer_To_Window_Class;
      My_Docker : Gnoga.Gui.View.Docker.Docker_View_Type;

      Top_Panel : aliased Gnoga.Gui.View.View_Type;

      Bottom_Panel       : aliased Gnoga.Gui.View.View_Type;
      Button_Msg_Statut  : aliased Gnoga.Gui.Element.Common.DIV_Type;
      Button_Infos_Perma : aliased Gnoga.Gui.Element.Common.DIV_Type;

      My_Exit : aliased Gnoga.Gui.Element.Common.Button_Type;

      Menu_Accordion : aliased Menu_Accordion_Type;
      Menu_Folder    : aliased Menu_Folder_Type;
      Main_Frame     : aliased Main_Frame_Type;
      User_Panel     : aliased User_Panel_Type;
      Stdr_Folder    : aliased Stdr_Folder_Type;
      Gestion_Folder : aliased Gestion_Folder_Type;

      Form_View_Gestion : Simple_Form.Form_View_Type;
   end record;
   type App_Access is access all App_Data;

   Form_Gestion_Row_Names : constant UXString :=
     "A,B,adresse,date de création,récurrence,durée d'engagement," &
     "tacite reconduction,contrat actif,nature du contrat,prix HT,TVA,prix TTC,prochaine date de facturation," &
     "prochaine date d'échéance,note publique,note privée,référence proposition";

   Current_Depth : Integer;

   Last_Parameters : Gnoga.Types.Data_Map_Type;

   -----------------------------------------------------------------------------
   --  Handlers
   -----------------------------------------------------------------------------
   procedure On_Main (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Accordion.Cards.Show_Card (App.Menu_Accordion.Name_Accueil);
      App.Menu_Folder.Cards.Show_Card (App.Menu_Folder.Name_Accueil);
      App.Main_Frame.Cards.Show_Card (App.Main_Frame.Name_Accueil);
      Current_Depth :=
        Breadcrumb.Update_Breadcrumb
          (View         => App.Top_Panel, Handler => On_Main'Unrestricted_Access, Content => "Accueil",
           Current_Depth => Current_Depth, Depth => 0);
   end On_Main;

   procedure On_Contrats (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Accordion.Cards.Show_Card (App.Menu_Accordion.Name_Contrats);
      App.Main_Frame.Cards.Show_Card (App.Main_Frame.Name_Contrats_Tab);
      App.Menu_Folder.Cards.Show_Card (App.Menu_Folder.Name_Standard);
      Current_Depth :=
        Breadcrumb.Update_Breadcrumb
          (View         => App.Top_Panel, Handler => On_Contrats'Unrestricted_Access, Content => "Contrats",
           Current_Depth => Current_Depth, Depth => 1);
   end On_Contrats;

   procedure On_Administration (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Menu_Accordion.Cards.Show_Card (App.Menu_Accordion.Name_Administration);
      App.Main_Frame.Cards.Show_Card (App.Main_Frame.Name_Administration_Tab);
      App.Menu_Folder.Cards.Show_Card (App.Menu_Folder.Name_Standard);
      Current_Depth :=
        Breadcrumb.Update_Breadcrumb
          (View         => App.Top_Panel, Handler => On_Administration'Unrestricted_Access, Content => "Administration",
           Current_Depth => Current_Depth, Depth => 1);
   end On_Administration;

   procedure On_Contrats_Gestion (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      App.Main_Frame.Cards.Show_Card (App.Main_Frame.Name_Contrats_Gestion);
      App.Menu_Folder.Cards.Show_Card (App.Menu_Folder.Name_Contrats_Gestion);
      Current_Depth :=
        Breadcrumb.Update_Breadcrumb
          (View         => App.Top_Panel, Handler => On_Contrats_Gestion'Unrestricted_Access, Content => "Gestion",
           Current_Depth => Current_Depth, Depth => 2);
   end On_Contrats_Gestion;

   procedure On_Administration_Utils (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      App.Menu_Folder.Cards.Show_Card (App.Menu_Folder.Name_Standard);
      App.Main_Frame.Cards.Show_Card (App.Main_Frame.Name_Administration_Utils);
      Current_Depth :=
        Breadcrumb.Update_Breadcrumb
          (View => App.Top_Panel, Handler => On_Administration_Utils'Unrestricted_Access, Content => "Utilisateurs",
           Current_Depth => Current_Depth, Depth => 2);
   end On_Administration_Utils;

   procedure On_Administration_Emails (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      App.Menu_Folder.Cards.Show_Card (App.Menu_Folder.Name_Standard);
      App.Main_Frame.Cards.Show_Card (App.Main_Frame.Name_Administration_Emails);
      Current_Depth :=
        Breadcrumb.Update_Breadcrumb
          (View         => App.Top_Panel, Handler => On_Administration_Emails'Unrestricted_Access, Content => "Emails",
           Current_Depth => Current_Depth, Depth => 2);
   end On_Administration_Emails;

   procedure On_Administration_Gen (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      App.Menu_Folder.Cards.Show_Card (App.Menu_Folder.Name_Standard);
      App.Main_Frame.Cards.Show_Card (App.Main_Frame.Name_Administration_Gen);
      Current_Depth :=
        Breadcrumb.Update_Breadcrumb
          (View => App.Top_Panel, Handler => On_Administration_Gen'Unrestricted_Access, Content => "Gen. Requêtes",
           Current_Depth => Current_Depth, Depth => 2);
   end On_Administration_Gen;

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
     (Main_Window : in out Gnoga.Gui.Window.Window_Type'Class;
      Connection  :        access Gnoga.Application.Multi_Connect.Connection_Holder_Type)
   is
      pragma Unreferenced (Connection);
      App : constant App_Access := new App_Data;

      procedure Widget_Create_Accueil
        (View   : in out Gnoga.Gui.View.View_Type;
         Parent : in out Gnoga.Gui.Base.Base_Type'Class;
         ID     : in     Gnoga.String := "")
      is
      begin
         Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
         View.Put_Line ("Accueil");
      end Widget_Create_Accueil;

      procedure Widget_Create_Contrats_Tab
        (View   : in out Gnoga.Gui.View.View_Type;
         Parent : in out Gnoga.Gui.Base.Base_Type'Class;
         ID     : in     Gnoga.String := "")
      is
      begin
         Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
         View.Put_Line ("Contrats Tableau de bord");
      end Widget_Create_Contrats_Tab;

      procedure Widget_Create_Contrats_Gestion
        (View   : in out Gnoga.Gui.View.View_Type;
         Parent : in out Gnoga.Gui.Base.Base_Type'Class;
         ID     : in     Gnoga.String := "")
      is
      begin
         Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
         View.Put_Line ("Contrats Gestion");
      end Widget_Create_Contrats_Gestion;

      procedure Widget_Create_Administration_Tab
        (View   : in out Gnoga.Gui.View.View_Type;
         Parent : in out Gnoga.Gui.Base.Base_Type'Class;
         ID     : in     Gnoga.String := "")
      is
      begin
         Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
         View.Put_Line ("Administration Tableau de bord");
      end Widget_Create_Administration_Tab;

      procedure Widget_Create_Administration_Utils
        (View   : in out Gnoga.Gui.View.View_Type;
         Parent : in out Gnoga.Gui.Base.Base_Type'Class;
         ID     : in     Gnoga.String := "")
      is
      begin
         Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
         View.Put_Line ("Administration Utilisateurs");
      end Widget_Create_Administration_Utils;

      procedure Widget_Create_Administration_Emails
        (View   : in out Gnoga.Gui.View.View_Type;
         Parent : in out Gnoga.Gui.Base.Base_Type'Class;
         ID     : in     Gnoga.String := "")
      is
      begin
         Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
         View.Put_Line ("Administration Emails");
      end Widget_Create_Administration_Emails;

      procedure Widget_Create_Administration_Gen
        (View   : in out Gnoga.Gui.View.View_Type;
         Parent : in out Gnoga.Gui.Base.Base_Type'Class;
         ID     : in     Gnoga.String := "")
      is
      begin
         Gnoga.Gui.View.View_Type (View).Create (Parent, ID);
         View.Put_Line ("Administration Génération de requêtes");
      end Widget_Create_Administration_Gen;

   begin
      App.My_Window := Main_Window'Unchecked_Access;
      Main_Window.Connection_Data (App);

      App.My_Docker.Create (Main_Window);

      App.My_Docker.Visible (False);
      App.Top_Panel.Create (App.My_Docker);
      App.My_Docker.Top_Dock (App.Top_Panel'Unchecked_Access);
      App.Bottom_Panel.Create (App.My_Docker);
      App.My_Docker.Bottom_Dock (App.Bottom_Panel'Unchecked_Access);

      --------------------------------------------------------------------------
      --  Top Left Menu
      --------------------------------------------------------------------------
      App.Menu_Accordion.Accordion.Create (App.Top_Panel);
      App.Menu_Accordion.Accordion.Create_Section ("Logo");
      App.Menu_Accordion.Cards.Create (App.Menu_Accordion.Accordion);

      App.Menu_Accordion.Widget (1).Create (App.Menu_Accordion.Cards);
      App.Menu_Accordion.Widget (2).Create (App.Menu_Accordion.Cards);
      App.Menu_Accordion.Widget (3).Create (App.Menu_Accordion.Cards);

      App.Menu_Accordion.Cards.Add_Class ("accordion_content");
      App.Menu_Accordion.Cards.Style ("overflow", "hidden");
      App.Menu_Accordion.Cards.Style ("padding", "0");

      App.Menu_Accordion.Button (1).Create (App.Menu_Accordion.Widget (1), "Contrats");
      App.Menu_Accordion.Button (1).On_Click_Handler (On_Contrats'Unrestricted_Access);

      App.Menu_Accordion.Button (2).Create (App.Menu_Accordion.Widget (1), "Administration");
      App.Menu_Accordion.Button (2).On_Click_Handler (On_Administration'Unrestricted_Access);

      App.Menu_Accordion.Button (3).Create (App.Menu_Accordion.Widget (2), "Gestion");
      App.Menu_Accordion.Button (3).On_Click_Handler (On_Contrats_Gestion'Unrestricted_Access);

      App.Menu_Accordion.Button (4).Create (App.Menu_Accordion.Widget (3), "Utilisateurs");
      App.Menu_Accordion.Button (4).On_Click_Handler (On_Administration_Utils'Unrestricted_Access);

      App.Menu_Accordion.Button (5).Create (App.Menu_Accordion.Widget (3), "Emails");
      App.Menu_Accordion.Button (5).On_Click_Handler (On_Administration_Emails'Unrestricted_Access);

      App.Menu_Accordion.Button (6).Create (App.Menu_Accordion.Widget (3), "Gen. Requêtes");
      App.Menu_Accordion.Button (6).On_Click_Handler (On_Administration_Gen'Unrestricted_Access);

      App.Menu_Accordion.Cards.Add_Card
        (Name => App.Menu_Accordion.Name_Accueil, Card => App.Menu_Accordion.Widget (1)'Access, Show => True);
      App.Menu_Accordion.Cards.Add_Card
        (Name => App.Menu_Accordion.Name_Contrats, Card => App.Menu_Accordion.Widget (2)'Access);
      App.Menu_Accordion.Cards.Add_Card
        (Name => App.Menu_Accordion.Name_Administration, Card => App.Menu_Accordion.Widget (3)'Access);

      App.Menu_Accordion.Accordion.Render_Accordion (True);

      --------------------------------------------------------------------------
      --  Top Right Menu
      --------------------------------------------------------------------------
      App.User_Panel.Accordion.Create (Parent => App.Top_Panel);
      App.User_Panel.Accordion.Create_Section ("Utilisateur");
      App.User_Panel.Deck_User.Create (App.User_Panel.Accordion);

      App.User_Panel.Deck_User.Add_Class ("accordion_content");
      App.User_Panel.Deck_User.Style ("overflow", "hidden");
      App.User_Panel.Deck_User.Style ("padding", "0");

      App.User_Panel.Button (1).Create (App.User_Panel.Deck_User, "Aide en ligne");
      App.User_Panel.Button (1).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.User_Panel.Button (2).Create (App.User_Panel.Deck_User, "Droits d'accès");
      App.User_Panel.Button (2).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.User_Panel.Button (3).Create (App.User_Panel.Deck_User, "Connecté depuis");
      App.User_Panel.Button (3).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.User_Panel.Button (4).Create (App.User_Panel.Deck_User, "Connexion précédente");
      App.User_Panel.Button (4).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.User_Panel.Button (5).Create (App.User_Panel.Deck_User, "À propos de");
      App.User_Panel.Button (5).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.User_Panel.Accordion.Render_Accordion (True);

      --------------------------------------------------------------------------
      --  Bottom menu
      --------------------------------------------------------------------------
      App.Button_Msg_Statut.Create (App.Bottom_Panel, "Messages de statuts");
      App.Button_Infos_Perma.Create (App.Bottom_Panel, "Informations permanentes");

      --------------------------------------------------------------------------
      --  Main_Frame
      --------------------------------------------------------------------------
      App.Main_Frame.Main_Deck.Create (App.My_Docker);
      App.My_Docker.Fill_Dock (App.Main_Frame.Main_Deck'Unchecked_Access);

      --------------------------------------------------------------------------
      --  Left Menu
      --------------------------------------------------------------------------
      App.Menu_Folder.Cards.Create (App.Main_Frame.Main_Deck);
      App.Main_Frame.Main_Deck.Left_Dock (App.Menu_Folder.Cards'Unchecked_Access);

      App.Menu_Folder.Widget (1).Create (App.Menu_Folder.Cards);
      App.Menu_Folder.Widget (2).Create (App.Menu_Folder.Cards);
      App.Menu_Folder.Widget (3).Create (App.Menu_Folder.Cards);

      --------------------------------------------------------

      App.Stdr_Folder.Folder.Create_Folder (App.Menu_Folder.Widget (2));

      --  --  --  Folder Fichier

      App.Stdr_Folder.Folder.Create_Section (Content => "<span class='rouge'>F</span>ichier", Name => "Fichier");
      App.Stdr_Folder.Widget (1).Create (App.Stdr_Folder.Folder);

      --  --  --  Button Créer

      App.Stdr_Folder.Button (1).Create (App.Stdr_Folder.Widget (1), "Créer");
      App.Stdr_Folder.Button (1).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Modifier
      App.Stdr_Folder.Button (2).Create (App.Stdr_Folder.Widget (1), "Modifier");
      App.Stdr_Folder.Button (2).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Supprimer
      App.Stdr_Folder.Button (3).Create (App.Stdr_Folder.Widget (1), "Supprimer");
      App.Stdr_Folder.Button (3).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Exporter
      App.Stdr_Folder.Button (4).Create (App.Stdr_Folder.Widget (1), "Exporter");
      App.Stdr_Folder.Button (4).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Importer
      App.Stdr_Folder.Button (5).Create (App.Stdr_Folder.Widget (1), "Importer");
      App.Stdr_Folder.Button (5).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Imprimer
      App.Stdr_Folder.Button (6).Create (App.Stdr_Folder.Widget (1), "Imprimer");
      App.Stdr_Folder.Button (6).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Folder Éditer

      App.Stdr_Folder.Folder.Create_Section (Content => "<span class='rouge'>É</span>diter", Name => "Editer");
      App.Stdr_Folder.Widget (2).Create (App.Stdr_Folder.Folder);

      --  --  --  Button Copier
      App.Stdr_Folder.Button (7).Create (App.Stdr_Folder.Widget (2), "Copier");
      App.Stdr_Folder.Button (7).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Coller
      App.Stdr_Folder.Button (8).Create (App.Stdr_Folder.Widget (2), "Coller");
      App.Stdr_Folder.Button (8).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Folder Afficher

      App.Stdr_Folder.Folder.Create_Section (Content => "<span class='rouge'>A</span>fficher", Name => "Afficher");
      App.Stdr_Folder.Widget (3).Create (App.Stdr_Folder.Folder);

      --  --  --  Button Précédent
      App.Stdr_Folder.Button (9).Create (App.Stdr_Folder.Widget (3), "Précédent");
      App.Stdr_Folder.Button (9).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Suivant
      App.Stdr_Folder.Button (10).Create (App.Stdr_Folder.Widget (3), "Suivant");
      App.Stdr_Folder.Button (10).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Rechercher
      App.Stdr_Folder.Button (11).Create (App.Stdr_Folder.Widget (3), "Rechercher");
      App.Stdr_Folder.Button (11).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.Stdr_Folder.Folder.Render_Folder (True);

      --------------------------------------------------------------------------

      App.Gestion_Folder.Folder.Create_Folder (App.Menu_Folder.Widget (3));

      --  --  --  Folder Fichier

      App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>F</span>ichier", Name => "Fichier");
      App.Gestion_Folder.Widget (1).Create (App.Gestion_Folder.Folder);

      --  --  --  Button Créer
      App.Gestion_Folder.Button (1).Create (App.Gestion_Folder.Widget (1), "Créer");
      App.Gestion_Folder.Button (1).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Modifier
      App.Gestion_Folder.Button (2).Create (App.Gestion_Folder.Widget (1), "Modifier");
      App.Gestion_Folder.Button (2).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Supprimer
      App.Gestion_Folder.Button (3).Create (App.Gestion_Folder.Widget (1), "Supprimer");
      App.Gestion_Folder.Button (3).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Exporter
      App.Gestion_Folder.Button (4).Create (App.Gestion_Folder.Widget (1), "Exporter");
      App.Gestion_Folder.Button (4).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Importer
      App.Gestion_Folder.Button (5).Create (App.Gestion_Folder.Widget (1), "Importer");
      App.Gestion_Folder.Button (5).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Imprimer
      App.Gestion_Folder.Button (6).Create (App.Gestion_Folder.Widget (1), "Imprimer");
      App.Gestion_Folder.Button (6).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Folder Éditer

      App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>É</span>diter", Name => "Editer");
      App.Gestion_Folder.Widget (2).Create (App.Gestion_Folder.Folder);

      --  --  --  Button Copier
      App.Gestion_Folder.Button (7).Create (App.Gestion_Folder.Widget (2), "Copier");
      App.Gestion_Folder.Button (7).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Coller
      App.Gestion_Folder.Button (8).Create (App.Gestion_Folder.Widget (2), "Coller");
      App.Gestion_Folder.Button (8).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Folder Afficher

      App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>A</span>fficher", Name => "Afficher");
      App.Gestion_Folder.Widget (3).Create (App.Gestion_Folder.Folder);

      --  --  --  Button Précédent
      App.Gestion_Folder.Button (9).Create (App.Gestion_Folder.Widget (3), "Précédent");
      App.Gestion_Folder.Button (9).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Suivant
      App.Gestion_Folder.Button (10).Create (App.Gestion_Folder.Widget (3), "Suivant");
      App.Gestion_Folder.Button (10).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Rechercher
      App.Gestion_Folder.Button (11).Create (App.Gestion_Folder.Widget (3), "Rechercher");
      App.Gestion_Folder.Button (11).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.Gestion_Folder.Line.Create (App.Gestion_Folder.Widget (3));
      App.Gestion_Folder.Line.Style ("border-bottom", "2px solid black");

      --  --  --  Button Lister
      App.Gestion_Folder.Button (12).Create (App.Gestion_Folder.Widget (3), "Lister");
      App.Gestion_Folder.Button (12).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Lister Factures
      App.Gestion_Folder.Button (13).Create (App.Gestion_Folder.Widget (3), "Lister Factures");
      App.Gestion_Folder.Button (13).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Lister SEPA
      App.Gestion_Folder.Button (14).Create (App.Gestion_Folder.Widget (3), "Lister SEPA");
      App.Gestion_Folder.Button (14).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Folder Valider

      App.Gestion_Folder.Folder.Create_Section (Content => "<span class='rouge'>V</span>alider", Name => "Valider");
      App.Gestion_Folder.Widget (4).Create (App.Gestion_Folder.Folder);

      --  --  --  Button Factures
      App.Gestion_Folder.Button (15).Create (App.Gestion_Folder.Widget (4), "Factures");
      App.Gestion_Folder.Button (15).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button SEPA
      App.Gestion_Folder.Button (16).Create (App.Gestion_Folder.Widget (4), "SEPA");
      App.Gestion_Folder.Button (16).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Folder Préférences

      App.Gestion_Folder.Folder.Create_Section
        (Content => "<span class='rouge'>P</span>références", Name => "Preferences");
      App.Gestion_Folder.Widget (5).Create (App.Gestion_Folder.Folder);

      --  --  --  Button Intervalles SEPA
      App.Gestion_Folder.Button (17).Create (App.Gestion_Folder.Widget (5), "Intervalles_SEPA");
      App.Gestion_Folder.Button (17).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      --  --  --  Button Natures
      App.Gestion_Folder.Button (18).Create (App.Gestion_Folder.Widget (5), "Natures");
      App.Gestion_Folder.Button (18).On_Click_Handler (On_Simple_Button'Unrestricted_Access);

      App.Gestion_Folder.Widget (1).Style ("top", "5px");
      App.Gestion_Folder.Widget (2).Style ("top", "45px");
      App.Gestion_Folder.Widget (3).Style ("top", "85px");
      App.Gestion_Folder.Widget (4).Style ("top", "125px");
      App.Gestion_Folder.Widget (5).Style ("top", "165px");

      App.Gestion_Folder.Folder.Position (Gnoga.Gui.Element.Absolute);

      App.Gestion_Folder.Folder.Render_Folder (True);

      App.Menu_Folder.Cards.Add_Card
        (Name => App.Menu_Folder.Name_Accueil, Card => App.Menu_Folder.Widget (1)'Access, Show => True);

      App.Menu_Folder.Cards.Add_Card (Name => App.Menu_Folder.Name_Standard, Card => App.Menu_Folder.Widget (2)'Access);

      App.Menu_Folder.Cards.Add_Card
        (Name => App.Menu_Folder.Name_Contrats_Gestion, Card => App.Menu_Folder.Widget (3)'Access);

      --------------------------------------------------------------------------
      --  Main_FrameView
      --------------------------------------------------------------------------
      App.Main_Frame.Cards.Create (App.Main_Frame.Main_Deck);
      App.Main_Frame.Main_Deck.Fill_Dock (App.Main_Frame.Cards'Unchecked_Access);

      Widget_Create_Accueil (App.Main_Frame.Widget (1), App.Main_Frame.Cards);
      App.Main_Frame.Cards.Add_Card
        (Name => App.Main_Frame.Name_Accueil, Card => App.Main_Frame.Widget (1)'Access, Show => True);
      --
      Widget_Create_Contrats_Tab (App.Main_Frame.Widget (2), App.Main_Frame.Cards);
      App.Main_Frame.Cards.Add_Card
        (Name => App.Main_Frame.Name_Contrats_Tab, Card => App.Main_Frame.Widget (2)'Access);
      --
      Widget_Create_Contrats_Gestion (App.Main_Frame.Widget (3), App.Main_Frame.Cards);
      App.Main_Frame.Cards.Add_Card
        (Name => App.Main_Frame.Name_Contrats_Gestion, Card => App.Main_Frame.Widget (3)'Access);

      App.Form_View_Gestion.Create
        (Parent  => App.Main_Frame.Widget (3), ID => "",
         Strings => Simple_Form.All_Fields (Form_Gestion_Row_Names, ","));

      Widget_Create_Administration_Tab (App.Main_Frame.Widget (4), App.Main_Frame.Cards);
      App.Main_Frame.Cards.Add_Card
        (Name => App.Main_Frame.Name_Administration_Tab, Card => App.Main_Frame.Widget (4)'Access);

      Widget_Create_Administration_Utils (App.Main_Frame.Widget (5), App.Main_Frame.Cards);
      App.Main_Frame.Cards.Add_Card
        (Name => App.Main_Frame.Name_Administration_Utils, Card => App.Main_Frame.Widget (5)'Access);

      Widget_Create_Administration_Emails (App.Main_Frame.Widget (6), App.Main_Frame.Cards);
      App.Main_Frame.Cards.Add_Card
        (Name => App.Main_Frame.Name_Administration_Emails, Card => App.Main_Frame.Widget (6)'Access);

      Widget_Create_Administration_Gen (App.Main_Frame.Widget (7), App.Main_Frame.Cards);
      App.Main_Frame.Cards.Add_Card
        (Name => App.Main_Frame.Name_Administration_Gen, Card => App.Main_Frame.Widget (7)'Access);

      --------------------------------------------------------------------------
      --  Breadcrumb + Main button
      --------------------------------------------------------------------------
      Current_Depth := -1;
      -- ne se réinitialise pas lors d'un refresh de la page si affectation "Current_Depth = -1" placée avant le "begin"
      Current_Depth :=
        Breadcrumb.Add_To_Breadcrumb
          (View         => App.Top_Panel, Handler => On_Main'Unrestricted_Access, Content => "Accueil",
           Current_Depth => Current_Depth, Depth => 0);

      --------------------------------------------------------------------------

      App.My_Exit.Create (App.Bottom_Panel, "Stopper l'exécution");
      App.My_Exit.On_Click_Handler (On_Exit'Unrestricted_Access);

      --------------------------------------------------------------------------
      --  Style
      --------------------------------------------------------------------------
      App.User_Panel.Accordion.Add_Class ("element_top_right border");
      App.Main_Frame.Main_Deck.Style ("top", "180px");
      App.Main_Frame.Main_Deck.Style ("left", "0px");
      App.Menu_Folder.Cards.Add_Class ("element_top_left border");
      App.Menu_Folder.Cards.Style ("width", "120px");
      App.Main_Frame.Cards.Style ("position", "absolute");
      App.Main_Frame.Cards.Style ("top", "0px");
      App.Main_Frame.Cards.Style ("left", "120px");
      App.Top_Panel.Add_Class ("element_top_left border");
      App.Top_Panel.Style ("height", "120px");
      App.Bottom_Panel.Add_Class ("element_bottom_left border");
      App.Bottom_Panel.Style ("height", "50px");
      App.Bottom_Panel.Style ("z-index", "2");
      App.Button_Msg_Statut.Add_Class ("element_bottom_left");
      App.Button_Infos_Perma.Add_Class ("element_bottom_right");
      App.Menu_Accordion.Cards.Style ("height", "auto");
      App.Menu_Accordion.Widget (1).Style ("height", "auto");
      App.Menu_Accordion.Widget (2).Style ("height", "auto");
      App.Menu_Accordion.Widget (3).Style ("height", "auto");
      App.My_Docker.Visible;

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
   Gnoga.Application.Title ("application-test");
   Gnoga.Application.HTML_On_Close ("Server closed.");
   Gnoga.Application.Multi_Connect.Initialize;
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Results'Unrestricted_Access, "/result");
   Gnoga.Server.Connection.On_Post_Handler (On_Post'Unrestricted_Access);
   Gnoga.Server.Connection.On_Post_Request_Handler (On_Post_Request'Unrestricted_Access);
   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end application1;
