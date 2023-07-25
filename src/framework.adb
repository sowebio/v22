with Gnoga.Application.Multi_Connect;
with Gnoga.Gui.Base;
with Gnoga.Gui.Window;
with Gnoga.Gui.Plugin;
with Gnoga.Gui.View;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Section;
with Gnoga.Types;
with UXStrings; use UXStrings;
with Header;
with CRUD;
with Footer;

procedure Framework is

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;
   package Element renames Gnoga.Gui.Element;

   use all type Gnoga.String;

   App_Name : constant UXString := "ADA Framework";

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Window    : Gnoga.Gui.Window.Pointer_To_Window_Class;
      Container : View.View_Type;

      User_Name : UXString := "User name";

      Header_Content : Header.Header_Type;

      Header_Parent : View.View_Type;
      Crud_Parent   : View.View_Type;
      Content       : View.View_Type;
      Footer_Parent : View.View_Type;

      Content_Header : Element.Section.Section_Type;
      Content_Text   : Element.Common.P_Type;

      Footer_Content : Footer.Footer_Type;

      Exit_Button : Element.Common.Button_Type;

      --  CRUD Data
      Crud_Instance : Crud.Crud_Type;

      ID_Crud_File        : Integer;
      ID_Crud_File_Create : Integer;
      ID_Crud_File_Edit   : Integer;
      ID_Crud_File_Delete : Integer;
      ID_Crud_File_Export : Integer;
      ID_Crud_File_Import : Integer;
      ID_Crud_File_Print  : Integer;

      ID_Crud_Edit       : Integer;
      ID_Crud_Edit_Copy  : Integer;
      ID_Crud_Edit_Paste : Integer;

      ID_Crud_Show           : Integer;
      ID_Crud_Show_Previous  : Integer;
      ID_Crud_Show_Next      : Integer;
      ID_Crud_Show_Search    : Integer;
      ID_Crud_Show_List      : Integer;
      ID_Crud_Show_List_Bill : Integer;
      ID_Crud_Show_List_SEPA : Integer;

      ID_Crud_Validate      : Integer;
      ID_Crud_Validate_Bill : Integer;
      ID_Crud_Validate_SEPA : Integer;

      ID_Crud_Preferences         : Integer;
      ID_Crud_Preferences_SEPA    : Integer;
      ID_Crud_Preferences_Service : Integer;

      ID_Crud_Security     : Integer;
      ID_Crud_Security_Bug : Integer;
   end record;
   type App_Access is access all App_Data;

   Lorem_Ipsum : constant UXString :=
     80 *
     "Lorem ipsum dolor sit amet. Aut consequatur ipsam eos inventore repellat et neque sint id tempora aliquid eos assumenda ullam ut quas nostrum.";

   -----------------------------------------------------------------------------
   --  CRUD Handlers
   -----------------------------------------------------------------------------
   procedure On_Crud_Callback (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Element_Click (Object);
   end On_Crud_Callback;

   procedure On_Key_Pressed
     (Object : in out Base.Base_Type'Class;
      Char   :        Character)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Key_Pressed (Char);
   end On_Key_Pressed;

   procedure On_Crud_File_Create (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_File_Create);
      Gnoga.Log ("Créer");
   end On_Crud_File_Create;

   procedure On_Crud_File_Edit (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_File_Edit);
      Gnoga.Log ("Éditer");
   end On_Crud_File_Edit;

   procedure On_Crud_File_Delete (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_File_Delete);
      Gnoga.Log ("Supprimer");
   end On_Crud_File_Delete;

   procedure On_Crud_File_Export (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_File_Export);
      Gnoga.Log ("Exporter");
   end On_Crud_File_Export;

   procedure On_Crud_File_Import (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_File_Import);
      Gnoga.Log ("Importer");
   end On_Crud_File_Import;

   procedure On_Crud_File_Print (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_File_Print);
      Gnoga.Log ("Imprimer");
   end On_Crud_File_Print;

   procedure On_Crud_Edit_Copy (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Edit_Copy);
      Gnoga.Log ("Copier");
   end On_Crud_Edit_Copy;

   procedure On_Crud_Edit_Paste (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Edit_Paste);
      Gnoga.Log ("Coller");
   end On_Crud_Edit_Paste;

   procedure On_Crud_Show_Previous (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Show_Previous);
      Gnoga.Log ("Précédent");
   end On_Crud_Show_Previous;

   procedure On_Crud_Show_Next (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Show_Next);
      Gnoga.Log ("Suivant");
   end On_Crud_Show_Next;

   procedure On_Crud_Show_Search (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Show_Search);
      Gnoga.Log ("Rechercher");
   end On_Crud_Show_Search;

   procedure On_Crud_Show_List (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Show_List);
      Gnoga.Log ("Lister");
   end On_Crud_Show_List;

   procedure On_Crud_Show_List_Bill (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Show_List_Bill);
      Gnoga.Log ("Lister Factures");
   end On_Crud_Show_List_Bill;

   procedure On_Crud_Show_List_SEPA (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Show_List_SEPA);
      Gnoga.Log ("Lister SEPA");
   end On_Crud_Show_List_SEPA;

   procedure On_Crud_Validate_Bill (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Validate_Bill);
      Gnoga.Log ("Factures");
   end On_Crud_Validate_Bill;

   procedure On_Crud_Validate_SEPA (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Validate_SEPA);
      Gnoga.Log ("SEPA");
   end On_Crud_Validate_SEPA;

   procedure On_Crud_Preferences_SEPA (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Preferences_SEPA);
      Gnoga.Log ("Intervalles SEPA");
   end On_Crud_Preferences_SEPA;

   procedure On_Crud_Preferences_Service (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Sub_Element_Click (App.ID_Crud_Preferences_Service);
      Gnoga.Log ("Type de Prestation");
   end On_Crud_Preferences_Service;

   procedure Load_Default_Crud_Roots (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.ID_Crud_File := App.Crud_Instance.Add_Element ("Fichier", "/css/icons/file.png");
      App.ID_Crud_Edit := App.Crud_Instance.Add_Element ("Éditer", "/css/icons/edit.png");
      App.ID_Crud_Show := App.Crud_Instance.Add_Element ("Afficher", "/css/icons/browse.png");
   end Load_Default_Crud_Roots;

   procedure Load_Default_Crud_Childs (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.ID_Crud_File_Create :=
        App.Crud_Instance.Add_Sub_Element ("Créer", App.ID_Crud_File, On_Crud_File_Create'Unrestricted_Access);
      App.ID_Crud_File_Edit :=
        App.Crud_Instance.Add_Sub_Element ("Modifier", App.ID_Crud_File, On_Crud_File_Edit'Unrestricted_Access);
      App.ID_Crud_File_Delete :=
        App.Crud_Instance.Add_Sub_Element ("Supprimer", App.ID_Crud_File, On_Crud_File_Delete'Unrestricted_Access);
      App.ID_Crud_File_Export :=
        App.Crud_Instance.Add_Sub_Element ("Exporter", App.ID_Crud_File, On_Crud_File_Export'Unrestricted_Access);
      App.Crud_Instance.Add_Delimiter_Above (App.ID_Crud_File_Export);
      App.Crud_Instance.Set_Unclickable (App.ID_Crud_File_Export);
      App.ID_Crud_File_Import :=
        App.Crud_Instance.Add_Sub_Element ("Importer", App.ID_Crud_File, On_Crud_File_Import'Unrestricted_Access);
      App.Crud_Instance.Set_Unclickable (App.ID_Crud_File_Import);
      App.ID_Crud_File_Print :=
        App.Crud_Instance.Add_Sub_Element ("Imprimer", App.ID_Crud_File, On_Crud_File_Print'Unrestricted_Access);
      App.Crud_Instance.Add_Delimiter_Above (App.ID_Crud_File_Print);

      App.ID_Crud_Edit_Copy :=
        App.Crud_Instance.Add_Sub_Element ("Copier", App.ID_Crud_Edit, On_Crud_Edit_Copy'Unrestricted_Access);
      App.ID_Crud_Edit_Paste :=
        App.Crud_Instance.Add_Sub_Element ("Coller", App.ID_Crud_Edit, On_Crud_Edit_Paste'Unrestricted_Access);

      App.ID_Crud_Show_Previous :=
        App.Crud_Instance.Add_Sub_Element ("Précédent", App.ID_Crud_Show, On_Crud_Show_Previous'Unrestricted_Access);
      App.ID_Crud_Show_Next :=
        App.Crud_Instance.Add_Sub_Element ("Suivant", App.ID_Crud_Show, On_Crud_Show_Next'Unrestricted_Access);
      App.ID_Crud_Show_Search :=
        App.Crud_Instance.Add_Sub_Element ("Rechercher", App.ID_Crud_Show, On_Crud_Show_Search'Unrestricted_Access);
   end Load_Default_Crud_Childs;

   -----------------------------------------------------------------------------
   --  Browser Handlers
   -----------------------------------------------------------------------------
   ID_Contract : Integer;
   procedure On_Contract (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Contract);
      App.Crud_Instance.Clear;

      App.Content_Header.Text ("Contrats");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract;

   ID_Contract_Stats : Integer;
   procedure On_Contract_Stats (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Contract_Stats);
      App.Crud_Instance.Clear;
      Load_Default_Crud_Roots (Object);
      Load_Default_Crud_Childs (Object);
      App.Crud_Instance.Load;

      App.Content_Header.Text ("Statistiques");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract_Stats;

   ID_Contract_Management : Integer;
   procedure On_Contract_Management (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Contract_Management);
      App.Crud_Instance.Clear;

      App.Content_Header.Text ("Gestion");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Contract_Management;

   ID_Administration_Users : Integer;
   procedure On_Administration_Users (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Administration_Users);
      App.Crud_Instance.Clear;

      App.Content_Header.Text ("Utilisateurs");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Users;

   ID_Administration_Emails : Integer;
   procedure On_Administration_Emails (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Administration_Emails);
      App.Crud_Instance.Clear;

      App.Content_Header.Text ("Emails");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Emails;

   ID_Administration_Gen : Integer;
   procedure On_Administration_Gen (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Administration_Gen);
      App.Crud_Instance.Clear;

      Load_Default_Crud_Roots (Object);
      App.ID_Crud_Validate    := App.Crud_Instance.Add_Element ("Valider", "/css/icons/checklist.png");
      App.ID_Crud_Preferences := App.Crud_Instance.Add_Element ("Préférences", "/css/icons/settings.png");
      App.ID_Crud_Security    := App.Crud_Instance.Add_Element ("Sécurité", "/css/icons/security.png");
      App.Crud_Instance.Set_Unclickable (App.ID_Crud_Security);

      Load_Default_Crud_Childs (Object);

      App.ID_Crud_Show_List :=
        App.Crud_Instance.Add_Sub_Element ("Lister", App.ID_Crud_Show, On_Crud_Show_List'Unrestricted_Access);
      App.Crud_Instance.Add_Delimiter_Above (App.ID_Crud_Show_List);
      App.ID_Crud_Show_List_Bill :=
        App.Crud_Instance.Add_Sub_Element
          ("Lister Factures", App.ID_Crud_Show, On_Crud_Show_List_Bill'Unrestricted_Access);
      App.ID_Crud_Show_List_SEPA :=
        App.Crud_Instance.Add_Sub_Element ("Lister SEPA", App.ID_Crud_Show, On_Crud_Show_List_SEPA'Unrestricted_Access);

      App.ID_Crud_Validate_Bill :=
        App.Crud_Instance.Add_Sub_Element ("Factures", App.ID_Crud_Validate, On_Crud_Validate_Bill'Unrestricted_Access);
      App.ID_Crud_Validate_SEPA :=
        App.Crud_Instance.Add_Sub_Element ("SEPA", App.ID_Crud_Validate, On_Crud_Validate_SEPA'Unrestricted_Access);

      App.ID_Crud_Preferences_SEPA :=
        App.Crud_Instance.Add_Sub_Element
          ("Intervalles SEPA", App.ID_Crud_Preferences, On_Crud_Preferences_SEPA'Unrestricted_Access);
      App.ID_Crud_Preferences_Service :=
        App.Crud_Instance.Add_Sub_Element
          ("Type de Prestation", App.ID_Crud_Preferences, On_Crud_Preferences_Service'Unrestricted_Access);

      App.ID_Crud_Security_Bug :=
        App.Crud_Instance.Add_Sub_Element ("Ne devrait pas être affiché...", App.ID_Crud_Security);

      App.Crud_Instance.Load;

      App.Content_Header.Text ("Générer des requêtes");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration_Gen;

   ID_Administration : Integer;
   procedure On_Administration (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Administration);
      App.Crud_Instance.Clear;

      App.Content_Header.Text ("Administration");
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Administration;

   ID_Main : Integer;
   procedure On_Main (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (ID_Main);
      App.Crud_Instance.Clear;

      App.Content_Header.Text (App_Name);
      App.Content_Text.Text (Lorem_Ipsum);
   end On_Main;

   procedure On_Confirm (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log ("Confirmed");
   end On_Confirm;

   procedure On_Cancel (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Log ("Cancelled");
   end On_Cancel;

   -----------------------------------------------------------------------------
   --  Menu Handlers
   -----------------------------------------------------------------------------
   procedure On_Logo (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.Header_Content.Is_Menu_Open then
         App.Header_Content.Close_Menu;
      else
         App.Crud_Instance.Clear; --  The reason why this function is here
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

   --  procedure On_Button_Click (Object : in out Base.Base_Type'Class) is
   --  begin
   --     Gnoga.Log (Object.jQuery_Execute ("text()"));
   --  end On_Button_Click;

   -----------------------------------------------------------------------------
   --  Tool Bar expand button
   -----------------------------------------------------------------------------
   procedure On_Tool_Bar_Expand (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Instance.Notify_Resize;
   end On_Tool_Bar_Expand;

   -----------------------------------------------------------------------------
   --  On_Exit
   -----------------------------------------------------------------------------
   procedure On_Exit (Object : in out Base.Base_Type'Class) is
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
      App.Window.On_Character_Handler (On_Key_Pressed'Unrestricted_Access);
      App.Container.Create (Screen);

      --  Containers
      App.Container.Style ("width", "100%");
      App.Container.Style ("height", "100%");

      App.Header_Parent.Create (App.Container);
      App.Header_Parent.Class_Name ("header");

      App.Content.Create (App.Container);
      App.Content.Class_Name ("content-container");

      App.Crud_Parent.Create (App.Container);
      App.Crud_Parent.Class_Name ("crud");

      App.Footer_Parent.Create (App.Container);
      App.Footer_Parent.Class_Name ("footer");

      --  Content
      App.Content_Header.Create (App.Content, Element.Section.H1);
      App.Content_Header.Class_Name ("content-header");
      App.Content_Text.Create (App.Content);

      App.Exit_Button.Create (App.Content, "Stopper exécution");
      App.Exit_Button.Style ("width", "140px");
      App.Exit_Button.On_Click_Handler (On_Exit'Unrestricted_Access);

      --  Header
      App.Header_Content.Create (App.Header_Parent, On_Logo'Unrestricted_Access, On_User'Unrestricted_Access);
      App.Header_Content.Set_App_Icon ("/css/icons/home.png");
      App.Header_Content.Set_User_Icon ("/css/icons/user.png");
      App.Header_Content.Set_User_Name ("User Name");
      App.Header_Content.Set_Menu (ID_Main);

      --  Footer
      App.Footer_Content.Create (App.Footer_Parent);
      App.Footer_Content.Set_State_Text ("Message de statut");
      App.Footer_Content.Set_Permanent_Text ("Informations permanentes");

      --  CRUD
      App.Crud_Instance.Create
        (App.Crud_Parent, On_Tool_Bar_Expand'Unrestricted_Access, On_Crud_Callback'Unrestricted_Access);

   end On_Connect;

begin
   Gnoga.Application.Title (App_Name);
   Gnoga.Application.HTML_On_Close ("Server closed.");
   Gnoga.Application.Multi_Connect.Initialize (Boot => "boot_jqueryui.html");
   Gnoga.Application.Multi_Connect.On_Connect_Handler (Event => On_Connect'Unrestricted_Access, Path => "default");

   ID_Main := Header.Set_Root (App_Name, On_Main'Unrestricted_Access);

   ID_Contract            := Header.Add_Child (ID_Main, "Contrats", On_Contract'Unrestricted_Access);
   ID_Contract_Management := Header.Add_Child (ID_Contract, "Gestion", On_Contract_Management'Unrestricted_Access);
   ID_Contract_Stats      := Header.Add_Child (ID_Contract, "Statistiques", On_Contract_Stats'Unrestricted_Access);

   ID_Administration       := Header.Add_Child (ID_Main, "Administration", On_Administration'Unrestricted_Access);
   ID_Administration_Users :=
     Header.Add_Child (ID_Administration, "Utilisateurs", On_Administration_Users'Unrestricted_Access);
   ID_Administration_Emails :=
     Header.Add_Child (ID_Administration, "Emails", On_Administration_Emails'Unrestricted_Access);
   ID_Administration_Gen :=
     Header.Add_Child (ID_Administration, "Gén. requêtes", On_Administration_Gen'Unrestricted_Access);

   Header.Add_Web ("Aide en ligne", "https://google.com");
   Header.Add_Dialog
     ("Droits d'accès", "Ajouter les droits d'accès", Confirm_Text => "Confirmer",
      Confirm_Handler => On_Confirm'Unrestricted_Access, Cancel_Text => "Annuler",
      Cancel_Handler                                               => On_Cancel'Unrestricted_Access);
   Header.Add_Dialog ("Connecté depuis...", "Ajouter durée de la connection");
   Header.Add_Dialog ("Connection précédente", "Ajouter la date de la dernière connection");
   Header.Add_Web ("À propos de...", "http://gnoga.com");

   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end Framework;
