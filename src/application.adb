with Gnoga.Gui.Base;
with Gnoga.Gui.Plugin;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Application.Multi_Connect;
with UXStrings; use UXStrings;

with Framework;

procedure Application is
   package Base renames Gnoga.Gui.Base;
   package Element renames Gnoga.Gui.Element;

   use all type Gnoga.String;

   App_Name : constant UXString := "GNOGA Framework";

   Lorem_Ipsum : constant UXString :=
     80 *
     "Lorem ipsum dolor sit amet. Aut consequatur ipsam eos inventore repellat et neque sint id tempora aliquid eos assumenda ullam ut quas nostrum.";

   -----------------------------------------------------------------------------
   --  CRUD Handlers
   -----------------------------------------------------------------------------

   --------------------
   --  Default CRUD  --
   --------------------

   procedure On_CRUD_File_Create (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "File_Create");
      Gnoga.Log ("Créer");
   end On_CRUD_File_Create;

   procedure On_CRUD_File_Edit (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "File_Edit");
      Gnoga.Log ("Modifier");
   end On_CRUD_File_Edit;

   procedure On_CRUD_File_Delete (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "File_Delete");
      Gnoga.Log ("Supprimer");

      Framework.Set_App_Title ("Clicked on delete");
   end On_CRUD_File_Delete;

   procedure On_CRUD_File_Export (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "File_Export");
      Gnoga.Log ("Exporter");
   end On_CRUD_File_Export;

   procedure On_CRUD_File_Import (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "File_Import");
      Gnoga.Log ("Importer");
   end On_CRUD_File_Import;

   procedure On_CRUD_File_Print (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "File_Print");
      Gnoga.Log ("Imprimer");
   end On_CRUD_File_Print;

   procedure On_CRUD_Edit_Copy (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Edit_Copy");
      Gnoga.Log ("Copier");
   end On_CRUD_Edit_Copy;

   procedure On_CRUD_Edit_Paste (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Edit_Paste");
      Gnoga.Log ("Coller");
   end On_CRUD_Edit_Paste;

   procedure On_CRUD_Show_Previous (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Show_Previous");
      Gnoga.Log ("Précédent");
   end On_CRUD_Show_Previous;

   procedure On_CRUD_Show_Next (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Show_Next");
      Gnoga.Log ("Suivant");
   end On_CRUD_Show_Next;

   procedure On_CRUD_Show_Search (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Show_Search");
      Gnoga.Log ("Rechercher");
   end On_CRUD_Show_Search;

   procedure Load_Default_CRUD_Roots (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Add_Element (Object, "File", "Fichier", "/css/icons/file.png");
      Framework.CRUD_Add_Element (Object, "Edit", "Éditer", "/css/icons/edit.png");
      Framework.CRUD_Add_Element (Object, "Show", "Afficher", "/css/icons/browse.png");
   end Load_Default_CRUD_Roots;

   procedure Load_Default_CRUD_Childs (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Add_Sub_Element (Object, "File_Create", "Créer", "File", On_CRUD_File_Create'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element (Object, "File_Edit", "Modifier", "File", On_CRUD_File_Edit'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element (Object, "File_Delete", "Supprimer", "File", On_CRUD_File_Delete'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element (Object, "File_Export", "Exporter", "File", On_CRUD_File_Export'Unrestricted_Access);
      Framework.CRUD_Add_Delimiter_Above (Object, "File_Export");
      Framework.CRUD_Set_Unclickable (Object, "File_Export");
      Framework.CRUD_Add_Sub_Element (Object, "File_Import", "Importer", "File", On_CRUD_File_Import'Unrestricted_Access);
      Framework.CRUD_Set_Unclickable (Object, "File_Import");
      Framework.CRUD_Add_Sub_Element (Object, "File_Print", "Imprimer", "File", On_CRUD_File_Print'Unrestricted_Access);
      Framework.CRUD_Add_Delimiter_Above (Object, "File_Print");

      Framework.CRUD_Add_Sub_Element (Object, "Edit_Copy", "Copier", "Edit", On_CRUD_Edit_Copy'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element (Object, "Edit_Paste", "Coller", "Edit", On_CRUD_Edit_Paste'Unrestricted_Access);

      Framework.CRUD_Add_Sub_Element (Object, "Show_Previous", "Précédent", "Show", On_CRUD_Show_Previous'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element (Object, "Show_Next", "Suivant", "Show", On_CRUD_Show_Next'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element (Object, "Show_Search", "Rechercher", "Show", On_CRUD_Show_Search'Unrestricted_Access);
   end Load_Default_CRUD_Childs;

   ---------------------
   --  Extended CRUD  --
   ---------------------

   procedure On_CRUD_Show_List (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Show_List");
      Gnoga.Log ("Lister");
   end On_CRUD_Show_List;

   procedure On_CRUD_Show_List_Bill (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Show_List_Bill");
      Gnoga.Log ("Lister Factures");
   end On_CRUD_Show_List_Bill;

   procedure On_CRUD_Show_List_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Show_List_SEPA");
      Gnoga.Log ("Lister SEPA");
   end On_CRUD_Show_List_SEPA;

   procedure On_CRUD_Validate_Bill (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Validate_Bill");
      Gnoga.Log ("Factures");
   end On_CRUD_Validate_Bill;

   procedure On_CRUD_Validate_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Validate_SEPA");
      Gnoga.Log ("SEPA");
   end On_CRUD_Validate_SEPA;

   procedure On_CRUD_Preferences_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Preferences_SEPA");
      Gnoga.Log ("Intervalles SEPA");
   end On_CRUD_Preferences_SEPA;

   procedure On_CRUD_Preferences_Service (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Preferences_Service");
      Gnoga.Log ("Type de Prestation");
   end On_CRUD_Preferences_Service;

   procedure On_CRUD_Security_Bug (Object : in out Base.Base_Type'Class) is
   begin
      Framework.CRUD_Notify_Sub_Element_Click (Object, "Security_Bug");
      Gnoga.Log ("Should not appear...");
   end On_CRUD_Security_Bug;

   -----------------------------------------------------------------------------
   --  Browser Handlers
   -----------------------------------------------------------------------------

   procedure On_Contract (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Contract");

      Framework.Content_Set_Title (Object, "Contrats");
      Framework.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Contract;

   procedure On_Contract_Stats (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Contract_Stats");

      Load_Default_CRUD_Roots (Object);
      Load_Default_CRUD_Childs (Object);
      Framework.CRUD_Load (Object);

      Framework.Content_Set_Title (Object, "Statistiques");
      Framework.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Contract_Stats;

   procedure On_Contract_Management (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Contract_Management");

      Framework.Content_Set_Title (Object, "Gestion");
      Framework.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Contract_Management;

   procedure On_Administration_Users (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Administration_Users");

      Framework.Content_Set_Title (Object, "Utilisateurs");
      Framework.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration_Users;

   procedure On_Administration_Emails (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Administration_Emails");

      Framework.Content_Set_Title (Object, "Emails");
      Framework.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration_Emails;

   procedure On_Administration_Gen (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Administration_Gen");

      Load_Default_CRUD_Roots (Object);
      Framework.CRUD_Add_Element (Object, "Validate", "Valider", "/css/icons/checklist.png");
      Framework.CRUD_Add_Element (Object, "Preferences", "Préférences", "/css/icons/settings.png");
      Framework.CRUD_Add_Element (Object, "Security", "Sécurité", "/css/icons/security.png");
      Framework.CRUD_Set_Unclickable (Object, "Security");

      Load_Default_CRUD_Childs (Object);

      Framework.CRUD_Add_Sub_Element (Object, "Show_List", "Lister", "Show", On_CRUD_Show_List'Unrestricted_Access);
      Framework.CRUD_Add_Delimiter_Above (Object, "Show_List");
      Framework.CRUD_Add_Sub_Element
        (Object, "Show_List_Bill", "Lister Factures", "Show", On_CRUD_Show_List_Bill'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element
        (Object, "Show_List_SEPA", "Lister SEPA", "Show", On_CRUD_Show_List_SEPA'Unrestricted_Access);

      Framework.CRUD_Add_Sub_Element
        (Object, "Validate_Bill", "Factures", "Validate", On_CRUD_Validate_Bill'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element
        (Object, "Validate_SEPA", "SEPA", "Validate", On_CRUD_Validate_SEPA'Unrestricted_Access);

      Framework.CRUD_Add_Sub_Element
        (Object, "Preferences_SEPA", "Intervalles SEPA", "Preferences", On_CRUD_Preferences_SEPA'Unrestricted_Access);
      Framework.CRUD_Add_Sub_Element
        (Object, "Preferences_Service", "Type de Prestation", "Preferences",
         On_CRUD_Preferences_Service'Unrestricted_Access);

      Framework.CRUD_Add_Sub_Element
        (Object, "Security_Bug", "Ne devrait pas être affiché...", "Security",
         On_CRUD_Security_Bug'Unrestricted_Access);

      Framework.CRUD_Load (Object);

      Framework.Content_Set_Title (Object, "Générer des requêtes");
      Framework.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration_Gen;

   procedure On_Administration (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Administration");

      Framework.Content_Set_Title (Object, "Administration");
      Framework.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration;

   -----------------------------------------------------------------------------
   --  Changelog
   -----------------------------------------------------------------------------
   procedure On_Changelog (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog");

      Framework.Content_Set_Title (Object, "Changelog");
      Framework.Content_Set_Text (Object, "");
   end On_Changelog;
   procedure On_Changelog_77974a0 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_77974a0");

      Framework.Content_Set_Title (Object, "Changelog 77974a0");
      Framework.Content_Set_Text (Object, "Cleaned code");
   end On_Changelog_77974a0;

   procedure On_Changelog_81b29b4 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_81b29b4");

      Framework.Content_Set_Title (Object, "Changelog 81b29b4");
      Framework.Content_Set_Text (Object, "Stable version, with Header, CRUD and Footer separated");
   end On_Changelog_81b29b4;

   procedure On_Changelog_b744a8d (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_b744a8d");

      Framework.Content_Set_Title (Object, "Changelog b744a8d");
      Framework.Content_Set_Text (Object, "Added delimiters and unclickable buttons for CRUD");
   end On_Changelog_b744a8d;

   procedure On_Changelog_24a2a78 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_24a2a78");

      Framework.Content_Set_Title (Object, "Changelog 24a2a78");
      Framework.Content_Set_Text (Object, "Removed useless log, moved Ada styling to CSS");
   end On_Changelog_24a2a78;

   procedure On_Changelog_575c406 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_575c406");

      Framework.Content_Set_Title (Object, "Changelog 575c406");
      Framework.Content_Set_Text (Object, "More scrolling stuff, fix content container scroll");
   end On_Changelog_575c406;

   procedure On_Changelog_058b918 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_058b918");

      Framework.Content_Set_Title (Object, "Changelog 058b918");
      Framework.Content_Set_Text (Object, "CRUD can now be specific to some menus, fixed bugs, tweaked icons");
   end On_Changelog_058b918;

   procedure On_Changelog_a2c0958 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_a2c0958");

      Framework.Content_Set_Title (Object, "Changelog a2c0958");
      Framework.Content_Set_Text (Object, "Tweaked CRUD");
   end On_Changelog_a2c0958;

   procedure On_Changelog_d827a26 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_d827a26");

      Framework.Content_Set_Title (Object, "Changelog d827a26");
      Framework.Content_Set_Text (Object, "Fixed previous fix on buttons");
   end On_Changelog_d827a26;

   procedure On_Changelog_de74c4e (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_de74c4e");

      Framework.Content_Set_Title (Object, "Changelog de74c4e");
      Framework.Content_Set_Text (Object, "Fixed various bugs and renamed application1 to Framework");
   end On_Changelog_de74c4e;

   procedure On_Changelog_4144e4d (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_4144e4d");

      Framework.Content_Set_Title (Object, "Changelog 4144e4d");
      Framework.Content_Set_Text (Object, "Added bash script to test website overloading");
   end On_Changelog_4144e4d;

   procedure On_Changelog_77207bc (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_77207bc");

      Framework.Content_Set_Title (Object, "Changelog 77207bc");
      Framework.Content_Set_Text (Object, "Toolbar now have items, shortcuts handling soon");
   end On_Changelog_77207bc;

   procedure On_Changelog_266bc83 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_266bc83");

      Framework.Content_Set_Title (Object, "Changelog 266bc83");
      Framework.Content_Set_Text (Object, "Added user menu");
   end On_Changelog_266bc83;

   procedure On_Changelog_0deda3d (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_0deda3d");

      Framework.Content_Set_Title (Object, "Changelog 0deda3d");
      Framework.Content_Set_Text (Object, "Added dialogs for user buttons. Dialogs are opened directly after creation.");
   end On_Changelog_0deda3d;

   procedure On_Changelog_9eef569 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_9eef569");

      Framework.Content_Set_Title (Object, "Changelog 9eef569");
      Framework.Content_Set_Text (Object, "Menu and Breadcrumb now use instances, easier to use");
   end On_Changelog_9eef569;

   procedure On_Changelog_e2cba7c (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_e2cba7c");

      Framework.Content_Set_Title (Object, "Changelog e2cba7c");
      Framework.Content_Set_Text (Object, "Tweaked button style");
   end On_Changelog_e2cba7c;

   procedure On_Changelog_5e86ba9 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_5e86ba9");

      Framework.Content_Set_Title (Object, "Changelog 5e86ba9");
      Framework.Content_Set_Text (Object, "Stable version, with menu management");
   end On_Changelog_5e86ba9;

   procedure On_Changelog_549626b (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_549626b");

      Framework.Content_Set_Title (Object, "Changelog 549626b");
      Framework.Content_Set_Text (Object, "Tweaked menu browser");
   end On_Changelog_549626b;

   procedure On_Changelog_9b69124 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_9b69124");

      Framework.Content_Set_Title (Object, "Changelog 9b69124");
      Framework.Content_Set_Text (Object, "Started responsive layout");
   end On_Changelog_9b69124;

   procedure On_Changelog_c86f2a6 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_c86f2a6");

      Framework.Content_Set_Title (Object, "Changelog c86f2a6");
      Framework.Content_Set_Text (Object, "Tweaked CSS");
   end On_Changelog_c86f2a6;

   procedure On_Changelog_1464be2 (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Header_Notify_Menu_Click (Object, "Changelog_1464be2");

      Framework.Content_Set_Title (Object, "Changelog 1464be2");
      Framework.Content_Set_Text (Object, "First files");
   end On_Changelog_1464be2;

   -----------------------------------------------------------------------------
   --  End of changelog
   -----------------------------------------------------------------------------

   procedure On_Exit (Object : in out Base.Base_Type'Class) is
      pragma Unreferenced (Object);
   begin
      Gnoga.Application.Multi_Connect.End_Application;
   exception
      when E : others =>
         Gnoga.Log (Message => "On_Exit: ", Occurrence => E);
   end On_Exit;

   procedure On_App_Menu (Object : in out Base.Base_Type'Class) is
      Exit_Button : constant Element.Pointer_To_Element_Class := new Element.Common.Button_Type;
   begin
      Framework.Header_Notify_Menu_Click (Object, "App_Menu");

      Load_Default_CRUD_Roots (Object);
      Load_Default_CRUD_Childs (Object);
      Framework.CRUD_Load (Object);

      Framework.Content_Set_Title (Object, App_Name);
      Framework.Content_Set_Text (Object, Lorem_Ipsum);

      Element.Common.Button_Access (Exit_Button).Create (Framework.Content_Parent (Object).all);
      Framework.Content_Parent (Object).all.Add_Element ("Exit button", Exit_Button);
      Exit_Button.Text ("Stopper exécution");
      Exit_Button.Style ("width", "140px");
      Exit_Button.On_Click_Handler (On_Exit'Unrestricted_Access);

   end On_App_Menu;

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
   --  On_Connect
   -----------------------------------------------------------------------------

   procedure On_Connect (Object : in out Base.Base_Type'Class) is
   begin
      Framework.Set_User_Name (Object, "User Name");

      Framework.Footer_Set_State_Text (Object, "Message de statut");
      Framework.Footer_Set_Permanent_Text (Object, "Informations permanentes");
   end On_Connect;

begin
   Framework.Setup (On_Connect'Unrestricted_Access, App_Name, "<h1>Server closed</h1>");
   Framework.Set_Browse_Icon ("/css/icons/widget.png");
   Framework.Set_User_Icon ("/css/icons/user.png");

   Framework.Header_Set_Root ("App_Menu", App_Name, On_App_Menu'Unrestricted_Access);

   Framework.Header_Add_Child ("Contract", "Contrats", "App_Menu", On_Contract'Unrestricted_Access);
   Framework.Header_Add_Child
     ("Contract_Management", "Gestion", "Contract", On_Contract_Management'Unrestricted_Access);
   Framework.Header_Add_Child ("Contract_Stats", "Statistiques", "Contract", On_Contract_Stats'Unrestricted_Access);

   Framework.Header_Add_Child ("Administration", "Administration", "App_Menu", On_Administration'Unrestricted_Access);
   Framework.Header_Add_Child
     ("Administration_Users", "Utilisateurs", "Administration", On_Administration_Users'Unrestricted_Access);
   Framework.Header_Add_Child
     ("Administration_Emails", "Emails", "Administration", On_Administration_Emails'Unrestricted_Access);
   Framework.Header_Add_Child
     ("Administration_Gen", "Gén. requêtes", "Administration", On_Administration_Gen'Unrestricted_Access);

   Framework.Header_Add_Child ("Changelog", "Changelog", "App_Menu", On_Changelog'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_77974a0", "Changelog 77974a0", "Changelog", On_Changelog_77974a0'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_81b29b4", "Changelog 81b29b4", "Changelog", On_Changelog_81b29b4'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_b744a8d", "Changelog b744a8d", "Changelog", On_Changelog_b744a8d'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_24a2a78", "Changelog 24a2a78", "Changelog", On_Changelog_24a2a78'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_575c406", "Changelog 575c406", "Changelog", On_Changelog_575c406'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_058b918", "Changelog 058b918", "Changelog", On_Changelog_058b918'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_a2c0958", "Changelog a2c0958", "Changelog", On_Changelog_a2c0958'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_d827a26", "Changelog d827a26", "Changelog", On_Changelog_d827a26'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_de74c4e", "Changelog de74c4e", "Changelog", On_Changelog_de74c4e'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_4144e4d", "Changelog 4144e4d", "Changelog", On_Changelog_4144e4d'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_77207bc", "Changelog 77207bc", "Changelog", On_Changelog_77207bc'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_266bc83", "Changelog 266bc83", "Changelog", On_Changelog_266bc83'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_0deda3d", "Changelog 0deda3d", "Changelog", On_Changelog_0deda3d'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_9eef569", "Changelog 9eef569", "Changelog", On_Changelog_9eef569'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_e2cba7c", "Changelog e2cba7c", "Changelog", On_Changelog_e2cba7c'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_5e86ba9", "Changelog 5e86ba9", "Changelog", On_Changelog_5e86ba9'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_549626b", "Changelog 549626b", "Changelog", On_Changelog_549626b'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_9b69124", "Changelog 9b69124", "Changelog", On_Changelog_9b69124'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_c86f2a6", "Changelog c86f2a6", "Changelog", On_Changelog_c86f2a6'Unrestricted_Access);
   Framework.Header_Add_Child ("Changelog_1464be2", "Changelog 1464be2", "Changelog", On_Changelog_1464be2'Unrestricted_Access);


   Framework.Header_Add_Web ("Aide en ligne", "https://google.com");
   Framework.Header_Add_Dialog
     ("Droits d'accès", "Ajouter les droits d'accès", "Confirmer", "Annuler", On_Confirm'Unrestricted_Access,
      On_Cancel'Unrestricted_Access);
   Framework.Header_Add_Dialog ("Connecté depuis...", "Ajouter durée de la connection");
   Framework.Header_Add_Dialog ("Connection précédente", "Ajouter la date de la dernière connection");
   Framework.Header_Add_Web ("À propos de...", "http://gnoga.com");
   Framework.Header_Add_Button ("Fermer l'application", On_Exit'Unrestricted_Access);

   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end Application;
