with Gnoga.Gui.Base;
with Gnoga.Gui.Plugin;
with Gnoga.Application.Multi_Connect;
with UXStrings; use UXStrings;

with V22;

procedure Application is

   package Base renames Gnoga.Gui.Base;

   use all type Gnoga.String;

   App_Name : constant UXString := "V22 GNOGA";

   Lorem_Ipsum : constant UXString :=
     10 *
     "Lorem ipsum dolor sit amet. Quo autem eaque ut sint molestias eos voluptate minus. Sed adipisci laudantium et molestias omnis aut error ducimus eum quia eligendi ut eius aliquid aut voluptas mollitia ut nemo porro. Rem cumque excepturi eos ducimus totam ex consectetur esse. Et incidunt delectus sit omnis pariatur et magnam itaque et eius quibusdam. Qui sint numquam est asperiores rerum ut reprehenderit consequatur aut corrupti voluptate et explicabo voluptas sed molestiae nobis. Est rerum labore et assumenda mollitia ad temporibus cupiditate aut facilis saepe qui ullam enim a quibusdam consectetur nam perferendis voluptate. In quia rerum 33 nihil fuga aut consequuntur omnis cum amet incidunt qui enim cumque cum enim consectetur. Qui aliquam veniam ea asperiores iste qui autem voluptatibus vel perspiciatis autem? Sed consectetur eligendi qui expedita ratione ea molestias laboriosam. Et neque eaque nam dolore dicta est repellat eligendi eos suscipit mollitia. Qui error sequi et saepe fuga eos error molestiae qui voluptatem ipsam aut minus tempore est quos inventore. Qui repellat dignissimos rem nemo repudiandae ex dolorem ipsa ut quidem debitis aut nihil quod aut ipsam consequuntur est minus possimus. Et voluptates officia ad sequi fugiat sed distinctio molestias. Hic nihil assumenda vel officia ullam non adipisci voluptatem vel asperiores autem et internos rerum et iusto nostrum ut voluptas alias. 33 nihil beatae sit Quis possimus sed error velit qui voluptatem tempore qui omnis inventore et eligendi velit est quas praesentium. Et odit quis ut illo cumque a veritatis facere est voluptate expedita qui dicta fuga et nulla magnam in quam ducimus. Sed rerum illum non quam nostrum et assumenda repellendus aut rerum omnis et praesentium galisum. Ut magnam quia et quibusdam inventore rem beatae natus nam repudiandae repellendus! Et obcaecati laboriosam et eius quam est consectetur nihil. Et eius praesentium sed beatae impedit ut voluptas dolorem in cumque quia eum molestiae incidunt et debitis optio. Qui amet sint 33 nulla quod ut asperiores asperiores nam necessitatibus harum aut autem voluptatem cum repellendus iste. Qui internos perspiciatis qui corporis commodi est dolores quia sit fugiat pariatur et earum quae non tempora voluptatem ex perferendis harum. At magni consequatur non inventore sint aut perspiciatis quos cum maiores beatae ab aperiam ullam ea maiores omnis? Aut Quis iste qui consequatur repellendus in quas soluta eum dolores rerum et quasi nulla ea voluptatem iusto? Quo esse illum est officia corrupti sit neque velit qui ducimus dolor. Eum eius consequatur et fugit beatae eum vitae ducimus aut asperiores provident. Sed eligendi corporis et nihil eius id accusantium earum ut obcaecati amet ea obcaecati tempora et voluptate minus et soluta consequuntur. Eos iste quidem aut porro eius et rerum quia qui veritatis beatae. Eum error veritatis ut nihil repellendus rem unde optio sit quam deserunt id quasi distinctio ut commodi repellendus. Sed sint galisum et nihil veniam ex rerum accusantium hic magnam voluptatem eum dolores dicta ut galisum eligendi. Eos sunt neque rem iure maxime cum tempore maxime eos nemo fugit et nobis facilis? Est odit excepturi sed voluptas maiores in galisum suscipit. Et quia itaque ex dolore dicta ut tenetur repudiandae et illum odit. Cum suscipit culpa non voluptatum nesciunt eum error ipsum eum rerum dolores et labore doloremque et deserunt similique. Sit maxime eius hic rerum vitae ab dolorem odio ex natus suscipit. Et obcaecati reprehenderit ut illum nesciunt aut enim dolores id dolorem debitis id quia itaque. ";
   -----------------------------------------------------------------------------
   --  CRUD Handlers
   -----------------------------------------------------------------------------

   --------------------
   --  Default CRUD  --
   --------------------

   procedure On_CRUD_File_Create (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Create");
      Gnoga.Log ("Créer");
   end On_CRUD_File_Create;

   procedure On_CRUD_File_Edit (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Edit");
      Gnoga.Log ("Modifier");
   end On_CRUD_File_Edit;

   procedure On_CRUD_File_Delete (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Delete");
      Gnoga.Log ("Supprimer");
   end On_CRUD_File_Delete;

   procedure On_CRUD_File_Export (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Export");
      Gnoga.Log ("Exporter");
   end On_CRUD_File_Export;

   procedure On_CRUD_File_Import (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Import");
      Gnoga.Log ("Importer");
   end On_CRUD_File_Import;

   procedure On_CRUD_File_Print (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Print");
      Gnoga.Log ("Imprimer");
   end On_CRUD_File_Print;

   procedure On_CRUD_Edit_Copy (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Edit_Copy");
      Gnoga.Log ("Copier");
   end On_CRUD_Edit_Copy;

   procedure On_CRUD_Edit_Paste (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Edit_Paste");
      Gnoga.Log ("Coller");
   end On_CRUD_Edit_Paste;

   procedure On_CRUD_Show_Previous (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_Previous");
      Gnoga.Log ("Précédent");
   end On_CRUD_Show_Previous;

   procedure On_CRUD_Show_Next (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_Next");
      Gnoga.Log ("Suivant");
   end On_CRUD_Show_Next;

   procedure On_CRUD_Show_Search (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_Search");
      Gnoga.Log ("Rechercher");
   end On_CRUD_Show_Search;

   procedure Load_Default_CRUD_Roots (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Add_Element (Object, "File", "Fichier", "/css/icons/file.png");
      V22.CRUD_Add_Element (Object, "Edit", "Éditer", "/css/icons/edit.png");
      V22.CRUD_Add_Element (Object, "Show", "Afficher", "/css/icons/browse.png");
   end Load_Default_CRUD_Roots;

   procedure Load_Default_CRUD_Childs (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Add_Sub_Element (Object, "File_Create", "Créer", "File", On_CRUD_File_Create'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element (Object, "File_Edit", "Modifier", "File", On_CRUD_File_Edit'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "File_Delete", "Supprimer", "File", On_CRUD_File_Delete'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "File_Export", "Exporter", "File", On_CRUD_File_Export'Unrestricted_Access);
      V22.CRUD_Add_Delimiter_Above (Object, "File_Export");
      V22.CRUD_Set_Unclickable (Object, "File_Export");
      V22.CRUD_Add_Sub_Element
        (Object, "File_Import", "Importer", "File", On_CRUD_File_Import'Unrestricted_Access);
      V22.CRUD_Set_Unclickable (Object, "File_Import");
      V22.CRUD_Add_Sub_Element (Object, "File_Print", "Imprimer", "File", On_CRUD_File_Print'Unrestricted_Access);
      V22.CRUD_Add_Delimiter_Above (Object, "File_Print");

      V22.CRUD_Add_Sub_Element (Object, "Edit_Copy", "Copier", "Edit", On_CRUD_Edit_Copy'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element (Object, "Edit_Paste", "Coller", "Edit", On_CRUD_Edit_Paste'Unrestricted_Access);

      V22.CRUD_Add_Sub_Element
        (Object, "Show_Previous", "Précédent", "Show", On_CRUD_Show_Previous'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element (Object, "Show_Next", "Suivant", "Show", On_CRUD_Show_Next'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "Show_Search", "Rechercher", "Show", On_CRUD_Show_Search'Unrestricted_Access);
   end Load_Default_CRUD_Childs;

   ---------------------
   --  Extended CRUD  --
   ---------------------

   procedure On_CRUD_Show_List (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_List");
      Gnoga.Log ("Lister");
   end On_CRUD_Show_List;

   procedure On_CRUD_Show_List_Bill (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_List_Bill");
      Gnoga.Log ("Lister Factures");
   end On_CRUD_Show_List_Bill;

   procedure On_CRUD_Show_List_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_List_SEPA");
      Gnoga.Log ("Lister SEPA");
   end On_CRUD_Show_List_SEPA;

   procedure On_CRUD_Validate_Bill (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Validate_Bill");
      Gnoga.Log ("Factures");
   end On_CRUD_Validate_Bill;

   procedure On_CRUD_Validate_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Validate_SEPA");
      Gnoga.Log ("SEPA");
   end On_CRUD_Validate_SEPA;

   procedure On_CRUD_Preferences_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Preferences_SEPA");
      Gnoga.Log ("Intervalles SEPA");
   end On_CRUD_Preferences_SEPA;

   procedure On_CRUD_Preferences_Service (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Preferences_Service");
      Gnoga.Log ("Type de Prestation");
   end On_CRUD_Preferences_Service;

   procedure On_CRUD_Security_Bug (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Security_Bug");
      Gnoga.Log ("Should not appear...");
   end On_CRUD_Security_Bug;

   -----------------------------------------------------------------------------
   --  Extended CRUD with different behaviour
   -----------------------------------------------------------------------------
   procedure Update_TTC (Object : in out Base.Base_Type'Class) is
      Price  : constant Integer := V22.Content_Group_Number_Get (Object, "Prix HT");
      TVA    : constant Integer := V22.Content_Group_Number_Get (Object, "TVA");
      Result : Integer;
   begin
      Result := Price + TVA;
      V22.Content_Group_Text_Set (Object, "TTC", From_UTF_8 (Result'Image) & " €");
   end Update_TTC;

   procedure On_CRUD_New_File_Create (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Create");
      V22.Content_Set_Title (Object, "Statistiques - Fichier");
      V22.Content_Clear_Text (Object);

      V22.Content_Group_Create (Object, "Spécification du contrat");
      V22.Content_Group_Add_Title (Object, "Détails du contrat", "Spécification du contrat");
      V22.Content_Group_Selection_Add (Object, "Type de contrat", "Spécification du contrat");
      V22.Content_Group_Selection_Add_Option (Object, "Type de contrat", "Standard", Enabled => True);
      V22.Content_Group_Selection_Add_Option (Object, "Type de contrat", "Modifié");
      V22.Content_Group_Selection_Add_Option (Object, "Type de contrat", "Complexe");
      V22.Content_Group_Date_Add (Object, "Date de création", "Spécification du contrat");
      V22.Content_Group_Number_Add (Object, "Récurrence", "Spécification du contrat");
      V22.Content_Group_Add_Title (Object, "Activité", "Spécification du contrat");
      V22.Content_Group_Number_Add (Object, "Durée d'engagement en jours", "Spécification du contrat");
      V22.Content_Group_Number_Set (Object, "Durée d'engagement en jours", 30);
      V22.Content_Group_Check_Box_Add (Object, "Contrat actif", "Spécification du contrat");
      V22.Content_Group_Check_Box_Checked (Object, "Contrat actif", True);
      V22.Content_Group_Number_Add (Object, "Durée du contrat en jours", "Spécification du contrat");
      V22.Content_Group_Number_Set (Object, "Durée du contrat en jours", 30);
      V22.Content_Group_Item_Lock (Object, "Durée du contrat en jours");

      V22.Content_Group_Create (Object, "Prix");
      V22.Content_Group_Number_Add (Object, "Prix HT", "Prix", Update_TTC'Unrestricted_Access);
      V22.Content_Group_Number_Set (Object, "Prix HT", 0);
      V22.Content_Group_Number_Add (Object, "TVA", "Prix", Update_TTC'Unrestricted_Access);
      V22.Content_Group_Number_Set (Object, "TVA", 0);
      V22.Content_Group_Text_Add (Object, "TTC", "Prix");

      V22.Content_Group_Create (Object, "Agenda");
      V22.Content_Group_Date_Add (Object, "Prochaine date de facturation", "Agenda");
      V22.Content_Group_Date_Add (Object, "Prochaine date d'échéance", "Agenda");

      V22.Content_Group_Create (Object, "Autres informations");
      V22.Content_Group_Text_Area_Add (Object, "Note publique", "Autres informations");
      V22.Content_Group_Text_Area_Add (Object, "Note privée", "Autres informations");

      V22.Content_Group_Item_Lock (Object, "Prochaine date d'échéance");
      V22.Content_Group_Item_Lock (Object, "Note privée");
      V22.Content_Group_Item_Lock (Object, "TTC");
      V22.Content_Group_Text_Set (Object, "TTC", "Remplissez les données précédentes");
      V22.Content_Group_Text_Set (Object, "TTC", "0 €");

      V22.CRUD_Set_Unclickable (Object, "File_Create");
      V22.CRUD_Set_Clickable (Object, "File_Edit");
      V22.CRUD_Set_Clickable (Object, "File_Delete");
      V22.CRUD_Set_Clickable (Object, "File_Print");
   end On_CRUD_New_File_Create;

   procedure On_CRUD_New_File_Edit (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Edit");
   end On_CRUD_New_File_Edit;

   procedure On_CRUD_New_File_Delete (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Delete");

      V22.Content_Set_Title (Object, "Statistiques");
      V22.Content_Set_Text (Object, Lorem_Ipsum);

      V22.CRUD_Set_Clickable (Object, "File_Create");
      V22.CRUD_Set_Unclickable (Object, "File_Edit");
      V22.CRUD_Set_Unclickable (Object, "File_Delete");
      V22.CRUD_Set_Unclickable (Object, "File_Print");
   end On_CRUD_New_File_Delete;

   procedure On_CRUD_New_File_Export (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Export");
   end On_CRUD_New_File_Export;

   procedure On_CRUD_New_File_Import (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Import");
   end On_CRUD_New_File_Import;

   procedure On_CRUD_New_File_Print (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "File_Print");
      V22.Print (Object);
   end On_CRUD_New_File_Print;

   procedure On_CRUD_New_Edit_Copy (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Edit_Copy");
   end On_CRUD_New_Edit_Copy;

   procedure On_CRUD_New_Edit_Paste (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Edit_Paste");
   end On_CRUD_New_Edit_Paste;

   procedure On_CRUD_New_Show_Previous (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_Previous");
   end On_CRUD_New_Show_Previous;

   procedure On_CRUD_New_Show_Next (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_Next");
   end On_CRUD_New_Show_Next;

   procedure On_CRUD_New_Show_Search (Object : in out Base.Base_Type'Class) is
   begin
      V22.CRUD_Notify_Sub_Element_Click (Object, "Show_Search");
   end On_CRUD_New_Show_Search;

   -----------------------------------------------------------------------------
   --  Browser Handlers
   -----------------------------------------------------------------------------

   procedure On_Contract (Object : in out Base.Base_Type'Class) is
   begin
      V22.Header_Notify_Menu_Click (Object, "Contract");

      V22.Content_Set_Title (Object, "Contrats");
      V22.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Contract;

   procedure On_Contract_Stats (Object : in out Base.Base_Type'Class) is
   begin
      V22.Header_Notify_Menu_Click (Object, "Contract_Stats");

      V22.Content_Set_Title (Object, "Statistiques");
      V22.Content_Set_Text (Object, Lorem_Ipsum);
      V22.CRUD_Add_Element (Object, "File", "Fichier", "/css/icons/file.png");
      V22.CRUD_Add_Element (Object, "Edit", "Éditer", "/css/icons/edit.png");
      V22.CRUD_Add_Element (Object, "Show", "Afficher", "/css/icons/browse.png");

      V22.CRUD_Add_Sub_Element
        (Object, "File_Create", "Créer", "File", On_CRUD_New_File_Create'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "File_Edit", "Modifier", "File", On_CRUD_New_File_Edit'Unrestricted_Access);
      V22.CRUD_Set_Unclickable (Object, "File_Edit");
      V22.CRUD_Add_Sub_Element
        (Object, "File_Delete", "Supprimer", "File", On_CRUD_New_File_Delete'Unrestricted_Access);
      V22.CRUD_Set_Unclickable (Object, "File_Delete");
      V22.CRUD_Add_Sub_Element
        (Object, "File_Export", "Exporter", "File", On_CRUD_New_File_Export'Unrestricted_Access);
      V22.CRUD_Add_Delimiter_Above (Object, "File_Export");
      V22.CRUD_Set_Unclickable (Object, "File_Export");
      V22.CRUD_Add_Sub_Element
        (Object, "File_Import", "Importer", "File", On_CRUD_New_File_Import'Unrestricted_Access);
      V22.CRUD_Set_Unclickable (Object, "File_Import");
      V22.CRUD_Add_Sub_Element
        (Object, "File_Print", "Imprimer", "File", On_CRUD_New_File_Print'Unrestricted_Access);
      V22.CRUD_Add_Delimiter_Above (Object, "File_Print");
      V22.CRUD_Set_Unclickable (Object, "File_Print");

      V22.CRUD_Add_Sub_Element (Object, "Edit_Copy", "Copier", "Edit", On_CRUD_New_Edit_Copy'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "Edit_Paste", "Coller", "Edit", On_CRUD_New_Edit_Paste'Unrestricted_Access);

      V22.CRUD_Add_Sub_Element
        (Object, "Show_Previous", "Précédent", "Show", On_CRUD_New_Show_Previous'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "Show_Next", "Suivant", "Show", On_CRUD_New_Show_Next'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "Show_Search", "Rechercher", "Show", On_CRUD_New_Show_Search'Unrestricted_Access);

      V22.CRUD_Load (Object);
   end On_Contract_Stats;

   procedure On_Contract_Management (Object : in out Base.Base_Type'Class) is
   begin
      V22.Header_Notify_Menu_Click (Object, "Contract_Management");

      V22.Content_Set_Title (Object, "Gestion");
      V22.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Contract_Management;

   procedure On_Administration_Users (Object : in out Base.Base_Type'Class) is
      Parent_Key : constant UXString := "Liste des utilisateurs";
      Dummy : Integer := 0;
      Index : Integer := 0;
   begin
      V22.Header_Notify_Menu_Click (Object, "Administration_Users");

      V22.Content_Set_Title (Object, "Utilisateurs");
      V22.Content_List_Create (Object, Parent_Key);
      V22.Content_List_Add_Variable (Object, "ID", Parent_Key);
      V22.Content_List_Add_Variable (Object, "Nom d'utilisateur", Parent_Key);
      V22.Content_List_Add_Variable (Object, "Email", Parent_Key);
      V22.Content_List_Add_Variable (Object, "Hash (25)", Parent_Key);

      V22.Content_List_Add_Variable (Object, "Nom", Parent_Key);
      V22.Content_List_Add_Variable (Object, "Prénom", Parent_Key);
      V22.Content_List_Add_Variable (Object, "Numéro de téléphone", Parent_Key);
      V22.Content_List_Add_Variable (Object, "Date de naissance", Parent_Key);
      V22.Content_List_Add_Variable (Object, "Ville", Parent_Key);

      for Data of v22.Identities loop
         Index := Index + 1;
         Dummy := V22.Content_List_Add_Item (Object, Parent_Key);
         V22.Content_List_Set_Variable (Object, From_UTF_8 (Index'Image), Dummy, Parent_Key);
         V22.Content_List_Set_Variable (Object, Data.User_Name, Dummy, Parent_Key);
         V22.Content_List_Set_Variable (Object, Data.Email, Dummy, Parent_Key);
         V22.Content_List_Set_Variable (Object, Head (Data.Password_Hash, 25), Dummy, Parent_Key);

         if Data.User_Name /= "Root User" then
            V22.Content_List_Set_Variable (Object, v22.Get (Data, "Surname"), Dummy, Parent_Key);
            V22.Content_List_Set_Variable (Object, v22.Get (Data, "Name"), Dummy, Parent_Key);
            V22.Content_List_Set_Variable (Object, v22.Get (Data, "Phone"), Dummy, Parent_Key);
            V22.Content_List_Set_Variable (Object, v22.Get (Data, "Date"), Dummy, Parent_Key);
            V22.Content_List_Set_Variable (Object, v22.Get (Data, "City"), Dummy, Parent_Key);
         end if;
      end loop;
   end On_Administration_Users;

   procedure On_Administration_Emails (Object : in out Base.Base_Type'Class) is
   begin
      V22.Header_Notify_Menu_Click (Object, "Administration_Emails");

      V22.Content_Set_Title (Object, "Emails");
      V22.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration_Emails;

   procedure On_Administration_Gen (Object : in out Base.Base_Type'Class) is
   begin
      V22.Header_Notify_Menu_Click (Object, "Administration_Gen");

      Load_Default_CRUD_Roots (Object);
      V22.CRUD_Add_Element (Object, "Validate", "Valider", "/css/icons/checklist.png");
      V22.CRUD_Add_Element (Object, "Preferences", "Préférences", "/css/icons/settings.png");
      V22.CRUD_Add_Element (Object, "Security", "Sécurité", "/css/icons/security.png");
      V22.CRUD_Set_Unclickable (Object, "Security");

      Load_Default_CRUD_Childs (Object);

      V22.CRUD_Add_Sub_Element (Object, "Show_List", "Lister", "Show", On_CRUD_Show_List'Unrestricted_Access);
      V22.CRUD_Add_Delimiter_Above (Object, "Show_List");
      V22.CRUD_Add_Sub_Element
        (Object, "Show_List_Bill", "Lister Factures", "Show", On_CRUD_Show_List_Bill'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "Show_List_SEPA", "Lister SEPA", "Show", On_CRUD_Show_List_SEPA'Unrestricted_Access);

      V22.CRUD_Add_Sub_Element
        (Object, "Validate_Bill", "Factures", "Validate", On_CRUD_Validate_Bill'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "Validate_SEPA", "SEPA", "Validate", On_CRUD_Validate_SEPA'Unrestricted_Access);

      V22.CRUD_Add_Sub_Element
        (Object, "Preferences_SEPA", "Intervalles SEPA", "Preferences", On_CRUD_Preferences_SEPA'Unrestricted_Access);
      V22.CRUD_Add_Sub_Element
        (Object, "Preferences_Service", "Type de Prestation", "Preferences",
         On_CRUD_Preferences_Service'Unrestricted_Access);

      V22.CRUD_Add_Sub_Element
        (Object, "Security_Bug", "Ne devrait pas être affiché...", "Security",
         On_CRUD_Security_Bug'Unrestricted_Access);

      V22.CRUD_Load (Object);

      V22.Content_Set_Title (Object, "Générer des requêtes");
      V22.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration_Gen;

   procedure On_Administration (Object : in out Base.Base_Type'Class) is
   begin
      V22.Header_Notify_Menu_Click (Object, "Administration");

      V22.Content_Set_Title (Object, "Administration");
      V22.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration;

   procedure On_App_Menu (Object : in out Base.Base_Type'Class) is
   begin
      V22.Header_Notify_Menu_Click (Object, "App_Menu");

      Load_Default_CRUD_Roots (Object);
      Load_Default_CRUD_Childs (Object);
      V22.CRUD_Load (Object);

      V22.Content_Set_Title (Object, App_Name);
      V22.Content_Set_Text (Object, Lorem_Ipsum);
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
      V22.Set_User_Name (Object, "");
      V22.Set_User_Icon (Object, "/css/icons/user.png");

      V22.Footer_Set_State_Text (Object, "Message de statut");
      V22.Footer_Set_Permanent_Text (Object, "Informations permanentes");
   end On_Connect;

   procedure On_Login (Object : in out Base.Base_Type'Class) is
   begin
      null;
   end On_Login;

   procedure On_Register_Create (Object : in out Base.Base_Type'Class) is
   begin
      V22.Content_Group_Add_Title (Object, "Données utilisateur", V22.Register_Group_Key);
      V22.Content_Group_Text_Add (Object, "Nom", V22.Register_Group_Key);
      V22.Content_Group_Text_Add (Object, "Prénom", V22.Register_Group_Key);
      V22.Content_Group_Phone_Add (Object, "Numéro de téléphone", V22.Register_Group_Key);
      V22.Content_Group_Date_Add (Object, "Date de naissance", V22.Register_Group_Key);
      V22.Content_Group_Text_Add (Object, "Ville", V22.Register_Group_Key);
   end On_Register_Create;

   function On_Register
     (Object   : in out Base.Base_Type'Class;
      Identity : in out V22.User_Data)
      return Boolean
   is
      Surname : constant UXString := V22.Content_Group_Text_Get (Object, "Nom");
      Name    : constant UXString := V22.Content_Group_Text_Get (Object, "Prénom");
      Phone   : constant UXString := V22.Content_Group_Phone_Get (Object, "Numéro de téléphone");
      Date    : constant UXString := V22.Content_Group_Date_Get (Object, "Date de naissance");
      City    : constant UXString := V22.Content_Group_Text_Get (Object, "Ville");
   begin
      if Surname = "" then
         V22.Set_Register_Error_Message (Object, "Entrez votre nom");
      else
         if Name = "" then
            V22.Set_Register_Error_Message (Object, "Entrez votre prénom");
         else
            if Phone = "" then
               V22.Set_Register_Error_Message (Object, "Entrez votre numéro de téléphone");
            else
               if Date = "" then
                  V22.Set_Register_Error_Message (Object, "Entrez votre date de naissance");
               else
                  if City = "" then
                     V22.Set_Register_Error_Message (Object, "Entrez votre ville");
                  else
                     V22.Set (Identity, "Surname", Surname);
                     V22.Set (Identity, "Name", Name);
                     V22.Set (Identity, "Phone", Phone);
                     V22.Set (Identity, "Date", Date);
                     V22.Set (Identity, "City", City);
                     return True;
                  end if;
               end if;
            end if;
         end if;
      end if;
      return False;
   end On_Register;

begin
   V22.Setup (On_Connect'Unrestricted_Access, App_Name, "<h1>Server closed</h1>");
   V22.Setup_Access
     (On_Login'Unrestricted_Access, On_Register_Create'Unrestricted_Access, On_Register'Unrestricted_Access);
   V22.Add_Root_User;

   V22.Set_Browse_Icon ("/css/icons/widget.png");
   V22.Set_Default_User_Icon ("/css/icons/user.png");

   V22.Header_Set_Root ("App_Menu", App_Name, On_App_Menu'Unrestricted_Access);

   V22.Header_Add_Child ("Contract", "Contrats", "App_Menu", On_Contract'Unrestricted_Access);
   V22.Header_Add_Child
     ("Contract_Management", "Gestion", "Contract", On_Contract_Management'Unrestricted_Access);
   V22.Header_Add_Child ("Contract_Stats", "Statistiques", "Contract", On_Contract_Stats'Unrestricted_Access);

   V22.Header_Add_Child ("Administration", "Administration", "App_Menu", On_Administration'Unrestricted_Access);
   V22.Header_Add_Child
     ("Administration_Users", "Utilisateurs", "Administration", On_Administration_Users'Unrestricted_Access);
   V22.Header_Add_Child
     ("Administration_Emails", "Emails", "Administration", On_Administration_Emails'Unrestricted_Access);
   V22.Header_Add_Child
     ("Administration_Gen", "Gén. requêtes", "Administration", On_Administration_Gen'Unrestricted_Access);

   V22.Header_Add_Web ("Aide en ligne", "https://google.com");
   V22.Header_Add_Dialog
     ("Droits d'accès", "Ajouter les droits d'accès", "Confirmer", "Annuler", On_Confirm'Unrestricted_Access,
      On_Cancel'Unrestricted_Access);
   V22.Header_Add_Dialog ("Connecté depuis...", "Ajouter durée de la connexion");
   V22.Header_Add_Dialog ("Connexion précédente", "Ajouter la date de la dernière connexion");
   V22.Header_Add_Web ("À propos de...", "http://gnoga.com");
   V22.Header_Add_Button ("Se déconnecter", V22.Disconnect_User'Unrestricted_Access);

   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end Application;
