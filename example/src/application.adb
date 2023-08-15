with Gnoga.Gui.Base;
with Gnoga.Application.Multi_Connect;
with UXStrings;             use UXStrings;
with UXStrings.Conversions; use UXStrings.Conversions;
with UXStrings.Hash;
with Ada.Containers.Hashed_Maps;

with v22; -- use v22;
with v22.Gui;

procedure Application is

   use v22;

   package Base renames Gnoga.Gui.Base;

   use all type Gnoga.String;

   App_Name : constant UXString := "GNOGA - v22";

   Lorem_Ipsum : constant UXString :=
     10 *
     "Lorem ipsum dolor sit amet. Quo autem eaque ut sint molestias eos voluptate minus. Sed adipisci laudantium et molestias omnis aut error ducimus eum quia eligendi ut eius aliquid aut voluptas mollitia ut nemo porro. Rem cumque excepturi eos ducimus totam ex consectetur esse. Et incidunt delectus sit omnis pariatur et magnam itaque et eius quibusdam. Qui sint numquam est asperiores rerum ut reprehenderit consequatur aut corrupti voluptate et explicabo voluptas sed molestiae nobis. Est rerum labore et assumenda mollitia ad temporibus cupiditate aut facilis saepe qui ullam enim a quibusdam consectetur nam perferendis voluptate. In quia rerum 33 nihil fuga aut consequuntur omnis cum amet incidunt qui enim cumque cum enim consectetur. Qui aliquam veniam ea asperiores iste qui autem voluptatibus vel perspiciatis autem? Sed consectetur eligendi qui expedita ratione ea molestias laboriosam. Et neque eaque nam dolore dicta est repellat eligendi eos suscipit mollitia. Qui error sequi et saepe fuga eos error molestiae qui voluptatem ipsam aut minus tempore est quos inventore. Qui repellat dignissimos rem nemo repudiandae ex dolorem ipsa ut quidem debitis aut nihil quod aut ipsam consequuntur est minus possimus. Et voluptates officia ad sequi fugiat sed distinctio molestias. Hic nihil assumenda vel officia ullam non adipisci voluptatem vel asperiores autem et internos rerum et iusto nostrum ut voluptas alias. 33 nihil beatae sit Quis possimus sed error velit qui voluptatem tempore qui omnis inventore et eligendi velit est quas praesentium. Et odit quis ut illo cumque a veritatis facere est voluptate expedita qui dicta fuga et nulla magnam in quam ducimus. Sed rerum illum non quam nostrum et assumenda repellendus aut rerum omnis et praesentium galisum. Ut magnam quia et quibusdam inventore rem beatae natus nam repudiandae repellendus! Et obcaecati laboriosam et eius quam est consectetur nihil. Et eius praesentium sed beatae impedit ut voluptas dolorem in cumque quia eum molestiae incidunt et debitis optio. Qui amet sint 33 nulla quod ut asperiores asperiores nam necessitatibus harum aut autem voluptatem cum repellendus iste. Qui internos perspiciatis qui corporis commodi est dolores quia sit fugiat pariatur et earum quae non tempora voluptatem ex perferendis harum. At magni consequatur non inventore sint aut perspiciatis quos cum maiores beatae ab aperiam ullam ea maiores omnis? Aut Quis iste qui consequatur repellendus in quas soluta eum dolores rerum et quasi nulla ea voluptatem iusto? Quo esse illum est officia corrupti sit neque velit qui ducimus dolor. Eum eius consequatur et fugit beatae eum vitae ducimus aut asperiores provident. Sed eligendi corporis et nihil eius id accusantium earum ut obcaecati amet ea obcaecati tempora et voluptate minus et soluta consequuntur. Eos iste quidem aut porro eius et rerum quia qui veritatis beatae. Eum error veritatis ut nihil repellendus rem unde optio sit quam deserunt id quasi distinctio ut commodi repellendus. Sed sint galisum et nihil veniam ex rerum accusantium hic magnam voluptatem eum dolores dicta ut galisum eligendi. Eos sunt neque rem iure maxime cum tempore maxime eos nemo fugit et nobis facilis? Est odit excepturi sed voluptas maiores in galisum suscipit. Et quia itaque ex dolore dicta ut tenetur repudiandae et illum odit. Cum suscipit culpa non voluptatum nesciunt eum error ipsum eum rerum dolores et labore doloremque et deserunt similique. Sit maxime eius hic rerum vitae ab dolorem odio ex natus suscipit. Et obcaecati reprehenderit ut illum nesciunt aut enim dolores id dolorem debitis id quia itaque. ";

   function Int_Value is new Integer_Value (Integer);

   function To_UXString
     (Value : Integer)
      return UXString
   is
   begin
      return From_UTF_8 (Value'Image).Delete (1, 1);
   end To_UXString;

   type User_Info is tagged record
      Email, Surname, Name, Phone, Date, City : UXString := "";
   end record;

   package User_Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => UXString, Element_Type => User_Info, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   User_Dict : User_Dictionary.Map;

   procedure Set_Info
     (Email : UXString;
      Identity : User_Info)
   is
   begin
      if User_Dict.Contains (Email) then
         User_Dict.Replace (Email, Identity);
      else
         User_Dict.Insert (Email, Identity);
      end if;
   end Set_Info;

   function Get_Info (Email : UXString) return User_Info is
   begin
      return User_Dict.Element (Email);
   end Get_Info;

   -----------------------------------------------------------------------------
   --  CRUD Handlers
   -----------------------------------------------------------------------------

   --------------------
   --  Default CRUD  --
   --------------------
   procedure On_CRUD_File_Create (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Create");
      Gnoga.Log ("Créer");
   end On_CRUD_File_Create;

   procedure On_CRUD_File_Edit (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Edit");
      Gnoga.Log ("Modifier");
   end On_CRUD_File_Edit;

   procedure On_CRUD_File_Delete (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Delete");
      Gnoga.Log ("Supprimer");
   end On_CRUD_File_Delete;

   procedure On_CRUD_File_Export (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Export");
      Gnoga.Log ("Exporter");
   end On_CRUD_File_Export;

   procedure On_CRUD_File_Import (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Import");
      Gnoga.Log ("Importer");
   end On_CRUD_File_Import;

   procedure On_CRUD_File_Print (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Print");
      Gnoga.Log ("Imprimer");
   end On_CRUD_File_Print;

   procedure On_CRUD_Edit_Copy (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Edit_Copy");
      Gnoga.Log ("Copier");
   end On_CRUD_Edit_Copy;

   procedure On_CRUD_Edit_Paste (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Edit_Paste");
      Gnoga.Log ("Coller");
   end On_CRUD_Edit_Paste;

   procedure On_CRUD_Show_Previous (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Show_Previous");
      Gnoga.Log ("Précédent");
   end On_CRUD_Show_Previous;

   procedure On_CRUD_Show_Next (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Show_Next");
      Gnoga.Log ("Suivant");
   end On_CRUD_Show_Next;

   procedure On_CRUD_Show_Search (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Show_Search");
      Gnoga.Log ("Rechercher");
   end On_CRUD_Show_Search;

   procedure Load_Default_CRUD_Roots (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Add_Element (Object, "File", "Fichier", "/css/icons/file.png");
      v22.Gui.CRUD_Add_Element (Object, "Edit", "Éditer", "/css/icons/edit.png");
      v22.Gui.CRUD_Add_Element (Object, "Show", "Afficher", "/css/icons/browse.png");
   end Load_Default_CRUD_Roots;

   procedure Load_Default_CRUD_Childs (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Create", "Créer", "File", On_CRUD_File_Create'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Edit", "Modifier", "File", On_CRUD_File_Edit'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Delete", "Supprimer", "File", On_CRUD_File_Delete'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Export", "Exporter", "File", On_CRUD_File_Export'Unrestricted_Access);
      v22.Gui.CRUD_Add_Delimiter_Above (Object, "File_Export");
      v22.Gui.CRUD_Set_Unclickable (Object, "File_Export");
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Import", "Importer", "File", On_CRUD_File_Import'Unrestricted_Access);
      v22.Gui.CRUD_Set_Unclickable (Object, "File_Import");
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Print", "Imprimer", "File", On_CRUD_File_Print'Unrestricted_Access);
      v22.Gui.CRUD_Add_Delimiter_Above (Object, "File_Print");

      v22.Gui.CRUD_Add_Sub_Element (Object, "Edit_Copy", "Copier", "Edit", On_CRUD_Edit_Copy'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "Edit_Paste", "Coller", "Edit", On_CRUD_Edit_Paste'Unrestricted_Access);

      v22.Gui.CRUD_Add_Sub_Element
        (Object, "Show_Previous", "Précédent", "Show", On_CRUD_Show_Previous'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "Show_Next", "Suivant", "Show", On_CRUD_Show_Next'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "Show_Search", "Rechercher", "Show", On_CRUD_Show_Search'Unrestricted_Access);
   end Load_Default_CRUD_Childs;

   ---------------------
   --  Extended CRUD  --
   ---------------------
   procedure On_CRUD_Show_List (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Show_List");
      Gnoga.Log ("Lister");
   end On_CRUD_Show_List;

   procedure On_CRUD_Show_List_Bill (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Show_List_Bill");
      Gnoga.Log ("Lister Factures");
   end On_CRUD_Show_List_Bill;

   procedure On_CRUD_Show_List_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Show_List_SEPA");
      Gnoga.Log ("Lister SEPA");
   end On_CRUD_Show_List_SEPA;

   procedure On_CRUD_Validate_Bill (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Validate_Bill");
      Gnoga.Log ("Factures");
   end On_CRUD_Validate_Bill;

   procedure On_CRUD_Validate_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Validate_SEPA");
      Gnoga.Log ("SEPA");
   end On_CRUD_Validate_SEPA;

   procedure On_CRUD_Preferences_SEPA (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Preferences_SEPA");
      Gnoga.Log ("Intervalles SEPA");
   end On_CRUD_Preferences_SEPA;

   procedure On_CRUD_Preferences_Service (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Preferences_Service");
      Gnoga.Log ("Type de Prestation");
   end On_CRUD_Preferences_Service;

   procedure On_CRUD_Security_Bug (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "Security_Bug");
      Gnoga.Log ("Should not appear...");
   end On_CRUD_Security_Bug;

   -----------------------------------------------------------------------------
   --  Extended CRUD with different behaviour
   -----------------------------------------------------------------------------
   procedure Update_TTC (Object : in out Base.Base_Type'Class) is
      Price  : constant Integer := v22.Gui.Content_Group_Number_Get (Object, "Prix HT");
      TVA    : constant Integer := v22.Gui.Content_Group_Number_Get (Object, "TVA");
      Result : Integer;
   begin
      Result := Price + TVA;
      v22.Gui.Content_Group_Text_Set (Object, "TTC", From_UTF_8 (Result'Image) & " €");
   end Update_TTC;

   procedure On_CRUD_Contract_File_Create (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Create");
      v22.Gui.Content_Set_Title (Object, "Statistiques - Fichier");
      v22.Gui.Content_Clear_Text (Object);

      v22.Gui.Content_Group_Create (Object, "Spécification du contrat");
      v22.Gui.Content_Group_Add_Title (Object, "Détails du contrat", "Spécification du contrat");
      v22.Gui.Content_Group_Selection_Add (Object, "Type de contrat", "Spécification du contrat");
      v22.Gui.Content_Group_Selection_Add_Option (Object, "Type de contrat", "Standard", Enabled => True);
      v22.Gui.Content_Group_Selection_Add_Option (Object, "Type de contrat", "Modifié");
      v22.Gui.Content_Group_Selection_Add_Option (Object, "Type de contrat", "Complexe");
      v22.Gui.Content_Group_Date_Add (Object, "Date de création", "Spécification du contrat");
      v22.Gui.Content_Group_Number_Add (Object, "Récurrence", "Spécification du contrat");
      v22.Gui.Content_Group_Add_Title (Object, "Activité", "Spécification du contrat");
      v22.Gui.Content_Group_Number_Add (Object, "Durée d'engagement en jours", "Spécification du contrat");
      v22.Gui.Content_Group_Number_Set (Object, "Durée d'engagement en jours", 30);
      v22.Gui.Content_Group_Check_Box_Add (Object, "Contrat actif", "Spécification du contrat");
      v22.Gui.Content_Group_Check_Box_Checked (Object, "Contrat actif", True);
      v22.Gui.Content_Group_Number_Add (Object, "Durée du contrat en jours", "Spécification du contrat");
      v22.Gui.Content_Group_Number_Set (Object, "Durée du contrat en jours", 30);
      v22.Gui.Content_Group_Item_Lock (Object, "Durée du contrat en jours");

      v22.Gui.Content_Group_Create (Object, "Prix");
      v22.Gui.Content_Group_Number_Add (Object, "Prix HT", "Prix", Update_TTC'Unrestricted_Access);
      v22.Gui.Content_Group_Number_Set (Object, "Prix HT", 0);
      v22.Gui.Content_Group_Number_Add (Object, "TVA", "Prix", Update_TTC'Unrestricted_Access);
      v22.Gui.Content_Group_Number_Set (Object, "TVA", 0);
      v22.Gui.Content_Group_Text_Add (Object, "TTC", "Prix");

      v22.Gui.Content_Group_Create (Object, "Agenda");
      v22.Gui.Content_Group_Date_Add (Object, "Prochaine date de facturation", "Agenda");
      v22.Gui.Content_Group_Date_Add (Object, "Prochaine date d'échéance", "Agenda");

      v22.Gui.Content_Group_Create (Object, "Autres informations");
      v22.Gui.Content_Group_Text_Area_Add (Object, "Note publique", "Autres informations");
      v22.Gui.Content_Group_Text_Area_Add (Object, "Note privée", "Autres informations");

      v22.Gui.Content_Group_Item_Lock (Object, "Prochaine date d'échéance");
      v22.Gui.Content_Group_Item_Lock (Object, "Note privée");
      v22.Gui.Content_Group_Item_Lock (Object, "TTC");
      v22.Gui.Content_Group_Text_Set (Object, "TTC", "Remplissez les données précédentes");
      v22.Gui.Content_Group_Text_Set (Object, "TTC", "0 €");

      v22.Gui.CRUD_Set_Unclickable (Object, "File_Create");
      v22.Gui.CRUD_Set_Clickable (Object, "File_Edit");
      v22.Gui.CRUD_Set_Clickable (Object, "File_Delete");
   end On_CRUD_Contract_File_Create;

   procedure On_CRUD_Contract_File_Edit (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Edit");
   end On_CRUD_Contract_File_Edit;

   procedure On_CRUD_Contract_File_Delete (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Delete");

      v22.Gui.Content_Set_Title (Object, "Statistiques");
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);

      v22.Gui.CRUD_Set_Clickable (Object, "File_Create");
      v22.Gui.CRUD_Set_Unclickable (Object, "File_Edit");
      v22.Gui.CRUD_Set_Unclickable (Object, "File_Delete");
      v22.Gui.CRUD_Set_Unclickable (Object, "File_Print");
   end On_CRUD_Contract_File_Delete;

   -----------------------------------------------------------------------------
   --  CRUD for Administration > Users
   -----------------------------------------------------------------------------
   procedure On_CRUD_User_File_Edit (Object : in out Base.Base_Type'Class) is
      Parent_Key : constant UXString := "Liste des utilisateurs";
      Sub_Key    : constant UXString := "Détails de l'utilisateur";
      Data_Index : constant Integer  := v22.Gui.Content_List_Selected_Row (Object, Parent_Key);
      Identity   : User_Info;
      Index      : Integer           := 0;
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Edit");
      if Data_Index /= 0 then
         for Data of User_Dict loop
            Index := Index + 1;
            if Index = Data_Index then
               Identity := Data;
            end if;
         end loop;

         v22.Gui.Set_Data (Object, "Index", To_UXString (Data_Index));

         v22.Gui.CRUD_Set_Unclickable (Object, "File_Delete");
         v22.Gui.CRUD_Set_Unclickable (Object, "File_Edit");
         v22.Gui.CRUD_Set_Clickable (Object, "File_Save");

         v22.Gui.Content_Set_Title (Object, "Utilisateur - Modification");
         v22.Gui.Content_Clear_Text (Object);

         v22.Gui.Content_Group_Create (Object, Sub_Key);
         v22.Gui.Content_Group_Email_Add (Object, "Adresse mail", Sub_Key);
         v22.Gui.Content_Group_Text_Add (Object, "Nom", Sub_Key);
         v22.Gui.Content_Group_Text_Add (Object, "Prénom", Sub_Key);
         v22.Gui.Content_Group_Phone_Add (Object, "Numéro de téléphone", Sub_Key);
         v22.Gui.Content_Group_Date_Add (Object, "Date de naissance", Sub_Key);
         v22.Gui.Content_Group_Text_Add (Object, "Ville", Sub_Key);

         v22.Gui.Content_Group_Email_Set (Object, "Adresse mail", Identity.Email);

         v22.Gui.Content_Group_Item_Lock (Object, "Adresse mail");

         v22.Gui.Content_Group_Text_Set (Object, "Nom", Identity.Surname);
         v22.Gui.Content_Group_Text_Set (Object, "Prénom", Identity.Name);
         v22.Gui.Content_Group_Phone_Set (Object, "Numéro de téléphone", Identity.Phone);
         v22.Gui.Content_Group_Date_Set (Object, "Date de naissance", Identity.Date);
         v22.Gui.Content_Group_Text_Set (Object, "Ville", Identity.City);
      end if;
   end On_CRUD_User_File_Edit;

   procedure On_Administration_Users (Object : in out Base.Base_Type'Class);

   procedure On_CRUD_User_File_Save (Object : in out Base.Base_Type'Class) is
      Data_Index : constant Integer := Int_Value (v22.Gui.Get_Data (Object, "Index"));
      Identity   : User_Info;
      Index      : Integer          := 0;
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Save");
      for Data of User_Dict loop
         Index := Index + 1;
         if Index = Data_Index then
            Identity := Data;
         end if;
      end loop;

      Identity.Surname := v22.Gui.Content_Group_Text_Get (Object, "Nom");
      Identity.Name := v22.Gui.Content_Group_Text_Get (Object, "Prénom");
      Identity.Phone := v22.Gui.Content_Group_Phone_Get (Object, "Numéro de téléphone");
      Identity.Date := v22.Gui.Content_Group_Date_Get (Object, "Date de naissance");
      Identity.City := v22.Gui.Content_Group_Text_Get (Object, "Ville");

      Set_Info (Identity.Email, Identity);

      On_Administration_Users (Object);
   end On_CRUD_User_File_Save;

   procedure On_CRUD_User_File_Delete (Object : in out Base.Base_Type'Class) is
      Parent_Key : constant UXString := "Liste des utilisateurs";
      Data_Index : constant Integer  := v22.Gui.Content_List_Selected_Row (Object, Parent_Key);
      Identity   : User_Info;
      Index      : Integer           := 0;
   begin
      v22.Gui.CRUD_Notify_Sub_Element_Click (Object, "File_Delete");
      for Data of User_Dict loop
         Index := Index + 1;
         if Index = Data_Index then
            Identity := Data;
         end if;
      end loop;

      if Data_Index /= 0 then
         v22.Gui.Delete_User (Identity.Email);
         User_Dict.Delete (Identity.Email);
      end if;

      On_Administration_Users (Object);
   end On_CRUD_User_File_Delete;

   -----------------------------------------------------------------------------
   --  Navigation Handlers
   -----------------------------------------------------------------------------
   procedure On_Contract (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "Contract");

      v22.Gui.Content_Set_Title (Object, "Contrats");
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Contract;

   procedure On_Contract_Stats (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "Contract_Stats");

      v22.Gui.Content_Set_Title (Object, "Statistiques");
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);
      v22.Gui.CRUD_Add_Element (Object, "File", "Fichier", "/css/icons/file.png");
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Create", "Créer", "File", On_CRUD_Contract_File_Create'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Edit", "Modifier", "File", On_CRUD_Contract_File_Edit'Unrestricted_Access);
      v22.Gui.CRUD_Set_Unclickable (Object, "File_Edit");
      v22.Gui.CRUD_Add_Sub_Element
        (Object, "File_Delete", "Supprimer", "File", On_CRUD_Contract_File_Delete'Unrestricted_Access);
      v22.Gui.CRUD_Set_Unclickable (Object, "File_Delete");

      v22.Gui.CRUD_Load (Object);
   end On_Contract_Stats;

   procedure On_Contract_Management (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "Contract_Management");

      v22.Gui.Content_Set_Title (Object, "Gestion");
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Contract_Management;

   procedure On_Administration_Users (Object : in out Base.Base_Type'Class) is
      Parent_Key : constant UXString := "Liste des utilisateurs";
      Dummy      : Integer           := 0;
      Index      : Integer           := 0;
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "Administration_Users");
      v22.Gui.Content_Set_Title (Object, "Utilisateurs");

      v22.Gui.CRUD_Add_Element (Object, "File", "Fichier", "/css/icons/file.png");
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Edit", "Modifier", "File", On_CRUD_User_File_Edit'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "File_Save", "Sauvegarder", "File", On_CRUD_User_File_Save'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element
        (Object, "File_Delete", "Supprimer", "File", On_CRUD_User_File_Delete'Unrestricted_Access);

      v22.Gui.CRUD_Set_Unclickable (Object, "File_Save");
      v22.Gui.CRUD_Load (Object);

      v22.Gui.Content_List_Create (Object, Parent_Key);
      v22.Gui.Content_List_Add_Column (Object, "ID", Parent_Key);
      v22.Gui.Content_List_Add_Column (Object, "Email", Parent_Key);

      v22.Gui.Content_List_Add_Column (Object, "Nom", Parent_Key);
      v22.Gui.Content_List_Add_Column (Object, "Prénom", Parent_Key);
      v22.Gui.Content_List_Add_Column (Object, "Numéro de téléphone", Parent_Key);
      v22.Gui.Content_List_Add_Column (Object, "Date de naissance", Parent_Key);
      v22.Gui.Content_List_Add_Column (Object, "Ville", Parent_Key);

      for Data of User_Dict loop
         Index := Index + 1;
         Dummy := v22.Gui.Content_List_Add_Item (Object, Parent_Key);
         v22.Gui.Content_List_Add_Text (Object, From_UTF_8 (Index'Image), Dummy, Parent_Key);
         v22.Gui.Content_List_Add_Text (Object, Data.Email, Dummy, Parent_Key);

         v22.Gui.Content_List_Add_Text (Object, Data.Surname, Dummy, Parent_Key);
         v22.Gui.Content_List_Add_Text (Object, Data.Name, Dummy, Parent_Key);
         v22.Gui.Content_List_Add_Text (Object, Data.Phone, Dummy, Parent_Key);
         v22.Gui.Content_List_Add_Text (Object, Data.Date, Dummy, Parent_Key);
         v22.Gui.Content_List_Add_Text (Object, Data.City, Dummy, Parent_Key);
      end loop;
   end On_Administration_Users;

   procedure On_Administration_Emails (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "Administration_Emails");

      v22.Gui.Content_Set_Title (Object, "Emails");
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration_Emails;

   procedure On_Administration_Gen (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "Administration_Gen");

      Load_Default_CRUD_Roots (Object);
      v22.Gui.CRUD_Add_Element (Object, "Validate", "Valider", "/css/icons/checklist.png");
      v22.Gui.CRUD_Add_Element (Object, "Preferences", "Préférences", "/css/icons/settings.png");
      v22.Gui.CRUD_Add_Element (Object, "Security", "Sécurité", "/css/icons/security.png");
      v22.Gui.CRUD_Set_Unclickable (Object, "Security");

      Load_Default_CRUD_Childs (Object);

      v22.Gui.CRUD_Add_Sub_Element (Object, "Show_List", "Lister", "Show", On_CRUD_Show_List'Unrestricted_Access);
      v22.Gui.CRUD_Add_Delimiter_Above (Object, "Show_List");
      v22.Gui.CRUD_Add_Sub_Element
        (Object, "Show_List_Bill", "Lister Factures", "Show", On_CRUD_Show_List_Bill'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element
        (Object, "Show_List_SEPA", "Lister SEPA", "Show", On_CRUD_Show_List_SEPA'Unrestricted_Access);

      v22.Gui.CRUD_Add_Sub_Element
        (Object, "Validate_Bill", "Factures", "Validate", On_CRUD_Validate_Bill'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element (Object, "Validate_SEPA", "SEPA", "Validate", On_CRUD_Validate_SEPA'Unrestricted_Access);

      v22.Gui.CRUD_Add_Sub_Element
        (Object, "Preferences_SEPA", "Intervalles SEPA", "Preferences", On_CRUD_Preferences_SEPA'Unrestricted_Access);
      v22.Gui.CRUD_Add_Sub_Element
        (Object, "Preferences_Service", "Type de Prestation", "Preferences",
         On_CRUD_Preferences_Service'Unrestricted_Access);

      v22.Gui.CRUD_Add_Sub_Element
        (Object, "Security_Bug", "Ne devrait pas être affiché...", "Security",
         On_CRUD_Security_Bug'Unrestricted_Access);

      v22.Gui.CRUD_Load (Object);

      v22.Gui.Content_Set_Title (Object, "Générer des requêtes");
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration_Gen;

   procedure On_Administration (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "Administration");

      v22.Gui.Content_Set_Title (Object, "Administration");
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);
   end On_Administration;

   procedure On_App_Menu (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_Menu_Click (Object, "App_Menu");

      Load_Default_CRUD_Roots (Object);
      Load_Default_CRUD_Childs (Object);
      v22.Gui.CRUD_Load (Object);

      v22.Gui.Content_Set_Title (Object, App_Name);
      v22.Gui.Content_Set_Text (Object, Lorem_Ipsum);
   end On_App_Menu;

   procedure On_Dialog_Confirm (Object : in out Base.Base_Type'Class) is
   begin
      Gnoga.Log ("Dialog: confirmed");
      v22.Gui.Close_Dialog (Object);
   end On_Dialog_Confirm;

   procedure On_Dialog_Cancel (Object : in out Base.Base_Type'Class) is
   begin
      Gnoga.Log ("Dialog: cancelled");
      v22.Gui.Close_Dialog (Object);
   end On_Dialog_Cancel;

   -----------------------------------------------------------------------------
   --  On_Connect
   -----------------------------------------------------------------------------
   procedure On_User_Help (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_User_Menu_Click (Object);
      v22.Gui.Launch_Web (Object, "https://google.com");
   end On_User_Help;

   procedure On_User_Rights (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_User_Menu_Click (Object);
      v22.Gui.Launch_Dialog
        (Object, "Droits d'accès", "Ajouter les droits d'accès ici", "Confirmer", "Annuler",
         On_Dialog_Confirm'Unrestricted_Access, On_Dialog_Cancel'Unrestricted_Access);
   end On_User_Rights;

   procedure On_User_Connection_Duration (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_User_Menu_Click (Object);
      v22.Gui.Launch_Dialog (Object, "Connecté depuis...", "Ajouter durée de la connexion ici");
   end On_User_Connection_Duration;

   procedure On_User_Last_Connection (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_User_Menu_Click (Object);
      v22.Gui.Launch_Dialog (Object, "Connexion précédente", "Ajouter la date de la dernière connexion ici");
   end On_User_Last_Connection;

   procedure On_User_About (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Header_Notify_User_Menu_Click (Object);
      v22.Gui.Launch_Web (Object, "http://gnoga.com");
   end On_User_About;

   procedure On_Connect (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Set_User_Name (Object, "Connectez-vous !");
      v22.Gui.Set_User_Icon (Object, "/css/icons/user.png");

      v22.Gui.Footer_Set_State_Text (Object, "Message de statut");
      v22.Gui.Footer_Set_Permanent_Text (Object, "Informations permanentes");

      v22.Gui.Header_Add_User_Button (Object, "Aide en ligne", On_User_Help'Unrestricted_Access);
      v22.Gui.Header_Add_User_Button (Object, "Droits d'accès", On_User_Rights'Unrestricted_Access);
      v22.Gui.Header_Add_User_Button (Object, "Connecté depuis...", On_User_Connection_Duration'Unrestricted_Access);
      v22.Gui.Header_Add_User_Button (Object, "Connexion précédente", On_User_Last_Connection'Unrestricted_Access);
      v22.Gui.Header_Add_User_Button (Object, "À propos de...", On_User_About'Unrestricted_Access);
      v22.Gui.Header_Add_User_Button (Object, "Se déconnecter", v22.Gui.Disconnect_User'Unrestricted_Access);
   end On_Connect;

   procedure On_Login (Object : in out Base.Base_Type'Class) is
      Identity : constant User_Info := Get_Info (v22.Gui.Get_User_Email (Object));
   begin
      v22.Gui.Set_User_Name (Object, Identity.Surname & " " & Identity.Name);
   end On_Login;

   procedure On_Register_Create (Object : in out Base.Base_Type'Class) is
   begin
      v22.Gui.Content_Group_Add_Title (Object, "Données utilisateur", v22.Gui.Register_Group_Key);
      v22.Gui.Content_Group_Text_Add (Object, "Nom", v22.Gui.Register_Group_Key);
      v22.Gui.Content_Group_Text_Add (Object, "Prénom", v22.Gui.Register_Group_Key);
      v22.Gui.Content_Group_Phone_Add (Object, "Numéro de téléphone", v22.Gui.Register_Group_Key);
      v22.Gui.Content_Group_Date_Add (Object, "Date de naissance", v22.Gui.Register_Group_Key);
      v22.Gui.Content_Group_Text_Add (Object, "Ville", v22.Gui.Register_Group_Key);
   end On_Register_Create;

   function On_Register
     (Object   : in out Base.Base_Type'Class;
      Email : UXString)
      return Boolean
   is
      Surname : constant UXString := v22.Gui.Content_Group_Text_Get (Object, "Nom");
      Name    : constant UXString := v22.Gui.Content_Group_Text_Get (Object, "Prénom");
      Phone   : constant UXString := v22.Gui.Content_Group_Phone_Get (Object, "Numéro de téléphone");
      Date    : constant UXString := v22.Gui.Content_Group_Date_Get (Object, "Date de naissance");
      City    : constant UXString := v22.Gui.Content_Group_Text_Get (Object, "Ville");
      User_Identity : User_Info;
   begin
      User_Identity.Email := Email;
      User_Identity.Surname := Surname;
      User_Identity.Name := Name;
      User_Identity.Phone := Phone;
      User_Identity.Date := Date;
      User_Identity.City := City;

      if Surname = "" then
         v22.Gui.Set_Register_Error_Message (Object, "Entrez votre nom");
      else
         if Name = "" then
            v22.Gui.Set_Register_Error_Message (Object, "Entrez votre prénom");
         else
            if Phone = "" then
               v22.Gui.Set_Register_Error_Message (Object, "Entrez votre numéro de téléphone");
            else
               if Date = "" then
                  v22.Gui.Set_Register_Error_Message (Object, "Entrez votre date de naissance");
               else
                  if City = "" then
                     v22.Gui.Set_Register_Error_Message (Object, "Entrez votre ville");
                  else
                     Set_Info (Email, User_Identity);
                     Gnoga.Log ("Created an account for " & Email);
                     return True;
                  end if;
               end if;
            end if;
         end if;
      end if;
      return False;
   end On_Register;

   Root_Identity : User_Info;
   Root_Password : constant UXString := "password";

begin
   v22.Gui.Setup (On_Connect'Unrestricted_Access, App_Name, "<h1>Server closed</h1>");
   v22.Gui.Setup_Access
     (On_Login'Unrestricted_Access, On_Register_Create'Unrestricted_Access, On_Register'Unrestricted_Access);

   Root_Identity.Email := "root@root";
   Root_Identity.Surname := "Utilisateur";
   Root_Identity.Name := "Root";
   Root_Identity.Phone := "0000000000";
   Root_Identity.Date := "0";
   Root_Identity.City := "-";
   v22.Gui.Add_User (Root_Identity.Email, Root_Password);
   Set_Info (Root_Identity.Email, Root_Identity);

   v22.Gui.Set_Navigation_Icon ("/css/icons/widget.png");
   v22.Gui.Set_Default_User_Icon ("/css/icons/user.png");

   v22.Gui.Header_Set_Root ("App_Menu", App_Name, On_App_Menu'Unrestricted_Access);

   v22.Gui.Header_Add_Child ("Contract", "Contrats", "App_Menu", On_Contract'Unrestricted_Access);
   v22.Gui.Header_Add_Child ("Contract_Management", "Gestion", "Contract", On_Contract_Management'Unrestricted_Access);
   v22.Gui.Header_Add_Child ("Contract_Stats", "Statistiques", "Contract", On_Contract_Stats'Unrestricted_Access);

   v22.Gui.Header_Add_Child ("Administration", "Administration", "App_Menu", On_Administration'Unrestricted_Access);
   v22.Gui.Header_Add_Child
     ("Administration_Users", "Utilisateurs", "Administration", On_Administration_Users'Unrestricted_Access);
   v22.Gui.Header_Add_Child
     ("Administration_Emails", "Emails", "Administration", On_Administration_Emails'Unrestricted_Access);
   v22.Gui.Header_Add_Child
     ("Administration_Gen", "Gén. requêtes", "Administration", On_Administration_Gen'Unrestricted_Access);

   Gnoga.Application.Multi_Connect.Message_Loop;
exception
   when E : others =>
      Gnoga.Log (E);
end Application;
