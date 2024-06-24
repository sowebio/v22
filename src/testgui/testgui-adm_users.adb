-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testgui-adm.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - GUI test program
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with GNAT.SHA512;

separate (TestGui) package body Adm_Users is

   ----------------------------------------------------------------------------
   --  Declarations
   ----------------------------------------------------------------------------

   procedure On_Create (Object : in out GGB.Base_Type'Class);
   procedure On_Delete (Object : in out GGB.Base_Type'Class);
   procedure On_Down (Object : in out GGB.Base_Type'Class);
   procedure On_Read (Object : in out GGB.Base_Type'Class);
   procedure On_Print (Object : in out GGB.Base_Type'Class);
   procedure On_Search (Object : in out GGB.Base_Type'Class);
   procedure On_Up (Object : in out GGB.Base_Type'Class);
   procedure On_Update (Object : in out GGB.Base_Type'Class);

   procedure On_Validate_Create (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Validate_Search (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Validate_Update (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure On_Validate_Delete (Object : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Put_Error_Message (Object : in out Gnoga.Gui.Base.Base_Type'Class; Error : String);
   procedure Put_Form (Object : in out Gnoga.Gui.Base.Base_Type'Class; Mode : Db_Mode := None; Key : String := "");
   procedure Put_List (Object : in out GGB.Base_Type'Class; Condition : String := "ORDER BY " & Users_Key_Main & " LIMIT " &
                      To_String_Unsigned (Users_List_Length));

   ----------------------------------------------------------------------------
   --  Routines
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Main_Menu (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Users > Main_Menu");

      Gui.Main_Menu_Clear (Object);
      Gui.Main_Menu_Add_Element (Object, "Up", "Précédent", "ico-db_list_up.png", On_Up'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Down", "Suivant", "ico-db_list_down.png", On_Down'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Search", "Rechercher", "ico-db_search.png", On_Search'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "View", "Voir", "ico-db_read.png", On_Read'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Create", "Créer", "ico-db_create.png", On_Create'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Edit", "Modifier", "ico-db_update.png", On_Update'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Delete", "Supprimer", "ico-db_delete.png", On_Delete'Unrestricted_Access);
      Gui.Main_Menu_Add_Element (Object, "Print", "Impression", "ico-print.png", On_Print'Unrestricted_Access);
      Gui.Main_Menu_Load (Object);

      Put_List (Object);
   end Main_Menu;

   ----------------------------------------------------------------------------
   procedure On_Cancel (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Data_Value : String := Gui.Get_Connection_Data (Object, Users_List_Key & "_Select");
   begin
      Msg.Debug ("Users > Cancel_Read with Value: " & Data_Value);
      Put_List (Object,"WHERE Login >= '" & Data_Value & "' ORDER BY " &
                Users_Key_Main & " LIMIT " & To_String_Unsigned (Users_List_Length));
   end On_Cancel;

   ----------------------------------------------------------------------------
   procedure On_Create (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Users > Create");
      Put_Form (Object, Create);
   end On_Create;

   ----------------------------------------------------------------------------
   procedure On_Delete (Object : in out GGB.Base_Type'Class) is
      Data_Index : constant Integer := Gui.Content_List_Selected_Row (Object, Users_List_Key);
      Data_Value : String := Gui.Get_Connection_Data (Object, Users_List_Key & "_" & To_String_Unsigned (Data_Index));
   begin
      Msg.Debug ("Users > Delete with Key/Value: " & Users_List_Key & "_" & To_String_Unsigned (Data_Index) & "/" & Data_Value);
      Put_Form (Object, Delete, Data_Value);
   end On_Delete;

   ----------------------------------------------------------------------------
   procedure On_Down (Object : in out GGB.Base_Type'Class) is
      Last_Line_Value : String := Gui.Get_Connection_Data (Object, Users_List_Key & "_Last");
      Query_Result : String := "";
      Query_Result_Count : Natural;
   begin
      Msg.Debug ("Users > Down");
      Query_Result := Sql.Read (DB, "Sys_Users", Users_Key_Main, "WHERE " & Users_Key_Main & " >= '" & Last_Line_Value &
                                "' ORDER BY " & Users_Key_Main & " LIMIT " & To_String_Unsigned (Users_List_Length));
      Query_Result_Count := Field_Count (Query_Result, RD);
      --  Crop to LIMIT value to display a complete list
      if Query_Result_Count < Users_List_Length then
         Query_Result := Sql.Read (DB, "Sys_Users", Users_Key_Main, "WHERE " & Users_Key_Main & " <= '" &
                                   Field_By_Index (Query_Result, Query_Result_Count, RD) & "' ORDER BY " &
                                   Users_Key_Main & " DESC LIMIT " & To_String_Unsigned (Users_List_Length));
         Query_Result_Count := Field_Count (Query_Result, RD);
         Last_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
      end if;
      Put_List (Object, "WHERE " & Users_Key_Main & " >= '" & Last_Line_Value & "' ORDER BY " &
                        Users_Key_Main & " LIMIT " & To_String_Unsigned (Users_List_Length));
   end On_Down;

   ----------------------------------------------------------------------------
   procedure On_Print (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Users > Print");
   end On_Print;

   ----------------------------------------------------------------------------
   procedure On_Read (Object : in out GGB.Base_Type'Class) is
      Data_Index : constant Integer := Gui.Content_List_Selected_Row (Object, Users_List_Key);
      Data_Value : String := Gui.Get_Connection_Data (Object, Users_List_Key & "_" & To_String_Unsigned (Data_Index));
   begin
      Gui.Set_Connection_Data (Object, Users_List_Key & "_Select", Data_Value);
      Msg.Debug ("Users > Edit with Key/Value: " & Users_List_Key & "_" & To_String_Unsigned (Data_Index) & "/" & Data_Value);
      Put_Form (Object, Read, Data_Value);
   end On_Read;

   ----------------------------------------------------------------------------
   procedure On_Search (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Users > Search");
      Put_Form (Object, Search);
   end On_Search;

   ----------------------------------------------------------------------------
   procedure On_Up (Object : in out GGB.Base_Type'Class) is
      First_Line_Value : String := Gui.Get_Connection_Data (Object, Users_List_Key & "_First");
      Query_Result : String := "";
      Query_Result_Count : Natural;
   begin
      Msg.Debug ("Users > Up");
      Query_Result := Sql.Read (DB, "Sys_Users", Users_Key_Main, "WHERE " & Users_Key_Main & " <= '" & First_Line_Value &
                                     "' ORDER BY " & Users_Key_Main & " DESC LIMIT " & To_String_Unsigned (Users_List_Length));
      Query_Result_Count := Field_Count (Query_Result, RD);
      First_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
      --  Crop to LIMIT value to display a complete list
      if Query_Result_Count < Users_List_Length then
         Query_Result_Count := Field_Count (Query_Result, RD);
         First_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
      end if;
      Put_List (Object, "WHERE " & Users_Key_Main & " >= '" & First_Line_Value & "' ORDER BY " &
                        Users_Key_Main & " LIMIT " & To_String_Unsigned (Users_List_Length));
   end On_Up;

   ----------------------------------------------------------------------------
   procedure On_Update (Object : in out GGB.Base_Type'Class) is
      Data_Index : constant Integer := Gui.Content_List_Selected_Row (Object, Users_List_Key);
      Data_Value : String := Gui.Get_Connection_Data (Object, Users_List_Key & "_" & To_String_Unsigned (Data_Index));
   begin
      Gui.Set_Connection_Data (Object, Users_List_Key & "_Select", Data_Value);
      Msg.Debug ("Users > Edit with Key/Value: " & Users_List_Key & "_" & To_String_Unsigned (Data_Index) & "/" & Data_Value);
      Put_Form (Object, Update, Data_Value);
   end On_Update;

   ----------------------------------------------------------------------------
   procedure On_Validate_Create (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Login : constant String := Gui.Content_Group_Text_Get (Object, "Identifiant");
      First_Name : constant String := Gui.Content_Group_Text_Get (Object, "Prénom");
      Last_Name : constant String := Gui.Content_Group_Text_Get (Object, "Nom");
      Phone : constant String := Gui.Content_Group_Phone_Get (Object, "Téléphone");
      Email : constant String := Gui.Content_Group_Email_Get (Object, "Email");
      Password_1 : constant String := Gui.Content_Group_Password_Get (Object, "Mot de passe");
      Password_2 : constant String := Gui.Content_Group_Password_Get (Object, "Mot de passe (confirmation)");
      Password : constant String := From_UTF_8 (GNAT.SHA512.Digest (To_UTF_8 (Password_1)));
      Query : String := "";
   begin
      Msg.Debug ("Users > Validate_Create");
      if not Is_Empty (Login) then
         if not Sql.Search (DB, "Sys_Users", Users_Key_Main & " = '" &  Login & "'") then
            if not Is_Empty (Password_1) then
               if Password_1 = Password_2 then
                  Query := "Login" & ND & Login & CD &
                           "First_Name" & ND & First_Name & CD &
                           "Last_Name" & ND & Last_Name & CD &
                           "Phone" & ND & Phone & CD &
                           "Email" & ND & Email & CD &
                           "Password" & ND & Password & CD &
                           "Created_On" & ND & Prg.Date_Time_Stamp;
                  Msg.Debug ("Query: " & Query);
                  Sql.Insert (DB, "Sys_Users", Query);
                  Put_List (Object);
               else
                  Put_Error_Message (Object, "Les mots de passe ne correspondent pas");
               end if;
            else
               Put_Error_Message (Object, "Le mot de passe est manquant");
            end if;
         else
            Put_Error_Message (Object, "L'identifiant existe déjà");
         end if;
      else
         Put_Error_Message (Object, "L'identifiant est manquant");
      end if;
   end On_Validate_Create;

   ----------------------------------------------------------------------------
   procedure On_Validate_Delete (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Login : constant String := Gui.Content_Group_Text_Get (Object, "Identifiant");
      Query : String := "";
   begin
      Msg.Debug ("Users > Validate_Delete");
      Sql.Delete (DB, "Sys_Users", Users_Key_Main & " = '" & Login & "'");
      Put_List (Object);
   end On_Validate_Delete;

   ----------------------------------------------------------------------------
   procedure On_Validate_Search (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Login : constant String := Gui.Content_Group_Text_Get (Object, "Identifiant");
      Query : String := "";
   begin
      Msg.Debug ("Users > Validate_Search");
      if not Is_Empty (Login) then
         if Sql.Search (DB, "Sys_Users", Users_Key_Main & " = '" &  Login & "'") then
            Put_Form (Object, Read, Login);
         else
            Put_Error_Message (Object, "Identifiant non trouvé");
         end if;
      end if;
   end On_Validate_Search;

   ----------------------------------------------------------------------------
   procedure On_Validate_Update (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Login : constant String := Gui.Content_Group_Text_Get (Object, "Identifiant");
      First_Name : constant String := Gui.Content_Group_Text_Get (Object, "Prénom");
      Last_Name : constant String := Gui.Content_Group_Text_Get (Object, "Nom");
      Phone : constant String := Gui.Content_Group_Phone_Get (Object, "Téléphone");
      Email : constant String := Gui.Content_Group_Email_Get (Object, "Email");
      Password_1 : String := Gui.Content_Group_Password_Get (Object, "Mot de passe");
      Password_2 : String := Gui.Content_Group_Password_Get (Object, "Mot de passe (confirmation)");
      Password : String := From_UTF_8 (GNAT.SHA512.Digest (To_UTF_8 (Password_1)));
      Query : String := "";
      Data_Value : String := Gui.Get_Connection_Data (Object, Users_List_Key & "_Select");
   begin
      Msg.Debug ("Users > Validate_Update");

      --  If Password not changed
      if Is_Empty (Password_1) and Is_Empty (Password_2) then
         Password_1 := "Dummy";
         Password_2 := Password_1;
         Password := Sql.Read (DB, "Sys_Users", "Password", "WHERE " & Users_Key_Main & " = '" &  Login & "'");
      end if;

      if not Is_Empty (Password_1) then
         if Password_1 = Password_2 then
            Query := "First_Name" & ND & First_Name & CD &
                     "Last_Name" & ND & Last_Name & CD &
                     "Phone" & ND & Phone & CD &
                     "Email" & ND & Email & CD &
                     "Password" & ND & Password & CD &
                     "Updated_On" & ND & Prg.Date_Time_Stamp;
            Msg.Debug ("Query: " & Query);
            Sql.Update (DB, "Sys_Users", Query, Users_Key_Main & " = '" &  Login & "'");

            Put_List (Object,"WHERE Login >= '" & Data_Value & "' ORDER BY " &
                Users_Key_Main & " LIMIT " & To_String_Unsigned (Users_List_Length));
            --Put_List (Object);
         else
            Put_Error_Message (Object, "Les mots de passe ne correspondent pas");
         end if;
      else
         Put_Error_Message (Object, "Le mot de passe est manquant");
      end if;

   end On_Validate_Update;

   ----------------------------------------------------------------------------
   procedure Put_Error_Message (Object : in out Gnoga.Gui.Base.Base_Type'Class; Error : String) is
   begin
      Gui.Content_Group_Warning_Set (Object, "register-error", Error);
   end Put_Error_Message;

   ----------------------------------------------------------------------------
   procedure Put_Form (Object : in out Gnoga.Gui.Base.Base_Type'Class; Mode : Db_Mode := None; Key : String := "") is
      Edit_Title : String := "Gestion des utilisateurs";
      Query : String := "";
      Query_Result : String := "";
   begin

      Gui.Content_Clear_Title (Object);
      Gui.Content_Clear_Text (Object);

      Gui.Content_Group_Create (Object, Edit_Title);
      if Mode = Create then
         Gui.Content_Group_Add_Title (Object, "Création", Edit_Title);
      elsif Mode = Read then
         Gui.Content_Group_Add_Title (Object, "Visualisation", Edit_Title);
      elsif Mode = Update then
         Gui.Content_Group_Add_Title (Object,"Modification", Edit_Title);
      elsif Mode = Delete then
         Gui.Content_Group_Add_Title (Object,"Confirmer la suppression ?", Edit_Title);
      elsif Mode = Search then
         Gui.Content_Group_Add_Title (Object,"Recherche", Edit_Title);
      end if;

      Gui.Content_Group_Text_Add (Object, "Identifiant", Edit_Title);
      if not (Mode = Search) then
         Gui.Content_Group_Text_Add (Object, "Prénom", Edit_Title);
         Gui.Content_Group_Text_Add (Object, "Nom", Edit_Title);
         Gui.Content_Group_Phone_Add (Object, "Téléphone", Edit_Title);
         Gui.Content_Group_Email_Add (Object, "Email", Edit_Title);
         Gui.Content_Group_Password_Add (Object, "Mot de passe", Edit_Title);
         Gui.Content_Group_Password_Add (Object, "Mot de passe (confirmation)", Edit_Title);
      end if;

      if Mode = Create then
         Gui.Dialog_Buttons (Object, Edit_Title, "Annuler", On_Cancel'Unrestricted_Access,
                                                 "Créer", On_Validate_Create'Unrestricted_Access);
      elsif Mode = Search then
         Gui.Dialog_Buttons (Object, Edit_Title, "Annuler", On_Cancel'Unrestricted_Access,
                                                 "Chercher", On_Validate_Search'Unrestricted_Access);
      else
         Query := "Login" & VD &
                  "First_Name" & VD &
                  "Last_Name" & VD &
                  "Phone" & VD &
                  "Email" & VD &
                  "Password";
         Msg.Debug ("Query: " & Query);
         Query_Result := Sql.Read (DB, "Sys_Users", Query,"WHERE " & Users_Key_Main & " = '" & Key & "'");

         Gui.Content_Group_Item_Lock (Object, "Identifiant");
         if not (Mode = Update) then
            Gui.Content_Group_Item_Lock (Object, "Prénom");
            Gui.Content_Group_Item_Lock (Object, "Nom");
            Gui.Content_Group_Item_Lock (Object, "Téléphone");
            Gui.Content_Group_Item_Lock (Object, "Email");
            Gui.Content_Group_Item_Lock (Object, "Mot de passe");
            Gui.Content_Group_Item_Lock (Object, "Mot de passe (confirmation)");
         end if;

         Gui.Content_Group_Text_Set (Object, "Identifiant", Field_By_Index (Query_Result, 1, CD));
         Gui.Content_Group_Text_Set (Object, "Prénom", Field_By_Index (Query_Result, 2, CD));
         Gui.Content_Group_Text_Set (Object, "Nom", Field_By_Index (Query_Result, 3, CD));
         Gui.Content_Group_Phone_Set (Object, "Téléphone", Field_By_Index (Query_Result, 4, CD));
         Gui.Content_Group_Email_Set (Object, "Email", Field_By_Index (Query_Result, 5, CD));
         Gui.Content_Group_Password_Set (Object, "Mot de passe", 20 * "-");
         Gui.Content_Group_Password_Set (Object, "Mot de passe (confirmation)", 20 * "-");

         if Mode = Read then
            Gui.Dialog_Buttons (Object, Edit_Title, "Quitter", On_Cancel'Unrestricted_Access);
         elsif Mode = Update then
            Gui.Dialog_Buttons (Object, Edit_Title, "Annuler", On_Cancel'Unrestricted_Access,
                                                    "Modifier",On_Validate_Update'Unrestricted_Access);
         elsif Mode = Delete then
            Gui.Dialog_Buttons (Object, Edit_Title, "Annuler", On_Cancel'Unrestricted_Access,
                                                    "Supprimer",On_Validate_Delete'Unrestricted_Access);
         end if;

      end if;
   end Put_Form;

   ----------------------------------------------------------------------------
   procedure Put_List (Object : in out GGB.Base_Type'Class; Condition : String := "ORDER BY " & Users_Key_Main & " LIMIT " &
                       To_String_Unsigned (Users_List_Length)) is
   begin
      Gui.Content_Clear_Title (Object);
      Gui.Content_Clear_Text (Object);
      Gui.List (Object, DB, Users_List_Key,
                             "Liste des utilisateurs",
                             "Sys_Users",
                             "~Login, First_Name, Last_Name, Phone, Email",
                             "Identifiant, Prénom, Nom, Téléphone, Email",
                             Condition);
   end Put_List;

-------------------------------------------------------------------------------
end Adm_Users;
-------------------------------------------------------------------------------
