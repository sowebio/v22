-------------------------------------------------------------------------------
--  @file      v22-gui-connection.adb
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Connection management
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

separate (v22.Gui) package body Connection is

   ----------------------------------------------------------------------------
   --  PRIVATE DECLARATIONS
   ----------------------------------------------------------------------------

   Login_Group_Key : constant String := "Se connecter";
   Password_Forgotten_Group_Key : constant String := "Mot de passe oublié";
   Password_New_Update_Group_Key : constant String := "Nouveau mot de passe";

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Put_Login_Form (Object : in out GGB.Base_Type'Class) is
   --  Display login form
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Content_Clear_Title (Object);
      Content_Clear_Text (Object);

      Content_Group_Create (Object, Login_Group_Key);
      Content_Group_Add_Space (Object, Login_Group_Key);
      Content_Group_Text_Add (Object, "Identifiant", Login_Group_Key);
      Content_Group_Password_Add (Object, "Mot de passe", Login_Group_Key);

      Put_Login_Buttons (Object);
   end Put_Login_Form;

   ----------------------------------------------------------------------------
   procedure Put_Login_Buttons (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Login_Group_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;

      First_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Second_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Password_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
      Submit_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
   begin
      Row.Dynamic;
      First_Column.Dynamic;
      Second_Column.Dynamic;
      Password_Button.Dynamic;
      Submit_Button.Dynamic;

      Content_Group_Add_Space (Object, Login_Group_Key);
      Content_Group_Warning_Add (Object, "", "login-error", Login_Group_Key);

      Row.Create (Table.all);
      First_Column.Create (Row.all);
      Second_Column.Create (Row.all);

      Password_Button.Create (First_Column.all, "Mot de passe oublié...");
      Password_Button.Class_Name ("button-light");
      Password_Button.On_Click_Handler (Put_Password_Forgotten_Form'Unrestricted_Access);

      Submit_Button.Create (Second_Column.all, "Connexion");
      Submit_Button.Class_Name ("button-standard");
      Submit_Button.On_Click_Handler (On_Login_Connection'Unrestricted_Access);
   end Put_Login_Buttons;

   ----------------------------------------------------------------------------
   procedure Put_Login_Message (Object : in out GGB.Base_Type'Class; Error : String) is
   begin
      Content_Group_Warning_Set (Object, "login-error", Error);
   end Put_Login_Message;

   ----------------------------------------------------------------------------
   procedure On_Login_Connection (Object : in out GGB.Base_Type'Class) is
      App  : constant App_Access := App_Access (Object.Connection_Data);
      Login : constant String := Content_Group_Text_Get (Object, "Identifiant");
      Password : constant String := Content_Group_Password_Get (Object, "Mot de passe");
      DBT : Sql.Database_Line_Type;

      DB_Request, DB_Where : String;
      DB_Result, DB_First_Name_Initial : String := "";
      DB_Id, DB_First_Name, DB_Last_Name, DB_Password, DB_Grants : String;
      DB_Password_Validity, DB_Password_Errors, DB_Password_Errors_Max, DB_Connection_Total, DB_Connection_Counter : Natural;

      -------------------------------------------------------------------------
      function Connection_Attempts return String is
      begin
         return To_String (DB_Password_Errors) & "/" & Sql.Get_Config ("Users_Max_Login_Errors");
      end Connection_Attempts;

      -------------------------------------------------------------------------
      procedure Connection_Attempts_Update is
      begin
         if DBT.Brand = Sql.MySQL then
            Sql.Update (DBT.DBM, "Sys_Users", "Password_Errors" & ND & To_String (DB_Password_Errors) & CD &
                                 "DTS_Update" & ND & Prg.Date_Time_Stamp, "Login = '" & Login & "'");
         elsif DBT.Brand = Sql.SQLite then
            Sql.Update (DBT.DBS, "Sys_Users", "Password_Errors" & ND & To_String (DB_Password_Errors) & CD &
                                 "DTS_Update" & ND & Prg.Date_Time_Stamp, "Login = '" & Login & "'");
         else
            Msg.Error ("Gui.On_Login_Connection > Properties not found for " & Sql.Get_Database_Main);
         end if;
      end Connection_Attempts_Update;

      -------------------------------------------------------------------------
      procedure Connection_Update is
      begin
         DB_Connection_Counter := DB_Connection_Counter + 1;
         if DBT.Brand = Sql.MySQL then
            Sql.Update (DBT.DBM, "Sys_Users", "Password_Errors" & ND & "0" & CD &
                                              "Connection_Counter" & ND & To_String (DB_Connection_Counter) & CD &
                                              "DTS_Update" & ND & Prg.Date_Time_Stamp, "Login = '" & Login & "'");
         elsif DBT.Brand = Sql.SQLite then
            Sql.Update (DBT.DBS, "Sys_Users", "Password_Errors" & ND & "0" & CD &
                                              "Connection_Counter" & ND & To_String (DB_Connection_Counter) & CD &
                                              "DTS_Update" & ND & Prg.Date_Time_Stamp, "Login = '" & Login & "'");
         else
            Msg.Error ("Gui.On_Login_Connection > Properties not found for " & Sql.Get_Database_Main);
         end if;
      end Connection_Update;

   begin
      Msg.Info ("Gui.On_Login_Connection > New user on logging screen");
      --  Have Login and Password
      if not (Is_Empty (Login) or Is_Empty (Password)) then
         DB_Request := "Id" & VD &
                       "First_Name" & VD &
                       "Last_Name" & VD &
                       "Password" & VD &
                       "Grants" & VD &
                       "coalesce(Password_Validity, 0)" & VD &
                       "coalesce(Password_Errors, 0)" & VD &
                       "coalesce(Connection_Total, 0)" & VD &
                       "coalesce(Connection_Counter, 0)";
         DB_Where := "WHERE Login = '" & Login & "'";
         DBT := Sql.Properties (Sql.Get_Database_Main);
         if DBT.Brand = Sql.MySQL then
            DB_Result := Sql.Read (DBT.DBM, "Sys_Users", DB_Request, DB_Where);
         elsif DBT.Brand = Sql.SQLite then
            DB_Result := Sql.Read (DBT.DBS, "Sys_Users", DB_Request, DB_Where);
         else
            Msg.Error ("Gui.On_Login_Connection > Properties not found for " & Sql.Get_Database_Main);
         end if;
         --  Test Login
         if not Is_Empty (DB_Result) then
            Msg.Info (DB_Result);
            DB_Id := Field_By_Index (DB_Result, 1, CD);
            DB_First_Name := Field_By_Index (DB_Result, 2, CD);
            DB_Last_Name := Field_By_Index (DB_Result, 3, CD);
            DB_Password := Field_By_Index (DB_Result, 4, CD);
            DB_Grants := Field_By_Index (DB_Result, 5, CD);
            DB_Password_Validity := To_Integer (Field_By_Index (DB_Result, 6, CD));
            DB_Password_Errors := To_Integer (Field_By_Index (DB_Result, 7, CD));
            DB_Connection_Total := To_Integer (Field_By_Index (DB_Result, 8, CD));
            DB_Connection_Counter := To_Integer (Field_By_Index (DB_Result, 9, CD));

            --  Test Connection errors
            DB_Password_Errors_Max := To_Integer (Sql.Get_Config ("Users_Max_Login_Errors"));
            DB_Password_Errors := DB_Password_Errors + 1;
            if DB_Password_Errors_Max = 0 then -- Users_Max_Login_Errors not found in Sys_Config or Sys_Config not found
               DB_Password_Errors_Max := 5;
            end if;

            if DB_Password_Errors <= DB_Password_Errors_Max then
               Msg.Debug ("DB_Password_Errors: " & To_String (DB_Password_Errors));
               Msg.Debug ("DB_Connection_Counter: " & To_String (DB_Connection_Counter));
               --  Test Password
               if DB_Password = From_Latin_1 (GNAT.SHA512.Digest (To_Latin_1 (Password))) then
                  --  Regular connection or forgotten password connection
                  if DB_Password_Validity = 0 then
                     --  Load user properties
                     App.User_Logged_In := True;
                     App.User_Logged_Since := AC.Clock;
                     --App.User_Display_In_Progress := False;

                     Connection_Update;
                     Msg.Info ("Gui.On_Login_Connection > User " & Login & " connected");

                     Connection_Data_Set (Object, "Id", DB_Id);
                     Connection_Data_Set (Object, "User_Login", Login);
                     Connection_Data_Set (Object, "First_Name", DB_First_Name);
                     Connection_Data_Set (Object, "Last_Name", DB_Last_Name);
                     Connection_Data_Set (Object, "Grants", DB_Grants);
                     Connection_Data_Set (Object, "Connection_Counter", To_String_Unsigned (DB_Connection_Counter));
                     Connection_Data_Set (Object, "Connection_Total", To_String_Unsigned (DB_Connection_Total));
                     Connection_Data_Set (Object, "Connection_Start_Session", To_String_Unsigned (Prg.Current_Time_Seconds));

                     --Connection_Data_Set (Object, "Menus_Initialize", "True");

                     --  Name formatting
                     if DB_First_Name.Length >= 1 then
                        DB_First_Name_Initial := Slice (DB_First_Name, 1, 1) & ". ";
                     end if;
                     Gui.Set_User_Name (Object, DB_First_Name_Initial & DB_Last_Name);

                     --  Launch point of On_Main routine from Gui.Header_Set_Root which is called at then main source's end.
                     App.Header_Instance.Set_Menu (ID_Main);
                  else
                     if (DB_Password_Validity - Prg.Current_Time_Seconds) >= 0 then
                        Connection_Data_Set (Object, "User_Login", Login);
                        Put_New_Password_Form (Object);
                     else
                        Msg.Info ("Gui.On_Login_Connection > User " & Login & " found but temporary password has expired");
                        Put_Login_Message (Object, "Mot de passe temporaire expiré");
                     end if;
                  end if;
               else
                  Msg.Info ("Gui.On_Login_Connection > User " & Login & " found but password not match, connection rejected");
                  Put_Login_Message (Object, "Mot de passe incorrect, erreur de connexion n°" & Connection_Attempts);
                  Connection_Attempts_Update;
               end if;
            else
               Msg.Info ("Gui.On_Login_Connection > User " & Login & " found but connection attempts exceeded " &
                                                             Sql.Get_Config ("Users_Max_Login_Errors") & ", connection rejected");
               Put_Login_Message (Object, "Accès bloqué, contactez le service technique pour rétablir l'accès");
            end if;
         else
            --  As the identifier is non-existent, connection errors cannot be counted.
            Msg.Info ("Gui.On_Login_Connection > User " & Login & ": not found");
            Put_Login_Message (Object, "Identifiant inconnu");
         end if;
      else
         Msg.Info ("Gui.On_Login_Connection > Empty user and/or password, connection rejected");
         Put_Login_Message (Object, "Identifiant et/ou mot de passe vide");
      end if;
   end On_Login_Connection;

   ----------------------------------------------------------------------------
   procedure Put_Password_Forgotten_Form (Object : in out GGB.Base_Type'Class) is
   --  Display login form
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Content_Clear_Title (Object);
      Content_Clear_Text (Object);

      Content_Group_Create (Object, Password_Forgotten_Group_Key);
      Content_Group_Add_Space (Object, Password_Forgotten_Group_Key);
      Content_Group_Text_Add (Object, "Identifiant", Password_Forgotten_Group_Key);

      Put_Password_Forgotten_Buttons (Object);
   end Put_Password_Forgotten_Form;

   ----------------------------------------------------------------------------
   procedure Put_Password_Forgotten_Buttons (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Password_Forgotten_Group_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;

      First_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Second_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Cancel_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
      Submit_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
   begin
      Row.Dynamic;
      First_Column.Dynamic;
      Second_Column.Dynamic;
      Cancel_Button.Dynamic;
      Submit_Button.Dynamic;

      Content_Group_Add_Space (Object, Password_Forgotten_Group_Key);
      Content_Group_Warning_Add (Object, "", "login-error", Password_Forgotten_Group_Key);

      Row.Create (Table.all);
      First_Column.Create (Row.all);
      Second_Column.Create (Row.all);

      Connection_Data_Set (Object, "User_Login", "Test");

      Cancel_Button.Create (First_Column.all, "Annuler");
      Cancel_Button.Class_Name ("button-standard");
      Cancel_Button.On_Click_Handler (Gui.User_Logout'Unrestricted_Access);

      Submit_Button.Create (Second_Column.all, "Envoyer");
      Submit_Button.Class_Name ("button-standard");
      Submit_Button.On_Click_Handler (On_Password_Forgotten_Send_Email'Unrestricted_Access);

   end Put_Password_Forgotten_Buttons;

   ----------------------------------------------------------------------------
   procedure On_Password_Forgotten_Send_Email (Object : in out GGB.Base_Type'Class) is
      App  : constant App_Access := App_Access (Object.Connection_Data);
      Login : constant String := Content_Group_Text_Get (Object, "Identifiant");
      Password : constant String := Content_Group_Password_Get (Object, "Mot de passe");
      --Identity : User_Data;
      DBT : Sql.Database_Line_Type;

      DB_Result : String;
      DB_Email : String;
      Temporary_Password : String := Prg.Generate_Password (DIGITS_DECIMAL, 4);
      DB_Password : String := From_Latin_1 (GNAT.SHA512.Digest (To_Latin_1 (Temporary_Password)));
      DB_Password_Validity : String := To_String_Unsigned (Prg.Current_Time_Seconds + PASSWORD_VALIDITY);

      Body_Text : String := "Bonjour," & CR &
                             CR &
                             "Suite à votre demande, voici votre mot de passe temporaire : " & Temporary_Password & CR &
                             CR &
                             "Vous avez " & To_String_Unsigned (PASSWORD_VALIDITY / 60) &
                             " minutes pour vous reconnecter et choisir un mot de passe définitif." & CR &
                             CR &
                             "Cordialement," & CR &
                             CR &
                             "--" & CR &
                             Replace (Sql.Get_Config ("Email_Footer"), "<CR>", CR);
   begin
      Msg.Info ("Gui.On_Password_Forgotten_Send_Email > New user on logging screen");

      DBT := Sql.Properties (Sql.Get_Database_Main);
      if DBT.Brand = Sql.MySQL then
         DB_Result := Sql.Read (DBT.DBM, "Sys_Users", "Email", "WHERE Login = '" & Login & "'");
      elsif DBT.Brand = Sql.SQLite then
         DB_Result := Sql.Read (DBT.DBS, "Sys_Users", "Email", "WHERE Login = '" & Login & "'");
      else
          Msg.Error ("Gui.On_Password_Forgotten_Send_Email > Properties not found for " & Sql.Get_Database_Main);
      end if;

      if not Is_Empty (DB_Result) then
         DB_Email := Field_By_Index (DB_Result, 1, CD);

         if Net.Send_Mail (Sql.Get_Config ("Email_From"), DB_Email,
                           Sql.Get_Config ("Email_Sender") & " : Mot de passe temporaire", Body_Text,
                           Sql.Get_Config ("Email_Sender")) then

            if DBT.Brand = Sql.MySQL then
               Sql.Update (DBT.DBM, "Sys_Users", "Password" & ND & DB_Password & CD &
                             "Password_Validity" & ND & DB_Password_Validity, "Login = '" & Login & "'");
            elsif DBT.Brand = Sql.SQLite then
               Sql.Update (DBT.DBS, "Sys_Users", "Password" & ND & DB_Password & CD &
                             "Password_Validity" & ND & DB_Password_Validity, "Login = '" & Login & "'");
            else
               Msg.Error ("Gui.On_Password_Forgotten_Send_Email > Properties not found for " & Sql.Get_Database_Main);
            end if;
            Msg.Info ("Gui.On_Password_Forgotten_Send_Email > User " & Login & " found and email sent");
            Gui.Dialog_Popup (Object, Title => "Email envoyé",
                                      Content => "Consultez votre boite mail pour obtenir le mot de passe temporaire.",
                                      Right_Text => "OK",
                                      Right_Handler => On_Dialog_Confirm'Unrestricted_Access);
            Gui.User_Logout (Object);
         else
            Put_Password_Forgotten_Message (Object, "Email non envoyé");
            Msg.Error ("Gui.On_Password_Forgotten_Send_Email > User " & Login & " found but email not sent");
         end if;
      else
         Put_Password_Forgotten_Message (Object, "Identifiant incorrect");
         Msg.Error ("Gui.On_Password_Forgotten_Send_Email > User " & Login & " not found, forgotten password procedure rejected");
      end if;

   end On_Password_Forgotten_Send_Email;

   ----------------------------------------------------------------------------
   procedure Put_Password_Forgotten_Message (Object : in out GGB.Base_Type'Class; Error : String) is
   begin
      Content_Group_Warning_Set (Object, "login-error", Error);
   end Put_Password_Forgotten_Message;

   ----------------------------------------------------------------------------
   procedure Put_New_Password_Form (Object : in out GGB.Base_Type'Class) is
   --  Display login form
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Content_Clear_Title (Object);
      Content_Clear_Text (Object);

      Content_Group_Create (Object, Password_New_Update_Group_Key);
      Content_Group_Add_Space (Object, Password_New_Update_Group_Key);
      Content_Group_Password_Add (Object, "Mot de passe", Password_New_Update_Group_Key);
      Content_Group_Password_Add (Object, "Mot de passe (confirmation)", Password_New_Update_Group_Key);

      Put_New_Password_Buttons (Object);
   end Put_New_Password_Form;

   ----------------------------------------------------------------------------
   procedure Put_New_Password_Buttons (Object : in out GGB.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      Table_Element : constant GGE.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Password_New_Update_Group_Key);
      Table : constant GGE.Table.Table_Access := GGE.Table.Table_Access (Table_Element);
      Row : constant GGE.Table.Table_Row_Access := new GGE.Table.Table_Row_Type;

      First_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Second_Column : constant GGE.Table.Table_Column_Access := new GGE.Table.Table_Column_Type;
      Cancel_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;
      Submit_Button : constant GGE.Common.Button_Access := new GGE.Common.Button_Type;

   begin
      Row.Dynamic;
      First_Column.Dynamic;
      Second_Column.Dynamic;
      Cancel_Button.Dynamic;
      Submit_Button.Dynamic;

      Content_Group_Add_Space (Object, Login_Group_Key);
      Content_Group_Warning_Add (Object, "", "login-error", Password_New_Update_Group_Key);

      Row.Create (Table.all);
      First_Column.Create (Row.all);
      Second_Column.Create (Row.all);

      Cancel_Button.Create (First_Column.all, "Annuler");
      Cancel_Button.Class_Name ("button-standard");
      Cancel_Button.On_Click_Handler (Gui.User_Logout'Unrestricted_Access);

      Submit_Button.Create (Second_Column.all, "Enregistrer");
      Submit_Button.Class_Name ("button-standard");
      Submit_Button.On_Click_Handler (On_New_Password_Update'Unrestricted_Access);

   end Put_New_Password_Buttons;

   ----------------------------------------------------------------------------
   procedure Put_New_Password_Message (Object : in out GGB.Base_Type'Class; Error : String) is
   begin
      Content_Group_Warning_Set (Object, "login-error", Error);
   end Put_New_Password_Message;

   ----------------------------------------------------------------------------
   procedure On_New_Password_Update (Object : in out GGB.Base_Type'Class) is
      App  : constant App_Access := App_Access (Object.Connection_Data);
      Password_1 : constant String := Content_Group_Password_Get (Object, "Mot de passe");
      Password_2 : constant String := Content_Group_Password_Get (Object, "Mot de passe (confirmation)");
      --Identity : User_Data;
      DBT : Sql.Database_Line_Type;
      Login : String := Connection_Data_Get (Object, "User_Login");
      DB_Password : String;
   begin
      Msg.Info ("Gui.On_New_Password_Update > New user on logging screen");

      if Password_1 = Password_2 then
         if Prg.Check_Password (Password_1) then
            DB_Password := From_Latin_1 (GNAT.SHA512.Digest (To_Latin_1 (Password_1)));
            DBT := Sql.Properties (Sql.Get_Database_Main);
            if DBT.Brand = Sql.MySQL then
               Sql.Update (DBT.DBM, "Sys_Users", "Password" & ND & DB_Password & CD &
                             "Password_Validity" & ND & "0", "Login = '" & Login & "'");
            elsif DBT.Brand = Sql.SQLite then
               Sql.Update (DBT.DBS, "Sys_Users", "Password" & ND & DB_Password & CD &
                             "Password_Validity" & ND & "0", "Login = '" & Login & "'");
            else
               Msg.Error ("Gui.On_New_Password_Update > Properties not found for " & Sql.Get_Database_Main);
            end if;
            Msg.Info ("Gui.On_New_Password_Update > User " & Login & " found and new password registered");
            Gui.Dialog_Popup (Object, Title => "Mot de passe",
                                      Content => "Nouveau mot de passe enregistré, veuillez vous reconnecter.",
                                      Right_Text => "OK",
                                      Right_Handler => On_Dialog_Confirm'Unrestricted_Access);
            Gui.User_Logout (Object);
         else
            Put_Login_Message (Object, "Le mot de passe doit contenir au moins " & To_String_Unsigned (PASSWORD_MINIMUM_LENGTH) &
                                       " caractères, dont une majuscule, une minuscule, un chiffre et un caractère spécial '-' ou '_'");
            Msg.Error ("Gui.On_New_Password_Update > A password must have at least " &
                      To_String_Unsigned (PASSWORD_MINIMUM_LENGTH) &
                      " and contains uppercase, lowercase, digit and one special char among '-' and '_'");
         end if;
      else
         Put_Login_Message (Object, "Les deux mots de passe ne sont pas identiques");
         Msg.Error ("Gui.On_New_Password_Update > User " & Login & " found but password not match, connection rejected");
      end if;

   end On_New_Password_Update;

   ----------------------------------------------------------------------------
   procedure On_Dialog_Confirm (Object : in out GGB.Base_Type'Class) is
   begin
      Gnoga.Log ("Dialog: confirmed");
      Gui.Close_Dialog (Object);
   end On_Dialog_Confirm;

-------------------------------------------------------------------------------
end Connection;
-------------------------------------------------------------------------------
