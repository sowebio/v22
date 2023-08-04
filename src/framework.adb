with Gnoga.Types;
with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.Plugin;
with Gnoga.Gui.Window;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.Element.Table;
with Gnoga.Application.Multi_Connect;

with GNAT.SHA512;

with Header;
with CRUD;
with Footer;

package body Framework is

   use all type Gnoga.String;

   Login_Group_Key : constant UXString := "Se connecter";

   package Element renames Gnoga.Gui.Element;

   package User_Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => UXString, Element_Type => User_Data, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   package Integer_Dictionary is new Ada.Containers.Hashed_Maps
     (Key_Type => UXString, Element_Type => Integer, Hash => UXStrings.Hash, Equivalent_Keys => "=");

   ID_Main                   : Integer;
   On_Custom_Connect         : Base.Action_Event;
   On_Custom_Login           : Base.Action_Event;
   On_Custom_Register        : Register_Function;
   On_Custom_Register_Create : Base.Action_Event;

   Header_Dict : Integer_Dictionary.Map;

   Browse_Icon_SRC : UXString := "";
   User_Icon_SRC   : UXString := "";

   Identities     : User_Dictionary.Map;
   Login_Required : Boolean := False;

   type App_Data is new Gnoga.Types.Connection_Data_Type with record
      Is_Logged : Boolean  := False;
      Email     : UXString := ""; -- As a dictionnary key

      Window    : Gnoga.Gui.Window.Pointer_To_Window_Class;
      Container : View.View_Type;

      Header_Content : Header.Header_Type;

      Header_Parent : View.View_Type;
      CRUD_Parent   : View.View_Type;
      Content       : View.View_Type;
      Footer_Parent : View.View_Type;

      Content_Header : View.View_Type;
      Content_Text   : aliased View.View_Type;

      Footer_Content : Footer.Footer_Type;

      Crud_Dict   : Integer_Dictionary.Map;
      Header_Dict : Integer_Dictionary.Map;

      CRUD_Instance : CRUD.CRUD_Type;
   end record;
   type App_Access is access all App_Data;

   procedure Print (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Window.Print;
   end Print;

   procedure CRUD_Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Value  :        Integer)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Crud_Dict.Insert (Key, Value);
   end CRUD_Set;

   function CRUD_Get
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
      return Integer
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Crud_Dict.Element (Key);
   end CRUD_Get;

   procedure Header_Set
     (Key   : UXString;
      Value : Integer)
   is
   begin
      Header_Dict.Insert (Key, Value);
   end Header_Set;

   function Header_Get
     (Key : UXString)
      return Integer
   is
   begin
      return Header_Dict.Element (Key);
   end Header_Get;

   -----------------------------------------------------------------------------
   --  CRUD Handlers
   -----------------------------------------------------------------------------

   procedure On_CRUD_Callback (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Element_Click (Object);
   end On_CRUD_Callback;

   procedure On_Key_Pressed
     (Object : in out Base.Base_Type'Class;
      Char   :        Character)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Key_Pressed (Char);
   end On_Key_Pressed;

   -----------------------------------------------------------------------------
   --  Menu Handlers
   -----------------------------------------------------------------------------

   procedure On_Logo (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.Is_Logged then
         if App.Header_Content.Is_Menu_Open then
            App.Header_Content.Close_Menu;
         else
            --  The reason why this function is here and not in CRUD.adb
            App.CRUD_Instance.Clear;
            App.Header_Content.Open_Menu (ID_Main);
            App.Header_Content.Close_User_Menu;
         end if;
      end if;
   end On_Logo;

   procedure On_User (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      if App.Is_Logged then
         if App.Header_Content.Is_User_Menu_Open then
            App.Header_Content.Close_User_Menu;
         else
            App.Header_Content.Open_User_Menu;
            App.Header_Content.Close_Menu;
         end if;
      end if;
   end On_User;

   -----------------------------------------------------------------------------
   --  Tool Bar expand button
   -----------------------------------------------------------------------------

   procedure On_Tool_Bar_Expand (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Resize;
   end On_Tool_Bar_Expand;

   -----------------------------------------------------------------------------
   --  Verify user identity
   -----------------------------------------------------------------------------

   ------------------
   --  Login form  --
   ------------------
   procedure On_Login (Object : in out Base.Base_Type'Class) is
      App      : constant App_Access := App_Access (Object.Connection_Data);
      Email    : constant UXString   := Content_Group_Email_Get (Object, "Adresse mail");
      Password : constant UXString   := Content_Group_Password_Get (Object, "Mot de passe");
   begin
      if Identities.Contains (Email) then
         declare
            Identity : constant User_Data := Identities.Element (Email);
         begin
            if From_UTF_8 (GNAT.SHA512.Digest (To_UTF_8 (Password))) = Identity.Password_Hash then
               App.Email := Email;
               if On_Custom_Login /= null then
                  On_Custom_Login (Object);
               end if;
               App.Header_Content.Set_Menu (ID_Main);
               App.Header_Content.Set_User_Name (Identity.User_Name);
               App.Is_Logged := True;
            else
               Set_Login_Error_Message (Object, "Mot de passe incorrect");
            end if;
         end;
      else
         Set_Login_Error_Message (Object, "Adresse mail inexistante. Créez un compte !");
      end if;
   end On_Login;

   procedure Setup_Login_Form (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Email := "";

      Content_Clear_Title (Object);
      Content_Clear_Text (Object);

      Content_Group_Create (Object, Login_Group_Key);

      Content_Group_Add_Space (Object, Login_Group_Key);
      Content_Group_Email_Add (Object, "Adresse mail", Login_Group_Key);
      Content_Group_Password_Add (Object, "Mot de passe", Login_Group_Key);

      Setup_Login_Buttons (Object);
   end Setup_Login_Form;

   procedure Setup_Login_Buttons (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Table_Element : constant Element.Pointer_To_Element_Class :=
        App.Content_Text.Element ("Table_" & Login_Group_Key);
      Table : constant Element.Table.Table_Access := Element.Table.Table_Access (Table_Element);

      Row           : constant Element.Table.Table_Row_Access    := new Element.Table.Table_Row_Type;
      First_Column  : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;
      Second_Column : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;

      Register_Link : constant Element.Common.Button_Access := new Element.Common.Button_Type;
      Submit_Button : constant Element.Common.Button_Access := new Element.Common.Button_Type;
   begin
      Content_Group_Add_Space (Object, Login_Group_Key);
      Content_Group_Warning_Add (Object, "", "login-error", Login_Group_Key);

      Row.Create (Table.all);
      First_Column.Create (Row.all);
      Second_Column.Create (Row.all);

      Register_Link.Create (First_Column.all, "Créer un compte...");
      Register_Link.Class_Name ("content-group-link");
      Register_Link.Style ("white-space", "nowrap");
      Register_Link.On_Click_Handler (Setup_Register_Form'Unrestricted_Access);

      Submit_Button.Create (Second_Column.all, "Connexion");
      Submit_Button.Style ("width", "100%");
      Submit_Button.Style ("box-sizing", "border-box");
      Submit_Button.On_Click_Handler (On_Login'Unrestricted_Access);
   end Setup_Login_Buttons;

   ----------------
   --  Register  --
   ----------------
   procedure On_Register (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);

      User_Name : constant UXString := Content_Group_Text_Get (Object, "Nom d'utilisateur");
      Email     : constant UXString := Content_Group_Email_Get (Object, "Adresse mail");
      Password  : constant UXString := Content_Group_Password_Get (Object, "Mot de passe");
      Confirmed : constant UXString := Content_Group_Password_Get (Object, "Confirmer le mot de passe");
      Identity  : User_Data;
      Allowed   : Boolean           := True;
   begin
      Gnoga.Log ("Created an account for " & User_Name & ", Email : " & Email);

      Identity.Password_Hash := From_UTF_8 (GNAT.SHA512.Digest (To_UTF_8 (Password)));
      Identity.User_Name     := User_Name;
      Identity.Email         := Email;

      if Identities.Contains (Email) then
         Set_Register_Error_Message (Object, "Cette adresse est déjà associée à un compte. Connectez-vous !");
      elsif Email /= "" then
         if Password /= "" then
            if Password = Confirmed then
               App.Email := Email;
               if On_Custom_Register /= null then
                  Allowed := On_Custom_Register (Object, Identity);
               end if;
               if Allowed then
                  Setup_Login_Form (Object);
                  Identities.Insert (Email, Identity);
               else
                  App.Email := "";
               end if;
            else
               Set_Register_Error_Message (Object, "Les mots de passes ne correspondent pas");
            end if;
         else
            Set_Register_Error_Message (Object, "Veuillez renseigner un mot de passe");
         end if;
      else
         Set_Register_Error_Message (Object, "Veuillez renseigner votre adresse mail");
      end if;
   end On_Register;

   procedure Setup_Register_Form (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Email := "";

      Content_Clear_Title (Object);
      Content_Clear_Text (Object);

      Content_Group_Create (Object, Register_Group_Key);

      Content_Group_Add_Title (Object, "Identifiants", Register_Group_Key);
      Content_Group_Text_Add (Object, "Nom d'utilisateur", Register_Group_Key);
      Content_Group_Email_Add (Object, "Adresse mail", Register_Group_Key);
      Content_Group_Password_Add (Object, "Mot de passe", Register_Group_Key);
      Content_Group_Password_Add (Object, "Confirmer le mot de passe", Register_Group_Key);

      if On_Custom_Register_Create /= null then
         On_Custom_Register_Create (Object);
      end if;
      Setup_Register_Buttons (Object);
   end Setup_Register_Form;

   procedure Setup_Register_Buttons (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Table_Element : constant Element.Pointer_To_Element_Class :=
        App.Content_Text.Element ("Table_" & Register_Group_Key);
      Table : constant Element.Table.Table_Access := Element.Table.Table_Access (Table_Element);

      Row           : constant Element.Table.Table_Row_Access    := new Element.Table.Table_Row_Type;
      First_Column  : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;
      Second_Column : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;

      Login_Link    : constant Element.Common.Button_Access := new Element.Common.Button_Type;
      Submit_Button : constant Element.Common.Button_Access := new Element.Common.Button_Type;
   begin
      Content_Group_Add_Space (Object, Register_Group_Key);
      Content_Group_Warning_Add (Object, "", "register-error", Register_Group_Key);

      Row.Create (Table.all);
      First_Column.Create (Row.all);
      Second_Column.Create (Row.all);

      Login_Link.Create (First_Column.all, "Se connecter...");
      Login_Link.Class_Name ("content-group-link");
      Login_Link.Style ("white-space", "nowrap");
      Login_Link.On_Click_Handler (Setup_Login_Form'Unrestricted_Access);

      Submit_Button.Create (Second_Column.all, "Valider");
      Submit_Button.Style ("width", "100%");
      Submit_Button.Style ("box-sizing", "border-box");
      Submit_Button.On_Click_Handler (On_Register'Unrestricted_Access);
   end Setup_Register_Buttons;

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
      App.Container.Style ("display", "flex");

      --  Containers
      App.Container.Style ("width", "100%");
      App.Container.Style ("height", "100%");

      App.Header_Parent.Create (App.Container);
      App.Header_Parent.Class_Name ("header");

      App.CRUD_Parent.Create (App.Container);
      App.CRUD_Parent.Class_Name ("crud");

      App.Content.Create (App.Container);
      App.Content.Class_Name ("content-container");

      App.Footer_Parent.Create (App.Container);
      App.Footer_Parent.Class_Name ("footer");

      --  Content
      App.Content_Header.Create (App.Content);
      App.Content_Header.Class_Name ("content-header");
      App.Content_Text.Create (App.Content);
      App.Content_Text.Class_Name ("content-text");

      --  Header
      App.Header_Content.Create (App.Header_Parent, On_Logo'Unrestricted_Access, On_User'Unrestricted_Access);
      App.Header_Content.Set_App_Icon (Browse_Icon_SRC);
      App.Header_Content.Set_User_Icon (User_Icon_SRC);

      --  Footer
      App.Footer_Content.Create (App.Footer_Parent);

      --  CRUD
      App.CRUD_Instance.Create
        (App.CRUD_Parent, On_Tool_Bar_Expand'Unrestricted_Access, On_CRUD_Callback'Unrestricted_Access);

      App.Is_Logged := not Login_Required;
      if Login_Required then
         Setup_Login_Form (App.Container);
      else
         App.Header_Content.Set_Menu (ID_Main);
      end if;

      On_Custom_Connect (App.Container);
   end On_Connect;

   procedure Add_Root_User is
      Identity : User_Data;
      Email    : constant UXString := "root@root";
      Password : constant UXString := "password";
   begin
      Identity.Email         := Email;
      Identity.Password_Hash := From_UTF_8 (GNAT.SHA512.Digest (To_UTF_8 (Password)));
      Identity.User_Name     := "Root User";
      Identities.Insert (Email, Identity);
   end Add_Root_User;

   procedure Setup
     (On_User_Connect       : Base.Action_Event;
      Title                 : UXString := "";
      Server_Closed_Content : UXString := "Server closed.")
   is
   begin
      Gnoga.Application.Title (Title);
      Gnoga.Application.HTML_On_Close (Server_Closed_Content);
      Gnoga.Application.Multi_Connect.Initialize (Boot => "boot_jqueryui.html");
      Gnoga.Application.Multi_Connect.On_Connect_Handler (On_Connect'Unrestricted_Access);
      On_Custom_Connect := On_User_Connect;
   end Setup;

   procedure Setup_Access
     (On_User_Login           : Base.Action_Event := null;
      On_User_Register_Create : Base.Action_Event := null;
      On_User_Register        : Register_Function := null)
   is
   begin
      Login_Required            := True;
      On_Custom_Login           := On_User_Login;
      On_Custom_Register        := On_User_Register;
      On_Custom_Register_Create := On_User_Register_Create;
   end Setup_Access;

   procedure Set_Login_Error_Message
     (Object : in out Base.Base_Type'Class;
      Error  :        UXString)
   is
   begin
      Content_Group_Warning_Set (Object, "login-error", Error);
   end Set_Login_Error_Message;

   procedure Set_Register_Error_Message
     (Object : in out Base.Base_Type'Class;
      Error  :        UXString)
   is
   begin
      Content_Group_Warning_Set (Object, "register-error", Error);
   end Set_Register_Error_Message;

   procedure Disconnect_User (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Setup_Login_Form (Object);
      App.Header_Content.Set_User_Name ("");
      App.Header_Content.Clear;
      App.Header_Content.Close_Menu;
      App.Header_Content.Close_User_Menu;
      App.Header_Content.App_Browse_Parent.Inner_HTML ("");
      App.CRUD_Instance.Clear;
      App.Crud_Dict.Clear;
      App.Is_Logged := False;
   end Disconnect_User;

   procedure Set_App_Title
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Window.Document.Title (Title);
   end Set_App_Title;

   procedure Set_App_Icon (Icon_SRC : UXString) is
   begin
      Gnoga.Application.Favicon (Icon_SRC);
   end Set_App_Icon;

   procedure Set_Browse_Icon (Icon_SRC : UXString) is
   begin
      Browse_Icon_SRC := Icon_SRC;
   end Set_Browse_Icon;

   procedure Set_Default_User_Icon (Icon_SRC : UXString) is
   begin
      User_Icon_SRC := Icon_SRC;
   end Set_Default_User_Icon;

   procedure Set_User_Icon
     (Object   : in out Base.Base_Type'Class;
      Icon_SRC :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Set_User_Icon (Icon_SRC);
   end Set_User_Icon;

   procedure Set_User_Name
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Set_User_Name (Name);
   end Set_User_Name;

   -----------------------------------------------------------------------------
   --  Header
   -----------------------------------------------------------------------------

   procedure Header_Set_Root
     (Key      : UXString;
      Name     : UXString;
      On_Click : Base.Action_Event)
   is
      Unique_ID : Integer;
   begin
      Unique_ID := Header.Set_Root (Name, On_Click);
      Header_Set (Key, Unique_ID);
      ID_Main := Unique_ID;
   end Header_Set_Root;

   procedure Header_Add_Child
     (Key        : UXString;
      Name       : UXString;
      Parent_Key : UXString;
      On_Click   : Base.Action_Event)
   is
      Unique_ID : Integer;
   begin
      Unique_ID := Header.Add_Child (Header_Get (Parent_Key), Name, On_Click);
      Header_Set (Key, Unique_ID);
   end Header_Add_Child;

   procedure Header_Add_Dialog
     (Title        : UXString;
      Content      : UXString          := "";
      Confirm_Text : UXString          := "";
      Cancel_Text  : UXString          := "";
      On_Confirm   : Base.Action_Event := null;
      On_Cancel    : Base.Action_Event := null)
   is
   begin
      Header.Add_Dialog (Title, Content, Confirm_Text, Cancel_Text, On_Confirm, On_Cancel);
   end Header_Add_Dialog;

   procedure Header_Add_Web
     (Title : UXString;
      URL   : UXString)
   is
   begin
      Header.Add_Web (Title, URL);
   end Header_Add_Web;

   procedure Header_Add_Button
     (Title    : UXString;
      On_Click : Base.Action_Event)
   is
   begin
      Header.Add_Button (Title, On_Click);
   end Header_Add_Button;

   -----------------
   --  Callbacks  --
   -----------------

   procedure Header_Notify_Menu_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Header_Content.Notify_Menu_Click (Header_Get (Key));
      App.Content_Header.Inner_HTML ("");
      App.Content_Text.Inner_HTML ("");
      App.CRUD_Instance.Clear;
      App.Crud_Dict.Clear;
   end Header_Notify_Menu_Click;

   -----------------------------------------------------------------------------
   --  CRUD
   -----------------------------------------------------------------------------

   procedure CRUD_Load (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Load;
   end CRUD_Load;

   procedure CRUD_Add_Element
     (Object   : in out Base.Base_Type'Class;
      Key      :        UXString;
      Name     :        UXString;
      Icon_SRC :        UXString)
   is
      App       : constant App_Access := App_Access (Object.Connection_Data);
      Unique_ID : Integer;
   begin
      Unique_ID := App.CRUD_Instance.Add_Element (Name, Icon_SRC);
      CRUD_Set (Object, Key, Unique_ID);
   end CRUD_Add_Element;

   procedure CRUD_Add_Sub_Element
     (Object     : in out Base.Base_Type'Class;
      Key        :        UXString;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Click   :        Base.Action_Event)
   is
      App       : constant App_Access := App_Access (Object.Connection_Data);
      Unique_ID : Integer;
   begin
      Unique_ID := App.CRUD_Instance.Add_Sub_Element (Name, CRUD_Get (Object, Parent_Key), On_Click);
      CRUD_Set (Object, Key, Unique_ID);
   end CRUD_Add_Sub_Element;

   procedure CRUD_Add_Delimiter_Above
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Add_Delimiter_Above (CRUD_Get (Object, Key));
   end CRUD_Add_Delimiter_Above;

   procedure CRUD_Set_Unclickable
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Set_Unclickable (CRUD_Get (Object, Key));
   end CRUD_Set_Unclickable;

   procedure CRUD_Set_Clickable
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Set_Clickable (CRUD_Get (Object, Key));
   end CRUD_Set_Clickable;

   procedure CRUD_Notify_Sub_Element_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.CRUD_Instance.Notify_Sub_Element_Click (CRUD_Get (Object, Key));
   end CRUD_Notify_Sub_Element_Click;

   procedure CRUD_Enable_Shortcuts (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      CRUD.Enable_Shortcuts (App.CRUD_Instance);
   end CRUD_Enable_Shortcuts;

   procedure CRUD_Disable_Shortcuts (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      CRUD.Disable_Shortcuts (App.CRUD_Instance);
   end CRUD_Disable_Shortcuts;

   -----------------------------------------------------------------------------
   --  Content
   -----------------------------------------------------------------------------

   function Content_Parent
     (Object : in out Base.Base_Type'Class)
      return View.View_Access
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      return App.Content_Text'Access;
   end Content_Parent;

   procedure Content_Clear (Object : in out Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Text.Inner_HTML ("");
   end Content_Clear;

   procedure Content_Set_Title
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Header.Text (Title);
   end Content_Set_Title;

   procedure Content_Clear_Title (Object : in out Base.Base_Type'Class) is
   begin
      Content_Set_Title (Object, "");
   end Content_Clear_Title;

   procedure Content_Set_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Content_Text.Text (Text);
   end Content_Set_Text;

   procedure Content_Clear_Text (Object : in out Base.Base_Type'Class) is
   begin
      Content_Set_Text (Object, "");
   end Content_Clear_Text;

   --------------
   --  Groups  --
   --------------
   procedure Content_Group_Create
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString)
   is
      App    : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent : constant Element.Pointer_To_Element_Class := new Element.Form.Form_Type;

      Table_Element : constant Element.Pointer_To_Element_Class := new Element.Table.Table_Type;
      Table         : constant Element.Table.Table_Access       := Element.Table.Table_Access (Table_Element);

      Row  : constant Element.Table.Table_Row_Access    := new Element.Table.Table_Row_Type;
      Data : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;
   begin
      Element.Form.Form_Access (Parent).Create (App.Content_Text);
      Parent.Class_Name ("content-group");
      App.Content_Text.Add_Element (Title, Parent);

      Table.Dynamic;
      Table.Create (Parent.all);
      Table.Style ("width", "100%");
      App.Content_Text.Add_Element ("Table_" & Title, Table_Element);

      Row.Create (Table.all);
      Data.Create (Row.all, Content => Title, Column_Span => 2);
      Data.Class_Name ("content-group-header");
   end Content_Group_Create;

   procedure Content_Group_Add_Title
     (Object     : in out Base.Base_Type'Class;
      Title      :        UXString;
      Parent_Key :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Table_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table         : constant Element.Table.Table_Access       := Element.Table.Table_Access (Table_Element);

      Row  : constant Element.Table.Table_Row_Access    := new Element.Table.Table_Row_Type;
      Data : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;

      Span : constant Element.Common.Span_Access := new Element.Common.Span_Type;
   begin
      Content_Group_Add_Space (Object, Parent_Key, 8);
      Row.Create (Table.all);
      Data.Create (Row.all, Column_Span => 2);
      Data.Style ("text-align", "center");
      Span.Create (Data.all, Title);
      Span.Class_Name ("content-group-title");
   end Content_Group_Add_Title;

   procedure Content_Group_Add_Space
     (Object     : in out Base.Base_Type'Class;
      Parent_Key :        UXString;
      Height     :        Integer := 8)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Table_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table         : constant Element.Table.Table_Access       := Element.Table.Table_Access (Table_Element);

      Row : constant Element.Table.Table_Row_Access := new Element.Table.Table_Row_Type;
   begin
      Row.Create (Table.all);
      Row.Style ("height", From_UTF_8 (Height'Image) & "px");
   end Content_Group_Add_Space;

   procedure Content_Group_Item_Add
     (Object     : in out Base.Base_Type'Class;
      Item       :        Element.Pointer_To_Element_Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App           : constant App_Access                       := App_Access (Object.Connection_Data);
      Table_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table         : constant Element.Table.Table_Access       := Element.Table.Table_Access (Table_Element);

      Row           : constant Element.Table.Table_Row_Access    := new Element.Table.Table_Row_Type;
      First_Column  : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;
      Second_Column : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;
   begin
      Row.Create (Table.all);
      First_Column.Create (Row.all, Name);
      Second_Column.Create (Row.all);
      Second_Column.Style ("white-space", "nowrap");

      Item.Place_Inside_Top_Of (Second_Column.all);
      Item.Style ("width", "100%");
      Item.Style ("box-sizing", "border-box");
      Item.On_Focus_In_Handler (Framework.CRUD_Disable_Shortcuts'Unrestricted_Access);
      Item.On_Focus_Out_Handler (Framework.CRUD_Enable_Shortcuts'Unrestricted_Access);
      Item.On_Change_Handler (On_Change);

      --  Not its true parent, but a lot easier to access
      App.Content_Text.Add_Element (Name, Item);
   end Content_Group_Item_Add;

   procedure Content_Group_Item_Lock
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
   is
      App          : constant App_Access                       := App_Access (Object.Connection_Data);
      Form_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Form         : constant Element.Form.Form_Element_Access := Element.Form.Form_Element_Access (Form_Element);
   begin
      Form.Disabled;
   end Content_Group_Item_Lock;

   procedure Content_Group_Item_Unlock
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
   is
      App          : constant App_Access                       := App_Access (Object.Connection_Data);
      Form_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Form         : constant Element.Form.Form_Element_Access := Element.Form.Form_Element_Access (Form_Element);
   begin
      Form.Disabled (False);
   end Content_Group_Item_Unlock;

   procedure Content_Group_Item_Place_Holder
     (Object       : in out Base.Base_Type'Class;
      Name         :        UXString;
      Place_Holder :        UXString)
   is
      App          : constant App_Access                       := App_Access (Object.Connection_Data);
      Form_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Form         : constant Element.Form.Text_Area_Access    := Element.Form.Text_Area_Access (Form_Element);
   begin
      Form.Place_Holder (Place_Holder);
   end Content_Group_Item_Place_Holder;

   -----------------
   --  Edit Text  --
   -----------------
   procedure Content_Group_Text_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Edit_Text      : constant Element.Pointer_To_Element_Class := new Element.Form.Text_Type;
   begin
      Element.Form.Text_Access (Edit_Text).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Edit_Text, Name, Parent_Key, On_Change);
   end Content_Group_Text_Add;

   procedure Content_Group_Text_Set
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString;
      Text   :        UXString)
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Edit_Text_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Edit_Text         : constant Element.Form.Text_Access         := Element.Form.Text_Access (Edit_Text_Element);
   begin
      Edit_Text.Value (Text);
   end Content_Group_Text_Set;

   function Content_Group_Text_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Edit_Text_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Edit_Text         : constant Element.Form.Text_Access         := Element.Form.Text_Access (Edit_Text_Element);
   begin
      return Edit_Text.Value;
   end Content_Group_Text_Get;

   -----------------
   --  Text Area  --
   -----------------
   procedure Content_Group_Text_Area_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Text_Area      : constant Element.Pointer_To_Element_Class := new Element.Form.Text_Area_Type;
   begin
      Element.Form.Text_Area_Access (Text_Area).Create (Form => Parent.all, Name => Name);
      Text_Area.Style ("resize", "vertical");
      Text_Area.Style ("height", "38px");
      Text_Area.Style ("min-height", "38px");
      Content_Group_Item_Add (Object, Text_Area, Name, Parent_Key, On_Change);
   end Content_Group_Text_Area_Add;

   procedure Content_Group_Text_Area_Set
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString;
      Text   :        UXString)
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Text_Area_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Text_Area         : constant Element.Form.Text_Area_Access := Element.Form.Text_Area_Access (Text_Area_Element);
   begin
      Text_Area.Value (Text);
   end Content_Group_Text_Area_Set;

   function Content_Group_Text_Area_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Text_Area_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Text_Area         : constant Element.Form.Text_Area_Access := Element.Form.Text_Area_Access (Text_Area_Element);
   begin
      return Text_Area.Value;
   end Content_Group_Text_Area_Get;

   -----------------
   --  Check Box  --
   -----------------
   procedure Content_Group_Check_Box_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Check_Box      : constant Element.Pointer_To_Element_Class := new Element.Form.Check_Box_Type;
   begin
      Element.Form.Check_Box_Access (Check_Box).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Check_Box, Name, Parent_Key, On_Change);
   end Content_Group_Check_Box_Add;

   procedure Content_Group_Check_Box_Checked
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Is_Checked :        Boolean)
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Check_Box_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Check_Box         : constant Element.Form.Check_Box_Access := Element.Form.Check_Box_Access (Check_Box_Element);
   begin
      Check_Box.Checked (Is_Checked);
   end Content_Group_Check_Box_Checked;

   function Content_Group_Check_Box_Is_Checked
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return Boolean
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Check_Box_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Check_Box         : constant Element.Form.Check_Box_Access := Element.Form.Check_Box_Access (Check_Box_Element);
   begin
      return Check_Box.Checked;
   end Content_Group_Check_Box_Is_Checked;

   --------------
   --  Number  --
   --------------
   procedure Content_Group_Number_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Number         : constant Element.Pointer_To_Element_Class := new Element.Form.Number_Type;
   begin
      Element.Form.Number_Access (Number).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Number, Name, Parent_Key, On_Change);
   end Content_Group_Number_Add;

   procedure Content_Group_Number_Set
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString;
      Value  :        Integer)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Number_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Number         : constant Element.Form.Number_Access       := Element.Form.Number_Access (Number_Element);
   begin
      Number.Value (Value);
   end Content_Group_Number_Set;

   function Content_Group_Number_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return Integer
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Number_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Number         : constant Element.Form.Number_Access       := Element.Form.Number_Access (Number_Element);
   begin
      return Number.Value;
   end Content_Group_Number_Get;

   -----------------
   --  Selection  --
   -----------------
   procedure Content_Group_Selection_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Selection      : constant Element.Pointer_To_Element_Class := new Element.Form.Selection_Type;
   begin
      Element.Form.Selection_Access (Selection).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Selection, Name, Parent_Key, On_Change);
   end Content_Group_Selection_Add;

   procedure Content_Group_Selection_Add_Option
     (Object  : in out Base.Base_Type'Class;
      Name    :        UXString;
      Option  :        UXString;
      Enabled :        Boolean := False)
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Selection_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Selection         : constant Element.Form.Selection_Access := Element.Form.Selection_Access (Selection_Element);
   begin
      Selection.Add_Option (Value => Option, Text => Option, Selected => Enabled);
   end Content_Group_Selection_Add_Option;

   function Content_Group_Selection_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString
   is
      App               : constant App_Access                       := App_Access (Object.Connection_Data);
      Selection_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Selection         : constant Element.Form.Selection_Access := Element.Form.Selection_Access (Selection_Element);
   begin
      return Selection.Value;
   end Content_Group_Selection_Get;

   ------------
   --  Date  --
   ------------
   procedure Content_Group_Date_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Date           : constant Element.Pointer_To_Element_Class := new Element.Form.Date_Type;
   begin
      Element.Form.Date_Access (Date).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Date, Name, Parent_Key, On_Change);
   end Content_Group_Date_Add;

   function Content_Group_Date_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString
   is
      App          : constant App_Access                       := App_Access (Object.Connection_Data);
      Date_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Date         : constant Element.Form.Date_Access         := Element.Form.Date_Access (Date_Element);
   begin
      return Date.Value;
   end Content_Group_Date_Get;

   -------------
   --  Email  --
   -------------
   procedure Content_Group_Email_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Email          : constant Element.Pointer_To_Element_Class := new Element.Form.Email_Type;
   begin
      Element.Form.Email_Access (Email).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Email, Name, Parent_Key, On_Change);
   end Content_Group_Email_Add;

   function Content_Group_Email_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString
   is
      App           : constant App_Access                       := App_Access (Object.Connection_Data);
      Email_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Email         : constant Element.Form.Email_Access        := Element.Form.Email_Access (Email_Element);
   begin
      return Email.Value;
   end Content_Group_Email_Get;

   ----------------
   --  Password  --
   ----------------
   procedure Content_Group_Password_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Password       : constant Element.Pointer_To_Element_Class := new Element.Form.Password_Type;
   begin
      Element.Form.Password_Access (Password).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Password, Name, Parent_Key, On_Change);
   end Content_Group_Password_Add;

   function Content_Group_Password_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString
   is
      App              : constant App_Access                       := App_Access (Object.Connection_Data);
      Password_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Password         : constant Element.Form.Password_Access     := Element.Form.Password_Access (Password_Element);
   begin
      return Password.Value;
   end Content_Group_Password_Get;

   -------------
   --  Phone  --
   -------------
   procedure Content_Group_Phone_Add
     (Object     : in out Base.Base_Type'Class;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Change  :        Base.Action_Event := null)
   is
      App            : constant App_Access                       := App_Access (Object.Connection_Data);
      Parent_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Parent_Key);
      Parent         : constant Element.Form.Form_Access         := Element.Form.Form_Access (Parent_Element);
      Tel            : constant Element.Pointer_To_Element_Class := new Element.Form.Tel_Type;
   begin
      Element.Form.Tel_Access (Tel).Create (Form => Parent.all, Name => Name);
      Content_Group_Item_Add (Object, Tel, Name, Parent_Key, On_Change);
   end Content_Group_Phone_Add;

   function Content_Group_Phone_Get
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString)
      return UXString
   is
      App         : constant App_Access                       := App_Access (Object.Connection_Data);
      Tel_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Name);
      Tel         : constant Element.Form.Tel_Access          := Element.Form.Tel_Access (Tel_Element);
   begin
      return Tel.Value;
   end Content_Group_Phone_Get;

   ---------------
   --  Warning  --
   ---------------
   procedure Content_Group_Warning_Add
     (Object     : in out Base.Base_Type'Class;
      Text       :        UXString;
      Key        :        UXString;
      Parent_Key :        UXString)
   is
      App : constant App_Access := App_Access (Object.Connection_Data);

      Table_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element ("Table_" & Parent_Key);
      Table         : constant Element.Table.Table_Access       := Element.Table.Table_Access (Table_Element);

      Row  : constant Element.Table.Table_Row_Access    := new Element.Table.Table_Row_Type;
      Data : constant Element.Table.Table_Column_Access := new Element.Table.Table_Column_Type;

      Span_Element : constant Element.Pointer_To_Element_Class := new Element.Common.Span_Type;
      Span         : constant Element.Common.Span_Access       := Element.Common.Span_Access (Span_Element);
   begin
      Row.Create (Table.all);
      Data.Create (Row.all, Column_Span => 2);
      Data.Style ("text-align", "center");
      Span.Create (Data.all, Text);
      Span.Class_Name ("content-group-warning");

      App.Content_Text.Add_Element (Key, Span_Element);
   end Content_Group_Warning_Add;

   procedure Content_Group_Warning_Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Text   :        UXString)
   is
      App          : constant App_Access                       := App_Access (Object.Connection_Data);
      Span_Element : constant Element.Pointer_To_Element_Class := App.Content_Text.Element (Key);
      Span         : constant Element.Common.Span_Access       := Element.Common.Span_Access (Span_Element);
   begin
      Span.Text (Text);
   end Content_Group_Warning_Set;

   -----------------------------------------------------------------------------
   --  Footer
   -----------------------------------------------------------------------------

   procedure Footer_Set_State_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "")
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Footer_Content.Set_State_Text (Text);
   end Footer_Set_State_Text;

   procedure Footer_Set_Permanent_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "")
   is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      App.Footer_Content.Set_Permanent_Text (Text);
   end Footer_Set_Permanent_Text;

   -----------------------------------------------------------------------------
   --  User relative data
   -----------------------------------------------------------------------------

   procedure Set
     (Identity : in out User_Data;
      Key      :        UXString;
      Value    :        UXString)
   is
   begin
      Identity.Extra.Insert (Key, Value);
   end Set;

   function Get
     (Identity : in out User_Data;
      Key      :        UXString)
      return UXString
   is
   begin
      return Identity.Extra.Element (Key);
   end Get;

   procedure Identity_Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Value  :        UXString)
   is
      App      : constant App_Access := App_Access (Object.Connection_Data);
      Identity : User_Data           := Identities.Element (App.Email);
   begin
      Identity.Extra.Insert (Key, Value);
   end Identity_Set;

   function Identity_Get
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
      return UXString
   is
      App      : constant App_Access := App_Access (Object.Connection_Data);
      Identity : constant User_Data  := Identities.Element (App.Email);
   begin
      return Identity.Extra.Element (Key);
   end Identity_Get;

   -----------------------------------------------------------------------------
end Framework;
