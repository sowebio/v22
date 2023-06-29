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
   type Browse_Type is new Gnoga.Types.Connection_Data_Type with record
      Contract : Gnoga.Gui.Element.Common.Button_Type;
      Administration : Gnoga.Gui.Element.Common.Button_Type;
      Contract_Management : Gnoga.Gui.Element.Common.Button_Type;
      Administration_Users : Gnoga.Gui.Element.Common.Button_Type;
      Administration_Emails : Gnoga.Gui.Element.Common.Button_Type;
      Administration_Gen : Gnoga.Gui.Element.Common.Button_Type;
   end record;

   --  Accordion Left
   type Menu_Accordion_Type is new Gnoga.Types.Connection_Data_Type with record
      Accordion : aliased Gnoga.Gui.Plugin.jQueryUI.Widget.Accordion_Type;
      Cards     : aliased Gnoga.Gui.View.Card.Card_View_Type;

      Name_Accueil        : UXString := "Name_Accueil_1";
      Name_Contrats       : UXString := "Name_Contrats";
      Name_Administration : UXString := "Name_Administration";

      Widget : aliased Widget_Set (1 .. 3);
      Button : aliased Button_Set (1 .. 6);
   end record;

   type Menu_Folder_Type is new Gnoga.Types.Connection_Data_Type with record
      Cards : aliased Gnoga.Gui.View.Card.Card_View_Type;

      Name_Accueil          : UXString := "Name_Accueil_2";
      Name_Standard         : UXString := "Name_Standard";
      Name_Contrats_Gestion : UXString := "Name_Contrats_Gestion_2";

      Widget : aliased Widget_Set (1 .. 3);
   end record;

   type Main_Frame_Type is new Gnoga.Types.Connection_Data_Type with record
      Main_Deck : aliased Gnoga.Gui.View.Docker.Docker_View_Type;
      Cards     : aliased Gnoga.Gui.View.Card.Card_View_Type;

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

      Button : aliased Button_Set (1 .. 5);
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
      Window : Gnoga.Gui.Window.Pointer_To_Window_Class;
      View   : aliased Gnoga.Gui.View.View_Type;

      Navigation_Bar : Gnoga.Gui.Element.Common.DIV_Type;
      Tool_Bar       : Gnoga.Gui.Element.Common.DIV_Type;
      Content        : Gnoga.Gui.Element.Common.DIV_Type;
      Bottom_Bar     : Gnoga.Gui.Element.Common.DIV_Type;

      Status    : Gnoga.Gui.Element.Common.DIV_Type;
      Permanent : Gnoga.Gui.Element.Common.DIV_Type;

      App_Icon : Gnoga.Gui.Element.Common.IMG_Type;

      User_Button : Gnoga.Gui.Element.Common.DIV_Type;
      User_Icon   : Gnoga.Gui.Element.Common.IMG_Type;
      User_Name   : Gnoga.Gui.Element.Common.P_Type;

      Navigation_App : Gnoga.Gui.Element.Common.DIV_Type;
      Navigation_Breadcrumb : Gnoga.Gui.View.View_Type;
      Navigation_User : Gnoga.Gui.Element.Common.DIV_Type;

      Tool_Browse : Gnoga.Gui.Element.Common.DIV_Type;
      Browse : aliased Browse_Type;

      Tool_Edit : Gnoga.Gui.Element.Common.DIV_Type;

      Exit_Button : Gnoga.Gui.Element.Common.Button_Type;

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
   procedure Hide_Browse_Buttons (App : App_Access) is
   begin
      App.Browse.Contract.Display ("none");
      App.Browse.Administration.Display ("none");
      App.Browse.Contract_Management.Display ("none");
      App.Browse.Administration_Users.Display ("none");
      App.Browse.Administration_Emails.Display ("none");
      App.Browse.Administration_Gen.Display ("none");
   end Hide_Browse_Buttons;

   procedure On_Contract_Management (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      Hide_Browse_Buttons (App);
      Current_Depth := Breadcrumb.Update_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Contract_Management'Unrestricted_Access, Content => "Gestion",
         Current_Depth => Current_Depth, Depth => 2);
   end On_Contract_Management;

   procedure On_Administration_Users (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      Hide_Browse_Buttons (App);
      Current_Depth := Breadcrumb.Update_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Administration_Users'Unrestricted_Access, Content => "Utilisateurs",
         Current_Depth => Current_Depth, Depth => 2);
   end On_Administration_Users;

   procedure On_Administration_Emails (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      Hide_Browse_Buttons (App);
      Current_Depth := Breadcrumb.Update_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Administration_Emails'Unrestricted_Access, Content => "Emails",
         Current_Depth => Current_Depth, Depth => 2);
   end On_Administration_Emails;

   procedure On_Administration_Gen (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
      pragma Unreferenced (Object);
   begin
      Hide_Browse_Buttons (App);
      Current_Depth := Breadcrumb.Update_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Administration_Gen'Unrestricted_Access, Content => "Gen. Requêtes",
         Current_Depth => Current_Depth, Depth => 2);
   end On_Administration_Gen;

   procedure On_Contract (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Hide_Browse_Buttons (App);
      App.Browse.Contract_Management.Display ("inherit");
      Current_Depth := Breadcrumb.Update_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Contract'Unrestricted_Access, Content => "Contrats",
         Current_Depth => Current_Depth, Depth => 1);
   end On_Contract;

   procedure On_Administration (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Hide_Browse_Buttons (App);
      App.Browse.Administration_Users.Display ("inherit");
      App.Browse.Administration_Emails.Display ("inherit");
      App.Browse.Administration_Gen.Display ("inherit");
      Current_Depth := Breadcrumb.Update_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Administration'Unrestricted_Access, Content => "Administration",
         Current_Depth => Current_Depth, Depth => 1);
   end On_Administration;

   procedure On_Main (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      App : constant App_Access := App_Access (Object.Connection_Data);
   begin
      Hide_Browse_Buttons (App);
      App.Browse.Contract.Display ("inherit");
      App.Browse.Administration.Display ("inherit");
      Current_Depth := Breadcrumb.Update_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Main'Unrestricted_Access, Content => "Accueil",
         Current_Depth => Current_Depth, Depth => 0);
   end On_Main;

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
      App.Content.Put_HTML ("<p>Lorem ipsum dolor sit amet. Aut consequatur ipsam eos inventore repellat et neque sint id tempora aliquid eos assumenda ullam ut quas nostrum. Et eveniet recusandae ut totam voluptatem ut nihil asperiores. Aut laudantium maxime aut suscipit maiores et voluptates internos ab autem beatae aut sunt eveniet. Ut nesciunt consequatur sed quos rerum et illo maxime? Cum facilis quod et cumque voluptatem non totam Quis non rerum error ad architecto illo in autem suscipit. Et dolorum distinctio aut inventore consectetur sed natus fugiat? Id laudantium aspernatur id quisquam consequatur non molestiae aliquam et perspiciatis molestias? Id dolorem exercitationem hic pariatur voluptatem sed enim autem aut officiis omnis ut voluptatem vero sed magni expedita id iste libero. Aut omnis vero aut quia quasi aut internos eaque et quaerat facilis. Est corporis dolorem ut quia debitis hic alias voluptatum et molestias sint. Ut voluptatem optio a quam corporis et incidunt libero vel aspernatur ratione non recusandae maxime ex consectetur alias. Non dolorem doloribus ea praesentium beatae eum molestiae dolorem eos minima officiis vel voluptates quod sit debitis quia. Quo dolore delectus non perspiciatis recusandae eos quod pariatur qui dolore labore. Eum accusantium beatae est accusamus quidem est voluptatum inventore! </p><p>Aut facere delectus qui voluptatum exercitationem qui earum porro. Est galisum excepturi ex porro suscipit sit beatae voluptatem aut pariatur animi rem eaque ipsam aut omnis architecto nam reiciendis consequatur. Ut exercitationem Quis ea cupiditate veritatis non ipsam eligendi. Et cupiditate molestias et vero dolorem ut dicta voluptatem et atque fuga est nihil aperiam! Qui voluptatem dolore ut dolore exercitationem vel magni harum qui eligendi temporibus sit voluptatum perferendis. Non nulla esse 33 totam sunt et corrupti earum in mollitia odio sed ipsum architecto quo repellendus vero. Et soluta sint est explicabo aliquid rem expedita quasi et corrupti dolor rem fugiat quos. Ut galisum consectetur ut illo expedita in accusamus voluptate a galisum corrupti aut similique possimus aut omnis adipisci. Qui pariatur amet eum voluptas assumenda eum tempore nihil? Ea facere architecto ut harum autem vel sunt fuga aut aspernatur quae ut enim similique. Ut Quis quas ea voluptas culpa sit adipisci earum et sint mollitia qui omnis consequatur. </p><p>Ut cumque reprehenderit aut accusamus esse et iusto modi ut maiores suscipit. Aut fugiat temporibus et porro omnis est tenetur velit cum fugit expedita! Vel Quis aliquid et nihil eligendi eum tempore repellat est odit consectetur aut velit dolores. Sed earum quia est fuga beatae sit reiciendis quasi qui nobis sint et accusamus voluptate aut porro quis. Et voluptates exercitationem quo nostrum ducimus et quod voluptatem aut culpa atque. Qui accusantium consectetur sit blanditiis enim ut quaerat natus id possimus quae non nisi quod qui velit inventore ea excepturi autem. Cum repudiandae dolorem aut exercitationem unde sed quia modi. Qui debitis minima est facilis deserunt aut dolor laborum At voluptas consectetur et Quis laboriosam sed ducimus voluptatibus id internos voluptate. Ut laborum deleniti et esse omnis eos aspernatur modi et voluptatem necessitatibus ab reprehenderit repellendus. Et veritatis ipsam vel culpa laborum ut maxime omnis 33 galisum dignissimos sed sint rerum qui voluptatem dolore! 33 similique nulla ut libero facere id deserunt odio non molestias accusantium sed facilis cupiditate id quas soluta rem error odit. </p><p>Est amet consequatur et eaque explicabo ex doloribus deleniti est voluptates labore. Non ullam incidunt eos necessitatibus quod a officia velit ut labore voluptatem cum eveniet placeat qui voluptatem voluptatum At sunt expedita. Et autem doloribus At dolorem dolorum et aperiam excepturi. Qui repellendus galisum cum aperiam velit ex molestiae dolores. Aut doloribus illo eum voluptas enim est maxime rerum ea magni sunt qui labore beatae et nostrum accusamus. Est soluta voluptatem est nisi fuga ad quasi omnis rem dolore accusamus quo rerum quisquam sed quae molestiae. Qui laboriosam molestiae aut quidem iste cum alias facere non doloremque culpa. Qui vitae sint vel earum distinctio est ullam officiis eum repellendus error. Ut officiis adipisci et veniam eius vel corporis nisi quo dignissimos dolore quo sunt eius. Aut adipisci reiciendis sed sequi ipsum et dolores porro sit nisi molestias sit sunt culpa aut voluptas quibusdam. Quo corporis omnis et reiciendis debitis est harum repellendus aut dolorem voluptas eum dolor rerum et ullam voluptatum. A vero iste non ducimus consequatur sit nihil praesentium eos voluptate ipsum ea sunt cupiditate aut assumenda odit. Ea labore laudantium in consequatur corporis est internos saepe et sint architecto rem necessitatibus expedita. Sit error voluptas est eius aspernatur ut deleniti illo? </p><p>In dolorem quis et unde pariatur aut sint consequatur in exercitationem galisum 33 consequatur minus quo iure nulla. Qui magni natus eos enim mollitia non internos numquam est quis ducimus qui explicabo galisum. Est laudantium iste et autem magni in minima possimus in amet doloremque quo reiciendis corporis. Qui error sapiente in optio facere in tempora omnis. Qui eius corrupti eos quas dolorum non fuga consequatur. Quo commodi omnis ea fuga similique qui velit dolorum. Sit beatae necessitatibus sit nulla dolor ut commodi consequuntur aut voluptatem numquam. Sit inventore cupiditate eum velit Quis et sint aspernatur et veritatis eius et consectetur neque est debitis possimus. </p><p>Vel galisum doloribus et fuga delectus non recusandae laudantium qui temporibus eius. Aut magnam tempora qui nulla necessitatibus ut voluptate repudiandae non tenetur velit aut natus neque vel dolorum animi. Et quaerat architecto et quae quos sed rerum quae qui omnis voluptatem? Sit rerum distinctio et necessitatibus ipsa hic officia numquam. Sed fugiat repudiandae est amet voluptatem 33 cupiditate rerum a expedita quibusdam. Et soluta quasi qui magnam neque non exercitationem earum ut consequuntur atque? Eos soluta quibusdam et voluptatibus enim rem numquam galisum id corrupti sequi qui nihil galisum eum reiciendis velit ut perferendis atque? Et consequatur laborum eum dolor sint ut maiores dolores et reiciendis quia et nisi itaque et assumenda consectetur qui quasi totam. </p><p>Aut impedit aspernatur quo dolores veritatis nam amet reiciendis et expedita rerum hic corporis esse! Ut ducimus quam ad deleniti fugit id internos neque quo dolor nisi ea facilis autem aut nulla natus. Qui nihil neque et dolorum velit eos illo voluptatem et recusandae quis et harum dolore aut iste autem non laudantium eveniet. Sed delectus voluptates et voluptatibus necessitatibus ut ullam dicta ut tenetur atque qui culpa atque. Aut Quis explicabo in pariatur delectus et tenetur modi hic dolores sint vel reiciendis veritatis aut pariatur similique et soluta omnis! Aut eligendi cupiditate et voluptatem dolore qui facilis tempore ut obcaecati itaque qui consequatur sint. Et cupiditate repellendus hic quibusdam optio est neque dolores sit odit nostrum et odio dolor. Ab reiciendis vero et tempore officiis est architecto similique sit sequi voluptatibus et reprehenderit sequi et temporibus labore eum cupiditate natus? Quo accusamus officiis id autem sunt eum deserunt omnis non atque minima non aperiam temporibus et cumque alias! Vel laborum distinctio ut rerum voluptatem qui obcaecati neque ut delectus dignissimos. Sed consequatur deleniti et accusantium asperiores et galisum inventore qui rerum dolores At esse magnam ab perspiciatis adipisci eos quisquam veritatis. </p>");

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
      App.Navigation_App.On_Click_Handler (On_Main'Unrestricted_Access);
      App.App_Icon.Create (App.Navigation_App, URL_Source => "/css/icons/home.png");
      App.App_Icon.Style ("width", "40px");
      App.App_Icon.Style ("height", "40px");

      App.Navigation_Breadcrumb.Create (App.Navigation_Bar);
      App.Navigation_Breadcrumb.Class_Name ("breadcrumb-container");

      Current_Depth := 0;
      --  Doesn't reset when refresh if "Current_Depth = -1" placed before begin ???
      Current_Depth := Breadcrumb.Add_To_Breadcrumb
        (View => App.Navigation_Breadcrumb, Handler => On_Main'Unrestricted_Access, Content => "Accueil",
         Current_Depth => Current_Depth, Depth => 0);

      App.Navigation_User.Create (App.Navigation_Bar);
      App.Navigation_User.Class_Name ("user-container");
      App.User_Panel.Accordion.Create (Parent => App.Navigation_User);
      App.User_Panel.Accordion.Class_Name ("user-accordion");
      App.User_Panel.Accordion.Create_Section ("Utilisateur");
      App.User_Panel.Deck_User.Create (App.User_Panel.Accordion);
      App.User_Panel.Deck_User.Add_Class ("accordion-content");

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
      --  Tool bar
      --------------------------------------------------------------------------
      App.Tool_Browse.Create (App.Tool_Bar);
      App.Tool_Browse.Class_Name ("tool-browse");

      App.Tool_Edit.Create (App.Tool_Bar);
      App.Tool_Edit.Class_Name ("tool-edit");

      App.Browse.Contract.Create (App.Tool_Browse, "Contrats");
      App.Browse.Contract.On_Click_Handler (On_Contract'Unrestricted_Access);
      App.Browse.Administration.Create (App.Tool_Browse, "Administration");
      App.Browse.Administration.On_Click_Handler (On_Administration'Unrestricted_Access);
      App.Browse.Contract_Management.Create (App.Tool_Browse, "Gestion");
      App.Browse.Contract_Management.On_Click_Handler (On_Contract_Management'Unrestricted_Access);
      App.Browse.Administration_Users.Create (App.Tool_Browse, "Utilisateurs");
      App.Browse.Administration_Users.On_Click_Handler (On_Administration_Users'Unrestricted_Access);
      App.Browse.Administration_Emails.Create (App.Tool_Browse, "Emails");
      App.Browse.Administration_Emails.On_Click_Handler (On_Administration_Emails'Unrestricted_Access);
      App.Browse.Administration_Gen.Create (App.Tool_Browse, "Gen. Requêtes");
      App.Browse.Administration_Gen.On_Click_Handler (On_Administration_Gen'Unrestricted_Access);

      Hide_Browse_Buttons (App);
      App.Browse.Contract.Display ("inherit");
      App.Browse.Administration.Display ("inherit");

      --  --------------------------------------------------------
      --
      --  App.Stdr_Folder.Folder.Create_Folder (App.Menu_Folder.Widget (2));
      --
      --  --  --  --  Folder Fichier
      --
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
      --  App.Menu_Folder.Cards.Add_Card
      --    (Name => App.Menu_Folder.Name_Accueil, Card => App.Menu_Folder.Widget (1)'Access, Show => True);
      --
      --  App.Menu_Folder.Cards.Add_Card (Name => App.Menu_Folder.Name_Standard, Card => App.Menu_Folder.Widget (2)'Access);
      --
      --  App.Menu_Folder.Cards.Add_Card
      --    (Name => App.Menu_Folder.Name_Contrats_Gestion, Card => App.Menu_Folder.Widget (3)'Access);

      App.Exit_Button.Create (App.Content, "Stopper exécution");
      App.Exit_Button.Style ("width", "140px");
      App.Exit_Button.On_Click_Handler (On_Exit'Unrestricted_Access);

      --------------------------------------------------------------------------
      --  Main_FrameView
      --------------------------------------------------------------------------
      --  App.Main_Frame.Cards.Create (App.Main_Frame.Main_Deck);
      --  App.Main_Frame.Main_Deck.Fill_Dock (App.Main_Frame.Cards'Unchecked_Access);
      --
      --  Widget_Create_Accueil (App.Main_Frame.Widget (1), App.Main_Frame.Cards);
      --  App.Main_Frame.Cards.Add_Card
      --    (Name => App.Main_Frame.Name_Accueil, Card => App.Main_Frame.Widget (1)'Access, Show => True);
      --  --
      --  Widget_Create_Contrats_Tab (App.Main_Frame.Widget (2), App.Main_Frame.Cards);
      --  App.Main_Frame.Cards.Add_Card
      --    (Name => App.Main_Frame.Name_Contrats_Tab, Card => App.Main_Frame.Widget (2)'Access);
      --  --
      --  Widget_Create_Contrats_Gestion (App.Main_Frame.Widget (3), App.Main_Frame.Cards);
      --  App.Main_Frame.Cards.Add_Card
      --    (Name => App.Main_Frame.Name_Contrats_Gestion, Card => App.Main_Frame.Widget (3)'Access);
      --
      --  App.Form_View_Gestion.Create
      --    (Parent  => App.Main_Frame.Widget (3), ID => "",
      --     Strings => Simple_Form.All_Fields (Form_Gestion_Row_Names, ","));
      --
      --  Widget_Create_Administration_Tab (App.Main_Frame.Widget (4), App.Main_Frame.Cards);
      --  App.Main_Frame.Cards.Add_Card
      --    (Name => App.Main_Frame.Name_Administration_Tab, Card => App.Main_Frame.Widget (4)'Access);
      --
      --  Widget_Create_Administration_Utils (App.Main_Frame.Widget (5), App.Main_Frame.Cards);
      --  App.Main_Frame.Cards.Add_Card
      --    (Name => App.Main_Frame.Name_Administration_Utils, Card => App.Main_Frame.Widget (5)'Access);
      --
      --  Widget_Create_Administration_Emails (App.Main_Frame.Widget (6), App.Main_Frame.Cards);
      --  App.Main_Frame.Cards.Add_Card
      --    (Name => App.Main_Frame.Name_Administration_Emails, Card => App.Main_Frame.Widget (6)'Access);
      --
      --  Widget_Create_Administration_Gen (App.Main_Frame.Widget (7), App.Main_Frame.Cards);
      --  App.Main_Frame.Cards.Add_Card
      --    (Name => App.Main_Frame.Name_Administration_Gen, Card => App.Main_Frame.Widget (7)'Access);

      --------------------------------------------------------------------------
      --  Style
      --------------------------------------------------------------------------
      --  App.Main_Frame.Main_Deck.Style ("top", "180px");
      --  App.Main_Frame.Main_Deck.Style ("left", "0px");
      --  App.Main_Frame.Cards.Style ("position", "absolute");
      --  App.Main_Frame.Cards.Style ("top", "0px");
      --  App.Main_Frame.Cards.Style ("left", "120px");

      --  App.Menu_Folder.Cards.Add_Class ("element_top_left border");
      --  App.Menu_Folder.Cards.Style ("width", "120px");
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
