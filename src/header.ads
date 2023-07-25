with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings;         use UXStrings;
with Breadcrumb;

package Header is

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;

   type Header_Data is tagged record
      Parent : View.View_Access;
      Icon   : Common.IMG_Access;

      App_Is_Open  : Boolean := False;
      User_Is_Open : Boolean := False;

      App_Parent        : View.View_Access;
      App_Icon          : Common.IMG_Access;
      App_Browse_Parent : View.View_Access;

      Breadcrumb_Parent  : View.View_Access;
      Breadcrumb_Content : Breadcrumb.Breadcrumb_Type;

      User_Parent        : View.View_Access;
      User_Name          : Common.P_Access;
      User_Icon          : Common.IMG_Access;
      User_Browse_Parent : View.View_Access;
   end record;
   type Header_Type is new Header_Data with null record;

   procedure Create
     (Instance         : in out Header_Type;
      Parent           : in out View.View_Type;
      On_Logo, On_User :        Base.Action_Event);
   --  Should be called every time a user connects

   function Set_Root
     (Name    : UXString;
      On_Open : Base.Action_Event)
      return Integer;

   function Add_Child
     (Parent_Id : Integer;
      Name      : UXString;
      On_Open   : Base.Action_Event)
      return Integer;

   -----------------------------------------------------------------------------
   --  Menu relative functions
   -----------------------------------------------------------------------------

   procedure Open_Menu
     (Instance  : in out Header_Type;
      Unique_Id :        Integer);

   procedure Close_Menu (Instance : in out Header_Type);

   function Is_Menu_Open
     (Instance : in out Header_Type)
      return Boolean;

   procedure Open_User_Menu (Instance : in out Header_Type);

   procedure Close_User_Menu (Instance : in out Header_Type);

   function Is_User_Menu_Open
     (Instance : in out Header_Type)
      return Boolean;

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   procedure Notify_Menu_Click
     (Instance  : in out Header_Type;
      Unique_Id :        Integer);

   procedure Notify_User_Menu_Click (Instance : in out Header_Type);

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------

   procedure Set_Menu
     (Instance  : in out Header_Type;
      Unique_Id :        Integer);

   procedure Set_User_Name
     (Instance  : in out Header_Type;
      User_Name :        UXString);

   procedure Set_App_Icon
     (Instance : in out Header_Type;
      Icon_SRC :        UXString);

   procedure Set_User_Icon
     (Instance : in out Header_Type;
      Icon_SRC :        UXString);

   -----------------------------------------------------------------------------
   --  User Menu
   -----------------------------------------------------------------------------

   procedure Add_Dialog
     (Title           : UXString;
      Content         : UXString          := "";
      Confirm_Text    : UXString          := "";
      Cancel_Text     : UXString          := "";
      Confirm_Handler : Base.Action_Event := null;
      Cancel_Handler  : Base.Action_Event := null);

   procedure Add_Web
     (Title : UXString;
      URL   : UXString);

private

end Header;
