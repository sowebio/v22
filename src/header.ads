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

   function Set_Root (Name : UXString; On_Open : Base.Action_Event) return Integer;
   --  Set first element in breadcrumb, first button to appear
   --  NOTE: Root must be created before childs, and returned value
   --  (Parent_ID) allows to create childs for this element

   function Add_Child
     (Parent_ID : Integer;
      Name      : UXString;
      On_Open   : Base.Action_Event)
      return Integer;
   --  Add child for a given parent
   --  NOTE: A child always refers to a parent, which can be itself a child...

   -----------------------------------------------------------------------------
   --  Menu relative functions
   -----------------------------------------------------------------------------

   --  Breadcrumb navigation menu
   procedure Open_Menu (Instance  : in out Header_Type; Unique_ID : Integer);

   procedure Close_Menu (Instance : in out Header_Type);

   function Is_Menu_Open (Instance : in out Header_Type) return Boolean;

   --  User menu
   procedure Open_User_Menu (Instance : in out Header_Type);

   procedure Close_User_Menu (Instance : in out Header_Type);

   function Is_User_Menu_Open (Instance : in out Header_Type) return Boolean;

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   procedure Notify_Menu_Click (Instance : in out Header_Type; Unique_ID : Integer);
   --  Callback to place in "On_Open" handlers (given to Add_Child)

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------

   procedure Set_Menu (Instance : in out Header_Type; Unique_ID : Integer);
   --  Set current menu, updating breadcrumb

   procedure Set_User_Name (Instance : in out Header_Type; User_Name : UXString);

   procedure Set_App_Icon (Instance : in out Header_Type; Icon_SRC : UXString);

   procedure Set_User_Icon (Instance : in out Header_Type; Icon_SRC : UXString);

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
   --  Function to create a button on the user menu, creating a jQuery dialog.
   --  Two default buttons are available : Cancel and Confirm
   --  Buttons are not displayed on dialog if corresponding text is empty

   procedure Add_Web (Title : UXString; URL : UXString);
   --  Function to create a button on the user menu, which opens a new web page

private

end Header;
