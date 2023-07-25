with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings;         use UXStrings;

package CRUD is

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;

   type Crud_Type is tagged private;

   --  NOTE: Given identifiers for elements and sub-elements can be used to
   --  create callbacks

   procedure Create
     (Instance  : in out Crud_Type;
      Parent    : in out View.View_Type;
      On_Resize :        Base.Action_Event;
      On_Click  :        Base.Action_Event);
   --  Should be called every time a user connects
   --  NOTE: On_Resize and On_Click allows to access user-defined
   --  connection data thus allowing access to current Instance...

   procedure Load (Instance : in out Crud_Type);
   --  Load CRUD on screen

   procedure Clear (Instance : in out Crud_Type);
   --  Clear CRUD from screen

   function Add_Element
     (Instance : in out Crud_Type;
      Name     :        UXString;
      Icon_SRC :        UXString)
      return Integer;
   --  Add element visible directly on screen, which comes with an icon
   --  NOTE: Elements must be created before sub-elements, and return value
   --  (Parent_Id) allows to create sub-elements for this element

   function Add_Sub_Element
     (Instance  : in out Crud_Type;
      Name      :        UXString;
      Parent_Id :        Integer;
      Handler   :        Base.Action_Event := null)
      return Integer;
   --  Add sub-element, accessible only when the user clicks on an element
   --  created with Add_Element

   procedure Add_Delimiter_Above
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer);
   --  Add a delimiter above the sub-element with "Unique_Id" identifier

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------
   procedure Set_Unclickable
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer);
   --  Set an element or sub-element unclickable and unaccessible using
   --  keyboard shortcuts

   procedure Set_Clickable
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer);
   --  Set an element or sub-element clickable again, working before and after
   --  load, assuming CRUD was built with Add_Element and Add_Sub_Element

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   --  NOTE: These callbacks allow to use user-defined connection data thus
   --  CRUD instance can be accessed (instead of only an Object type)

   procedure Notify_Element_Click
     (Instance : in out Crud_Type;
      Object   : in out Base.Base_Type'Class);
   --  Callback to place in elements' handlers

   procedure Notify_Sub_Element_Click
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer);
   --  Callback to place in sub-elements' handlers

   procedure Notify_Key_Pressed
     (Instance : in out Crud_Type;
      Key      :        Character);
   --  Callback to place in window's user-defined Character_Handler callback

   procedure Notify_Resize (Instance : in out Crud_Type);
   --  Callback to place in On_Resize, argument of Create procedure

private

   Root_Parent_Id      : constant Integer           := -1;
   Force_Shortcut_Char : constant Unicode_Character := '~';

   type Data_Type is record
      Parent_Id : Integer  := Root_Parent_Id;
      Icon_SRC  : UXString := "";
      Name      : UXString;
      Clickable : Boolean  := True;

      Delimiter_Above : Boolean := False;

      Handler     : Base.Action_Event;
      Shortcut_Id : Integer;
   end record;

   Max_Menu_Amount : constant Integer := 50;
   type Menu_Array is array (1 .. Max_Menu_Amount) of Data_Type;

   type Boolean_Array is array (Integer range <>) of Boolean;

   type Crud_Type is tagged record
      Active_Shortcuts : Boolean_Array (1 .. 26)             := (others => False);
      Active_Menu      : Boolean_Array (1 .. Max_Menu_Amount) := (others => False);
      Menu_Table       : Menu_Array;
      Next_Id          : Integer                             := 1;

      Current_Root : Integer := Root_Parent_Id;
      Is_Opened    : Boolean := False;

      On_Click : Base.Action_Event;

      Parent              : View.View_Access;
      Sub_Elements_Parent : View.View_Access;
      Elements_Parent     : View.View_Access;

      Extend_Shrink_Button : Common.Button_Access;
      Is_Expanded          : Boolean := False;
   end record;

end CRUD;
