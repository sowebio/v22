with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings;         use UXStrings;

package CRUD is

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;

   type CRUD_Type is tagged private;

   procedure Create
     (Instance  : in out CRUD_Type;
      Parent    : in out View.View_Type;
      On_Resize :        Base.Action_Event;
      On_Click  :        Base.Action_Event);
   --  Should be called every time a user connects.
   --  NOTE: On_Resize and On_Click allows to access user-defined
   --  connection data thus allowing access to current Instance...

   procedure Load (Instance : in out CRUD_Type);
   --  Load CRUD on screen, necessary once CRUD is fully built with
   --  Add_Element and Add_Sub_Element.

   procedure Close_Menu (Instance : in out CRUD_Type);
   --  Closes sub-elements panel.

   procedure Clear (Instance : in out CRUD_Type);
   --  Clear every CRUD element and sub-elements from screen.

   function Add_Element
     (Instance : in out CRUD_Type;
      Name     :        UXString;
      Icon_SRC :        UXString)
      return Integer;
   --  Add element which will be visible directly on screen. Elements come
   --  with an icon.
   --  NOTE: Elements must be created before sub-elements, and returned value
   --  (Parent_ID) allows to create sub-elements for this element.

   function Add_Sub_Element
     (Instance  : in out CRUD_Type;
      Name      :        UXString;
      Parent_ID :        Integer;
      Handler   :        Base.Action_Event := null)
      return Integer;
   --  Add sub-element, accessible only when the user clicks on an element
   --  created with Add_Element.
   --  NOTE: Handler must trigger Notify_Sub_Element_Click.

   procedure Add_Delimiter_Above
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer);
   --  Add a delimiter above the sub-element with "Unique_ID" identifier.

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------
   procedure Set_Unclickable
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer);
   --  Set an element or sub-element unclickable and unaccessible using
   --  keyboard shortcuts.

   procedure Set_Clickable
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer);
   --  Set an element or sub-element clickable again, working before and after
   --  load, assuming CRUD was built with Add_Element and Add_Sub_Element.

   procedure Enable_Shortcuts (Instance : in out CRUD_Type);
   --  Enable CRUD shortcuts, allowing both elements and sub-elements shortcuts.

   procedure Disable_Shortcuts (Instance : in out CRUD_Type);
   --  Disable CRUD shortcuts, for both elements and sub-elements.

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   --  NOTE: These callbacks allow the v22.adb scope, thus accessing App_Data
   --  and thus the CRUD instance can be accessed.

   procedure Notify_Element_Click
     (Instance : in out CRUD_Type;
      Object   : in out Base.Base_Type'Class);

   procedure Notify_Sub_Element_Click
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer);
   --  Callback to place in sub-elements' handlers.

   procedure Notify_Key_Pressed
     (Instance : in out CRUD_Type;
      Key      :        Character);
   --  Callback to place in window's Character_Handler callback.

   procedure Notify_Resize (Instance : in out CRUD_Type);
   --  Callback to place in On_Resize, parameter of the Create procedure.
   --  Must be triggered when user clicks to resize CRUD panel.

private

   Root_Parent_ID      : constant Integer           := -1;
   Force_Shortcut_Char : constant Unicode_Character := '~';

   type Data_Type is record
      Parent_ID : Integer  := Root_Parent_ID;
      Icon_SRC  : UXString := "";
      HTML      : UXString;

      Clickable       : Boolean := True;
      Delimiter_Above : Boolean := False;

      Handler     : Base.Action_Event;
      Shortcut_ID : Integer;
   end record;

   Max_Menu_Amount : constant Integer := 50;
   type Menu_Array is array (1 .. Max_Menu_Amount) of Data_Type;

   type Boolean_Array is array (Integer range <>) of Boolean;

   type CRUD_Type is tagged record
      Active_Shortcuts : Boolean_Array (1 .. 26)              := (others => False);
      Active_Menu      : Boolean_Array (1 .. Max_Menu_Amount) := (others => False);
      Menu_Table       : Menu_Array;
      Last_ID          : Integer                              := 0;

      Current_Root          : Integer := Root_Parent_ID;
      Is_Opened             : Boolean := False;
      Are_Shortcuts_Enabled : Boolean := True;

      On_Click : Base.Action_Event;

      Parent              : View.View_Access;
      Sub_Elements_Parent : View.View_Access;
      Elements_Parent     : View.View_Access;

      Extend_Shrink_Button : Common.Button_Access;
      Is_Extended          : Boolean := False;
   end record;

end CRUD;
