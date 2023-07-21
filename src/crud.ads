with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings;      use UXStrings;

package Crud is

   type Crud_Type is tagged private;

   procedure Create
     (Instance  : in out Crud_Type;
      Parent    : in out Gnoga.Gui.View.View_Type;
      On_Resize :        Gnoga.Gui.Base.Action_Event;
      On_Click  :        Gnoga.Gui.Base.Action_Event);

   procedure Load (Instance : in out Crud_Type);

   procedure Clear (Instance : in out Crud_Type);

   --  In order to have a working shortcut system, roots must not be created
   --  after any child
   function Add_Root
     (Instance : in out Crud_Type;
      Name     :        UXString;
      Icon_SRC :        UXString := "")
      return Integer;

   function Add_Child
     (Instance  : in out Crud_Type;
      Name      :        UXString;
      Parent_Id :        Integer;
      Handler   :        Gnoga.Gui.Base.Action_Event := null)
      return Integer;

   procedure Add_Delimiter (Parent_Id : Integer);

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------
   procedure Set_Unclickable (Unique_Id : Integer);

   procedure Set_Clickable (Unique_Id : Integer);

   -----------------------------------------------------------------------------
   --  Getters
   -----------------------------------------------------------------------------
   function Menu_Name
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
      return UXString;

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------
   procedure On_Key_Pressed
     (Instance : in out Crud_Type;
      Key      : in     Character);

   procedure Notify_Root_Click
     (Instance : in out Crud_Type;
      Object   : in out Gnoga.Gui.Base.Base_Type'Class);

   procedure Notify_Click
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer);

   procedure Notify_Key_Pressed
     (Instance : in out Crud_Type;
      Key      :        Character);

   procedure Notify_Resize (Instance : in out Crud_Type);

private

   Root_Parent_Id      : constant Integer           := -1;
   Force_Shortcut_Char : constant Unicode_Character := '~';

   type Data_Type is record
      Parent_Id : Integer  := Root_Parent_Id;
      Icon_SRC  : UXString := "";
      Name      : UXString;

      Handler     : Gnoga.Gui.Base.Action_Event;
      Shortcut_Id : Integer;
   end record;

   Max_Menu_Count : constant Integer := 50;
   type Menu_Array is array (1 .. Max_Menu_Count) of Data_Type;

   type Boolean_Array is array (Integer range <>) of Boolean;

   type Crud_Type is tagged record
      Active_Shortcuts : Boolean_Array (1 .. 26)             := (others => False);
      Active_Menu      : Boolean_Array (1 .. Max_Menu_Count) := (others => False);
      Menu_Table       : Menu_Array;
      Next_Id          : Integer                             := 1;

      Current_Root : Integer := Root_Parent_Id;
      Is_Opened : Boolean := False;

      On_Click : Gnoga.Gui.Base.Action_Event;

      Parent                : Gnoga.Gui.View.View_Access;
      Tools_Container       : Gnoga.Gui.View.View_Access;
      Tools_Roots_Container : Gnoga.Gui.View.View_Access;

      Expand_Collapse_Button : Gnoga.Gui.Element.Common.Button_Access;
      Is_Expanded       : Boolean := False;
   end record;

end Crud;
