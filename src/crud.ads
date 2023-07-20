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

      --  Default CRUD
   procedure Create_From_Base (Parent : in out Gnoga.Gui.View.View_Type);

   procedure On_Shortcut_Pressed
     (Instance : in out Crud_Type;
      Key      : in     Character);

   function Add_Root
     (Name     : UXString;
      Icon_SRC : UXString := "")
      return Integer;

   function Add_Child
     (Name      : UXString;
      Parent_Id : Integer;
      Handler   : Gnoga.Gui.Base.Action_Event := null)
      return Integer;

   procedure Add_Delimiter (Parent_Id : Integer);

   procedure Set_Unclickable (Unique_Id : Integer);

   procedure Set_Clickable (Unique_Id : Integer);

   procedure Clear;

   function Menu_Name
     (Unique_Id : Integer)
      return UXString;
   procedure Menu_Shortcut (Unique_Id : Integer);

--private

   procedure Show_With_Code (Code : Integer);

   procedure Notify_Root_Clicked
     (Instance : in out Crud_Type;
      Object   : in out Gnoga.Gui.Base.Base_Type'Class);
   procedure Notify_Key_Pressed
     (Instance : in out Crud_Type;
      Key      :        Character);
   procedure Notify_Resize (Instance : in out Crud_Type);

private

   type Shortcut_Array is array (1 .. 26) of Boolean;
   Max_Menu_Count : constant Integer := 50;
   type Active_Array is array (1 .. Max_Menu_Count) of Boolean;

   type Crud_Type is tagged record
      Active_Shortcuts       : Shortcut_Array := (others => False);
      Active_Menu            : Active_Array   := (others => False);
      Parent                 : Gnoga.Gui.View.View_Access;
      Expand_Collapse_Button : Gnoga.Gui.Element.Common.Button_Access;
      Is_Menu_Expanded       : Boolean        := False;
   end record;

end Crud;
