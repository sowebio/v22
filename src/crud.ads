with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings; use UXStrings;

package Crud is

   type Crud_Type is tagged private;

   procedure Create
     (Instance  : in out Crud_Type;
      Parent    : in out Gnoga.Gui.View.View_Type;
      On_Resize :        Gnoga.Gui.Base.Action_Event);

      --  Default CRUD
   procedure Create_From_Base (Parent : in out Gnoga.Gui.View.View_Type);

   procedure On_Shortcut_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Key    : in     Character);

   function Add_Root
     (Name     : UXString;
      Icon_SRC : UXString := "")
      return Integer;

   function Add_Child
     (Name      : UXString;
      Parent_Id : Integer;
      Handler   : Gnoga.Gui.Base.Action_Event)
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

   function Find_Shortcut
     (Parent_Id : Integer;
      Name      : UXString)
      return Unicode_Character;
      --  need boolean array to know which shortcuts can be used
      --  should be updated a lot

   function Remove_Shortcut_Marker
     (Text : UXString)
      return UXString;

   procedure Show_With_Code (Code : Integer);

   procedure Notify_Click (Unique_Id : Integer);

   procedure Notify_Resize (Instance : in out Crud_Type);

private

   type Crud_Type is tagged record
      Parent                 : Gnoga.Gui.View.View_Access;
      Expand_Collapse_Button : Gnoga.Gui.Element.Common.Button_Access;
      Is_Menu_Expanded       : Boolean := False;
   end record;

end Crud;
