with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;
with Breadcrumb;

package Menu is

   type Menu_Type is tagged private;

   function Create
     (Parent, Breadcrumb_Parent : in out Gnoga.Gui.View.View_Type)
      return Menu_Type;

   function Set_Root
     (Name    : UXString;
      On_Open : Gnoga.Gui.Base.Action_Event)
      return Integer;

   function Add_Child
     (Parent_Id : Integer;
      Name      : UXString;
      On_Open   : Gnoga.Gui.Base.Action_Event)
      return Integer;

   procedure Notify_Click
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer);

   procedure Set_Menu
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer);

private

   type Menu_Type is tagged record
      Parent             : Gnoga.Gui.View.View_Access;
      Breadcrumb_Parent  : Gnoga.Gui.View.View_Access;
      Breadcrumb_Content : Breadcrumb.Breadcrumb_Type;
   end record;

end Menu;
