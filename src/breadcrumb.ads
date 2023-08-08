with Gnoga.Gui.Base;
with Gnoga.Gui.View;

with UXStrings; use UXStrings;

package Breadcrumb is

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;

   type Breadcrumb_Data is tagged record
      Parent        : View.View_Access;
      Current_Depth : Integer := 0;
   end record;
   type Breadcrumb_Type is new Breadcrumb_Data with null record;

   procedure Create
     (Instance : in out Breadcrumb_Type;
      Parent   : in out View.View_Type);
   --  Should be called every time a user connects

   procedure Update
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0);
   --  Set new last element in instancied breadcrumb

   procedure Clear (Instance : in out Breadcrumb_Type);
   --  Clear every element in breadcrumb

private

end Breadcrumb;
