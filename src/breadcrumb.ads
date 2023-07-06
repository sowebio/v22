with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package Breadcrumb is

   type Breadcrumb_Type is tagged private;

   function Create
     (View : in out Gnoga.Gui.View.View_Type)
      return Breadcrumb_Type;

   procedure Add
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0);

   procedure Update
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0);

private

   type Breadcrumb_Type is tagged record
      Parent        : Gnoga.Gui.View.View_Access;
      Current_Depth : Integer := 0;
   end record;

end Breadcrumb;
