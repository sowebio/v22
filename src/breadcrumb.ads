with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package Breadcrumb is

   type Breadcrumb_Type is record
      Current_Depth : Integer := 0;
   end record;

   function Create return Breadcrumb_Type;

   procedure Add
     (Instance : in out Breadcrumb_Type;
      View     : in out Gnoga.Gui.View.View_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0);

   procedure Remove
     (Instance : in out Breadcrumb_Type;
      View     : in out Gnoga.Gui.View.View_Type);

   procedure Update
     (Instance : in out Breadcrumb_Type;
      View     : in out Gnoga.Gui.View.View_Type;
      Handler  : in     Gnoga.Gui.Base.Action_Event;
      Content  : in     UXString := "";
      Depth    : in     Integer  := 0);

end Breadcrumb;
