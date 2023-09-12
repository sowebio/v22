-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-gui-breadcrumb.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Breadcrumb package
--
--  @description
--
--  @authors
--  Théodore Gigault - tg - developpement@soweb.io
--  Arthur Le Floch - alf - developpement@soweb.io
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Gnoga.Gui.Base;
with Gnoga.Gui.View;

package v22.Gui.Breadcrumb is

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;

   type Breadcrumb_Type is tagged private;

   procedure Create
     (Instance : in out Breadcrumb_Type;
      Parent   : in out View.View_Type);
   --  Should be called every time a user connects.

   procedure Update
     (Instance : in out Breadcrumb_Type;
      Handler  : in     Base.Action_Event;
      Content  : in     String := "";
      Depth    : in     Integer  := 0);
   --  Set new last element in instancied breadcrumb.

   procedure Clear (Instance : in out Breadcrumb_Type);
   --  Clear every element in breadcrumb.

private

   type Breadcrumb_Type is tagged record
      Parent        : View.View_Access;
      Current_Depth : Integer := 0;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.Breadcrumb;
-------------------------------------------------------------------------------
