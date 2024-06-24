-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-breadcrumb.ads
--  @copyright See authors list below and README.md file
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

with v22.Uxs; use v22.Uxs;

package v22.Gui.Breadcrumb is

   package GGB renames Gnoga.Gui.Base;
   package GGV renames Gnoga.Gui.View;

   type Breadcrumb_Type is tagged private;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Create (Instance : in out Breadcrumb_Type; Parent : in out GGV.View_Type);
   --  Should be called every time a user connects.

   procedure Update (Instance : in out Breadcrumb_Type; Handler : GGB.Action_Event; Content : String := ""; Depth : Integer := 0);
   --  Set new last element in instancied breadcrumb.

   procedure Clear (Instance : in out Breadcrumb_Type);
   --  Clear every element in breadcrumb.

-------------------------------------------------------------------------------
private

   type Breadcrumb_Type is tagged record
      Parent : GGV.View_Access;
      Current_Depth : Integer := 0;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.Breadcrumb;
-------------------------------------------------------------------------------
