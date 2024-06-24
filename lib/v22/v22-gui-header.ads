-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-header.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Header package
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
with Gnoga.Gui.Element; use Gnoga.Gui.Element; -- Needed for operators
with Gnoga.Gui.Element.Common;

with v22.Gui.Breadcrumb;
with v22.Gui.User_Menu;

with v22.Uxs; use v22.Uxs;

package v22.Gui.Header is

   package GGB renames Gnoga.Gui.Base;
   package GGV renames Gnoga.Gui.View;
   package GGE renames Gnoga.Gui.Element;
   package GGEC renames Gnoga.Gui.Element.Common;

   type Header_Type is tagged private;

   procedure Create (Instance : in out Header_Type; Parent : in out GGV.View_Type; On_Logo, On_User : GGB.Action_Event);
   --  Should be called every time a user connects.

   -----------------------------------------------------------------------------
   --  Main Menu
   -----------------------------------------------------------------------------

   procedure Open_Menu (Instance : in out Header_Type; Unique_ID : Integer);
   --  Open navigation menu.

   procedure Close_Menu (Instance : in out Header_Type);
   --  Close navigation menu.

   function Is_Menu_Open (Instance : in out Header_Type) return Boolean;
   --  Return a boolean depending on navigation menu's state.

   function Set_Root (Name : String; On_Open : GGB.Action_Event) return Integer;
   --  Set (unique) first element in breadcrumb, i.e. the first button to appear.
   --  NOTE: Root must be created before childs, and returned value
   --  (Parent_ID) allows to create childs for this element.

   function Add_Child (Parent_ID : Integer; Name : String; On_Open : GGB.Action_Event) return Integer;
   --  Add child for a given parent.
   --  NOTE: A child always refers to a parent, which can be itself a child...

   procedure Clear (Instance : in out Header_Type);
   --  Removes current breadcrumb, waiting for upcoming breadcrumb update.

   procedure Set_Menu (Instance : in out Header_Type; Unique_ID : Integer);
   --  Set current menu, updating breadcrumb

   procedure Set_App_Icon (Instance : in out Header_Type; Icon_SRC : String);
   --  Set button icon (button opening navigation menu).

   procedure Notify_Menu_Click (Instance : in out Header_Type; Unique_ID : Integer);
   --  Callback to place in "On_Open" handlers (parameter of Add_Child).

   -----------------------------------------------------------------------------
   --  User Menu
   -----------------------------------------------------------------------------

   procedure Open_User_Menu (Instance : in out Header_Type);
   --  Open user navigation menu.

   procedure Close_User_Menu (Instance : in out Header_Type);
   --  Close user navigation menu.

   function Is_User_Menu_Open (Instance : in out Header_Type) return Boolean;

   procedure Add_Element (Instance : in out Header_Type; Name : String; On_Click : GGB.Action_Event);
   --  Create a button with customized click handler.

   procedure Set_User_Name (Instance : in out Header_Type; User_Name : String);
   --  Set user name (placed next to user icon).

   procedure Set_User_Icon (Instance : in out Header_Type; Icon_SRC : String);
   --  Set user icon (button opening user navigation menu).

   procedure Notify_User_Menu_Click (Instance : in out Header_Type);
   --  Callback

-------------------------------------------------------------------------------
private

   type Header_Type is tagged record
      Parent : GGV.View_Access;
      Icon : GGEC.IMG_Access;
      App_Is_Open : Boolean := False;
      User_Is_Open : Boolean := False;
      App_Parent : GGV.View_Access;
      App_Icon : aliased GGEC.IMG_Access;
      App_Navigation_Parent : GGV.View_Access;
      Breadcrumb_Parent : GGV.View_Access;
      Breadcrumb_Content : v22.Gui.Breadcrumb.Breadcrumb_Type;
      User_Parent : GGV.View_Access;
      User_Name : GGEC.P_Access;
      User_Icon : GGEC.IMG_Access;
      User_Navigation_Parent : GGV.View_Access;
      User_Content : v22.Gui.User_Menu.User_Menu_Type;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.Header;
-------------------------------------------------------------------------------
