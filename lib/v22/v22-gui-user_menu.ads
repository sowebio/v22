-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-user_menu.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - User menu package
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
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

package v22.Gui.User_Menu is

   package GGB renames Gnoga.Gui.Base;
   package GGV renames Gnoga.Gui.View;
   package GGE renames Gnoga.Gui.Element;
   package GGEC renames Gnoga.Gui.Element.Common;

   type User_Menu_Type is tagged private;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Create (Instance : in out User_Menu_Type; Parent : GGV.View_Type);
   --

   procedure Display (Instance : in out User_Menu_Type);
   --  Should be called every time user menu is opened.

   procedure Add_Element (Instance : in out User_Menu_Type; Name : String; On_Click : GGB.Action_Event);
   --  Create a button with customized click handler.

   procedure Clear (Instance : in out User_Menu_Type);
   --  Remove elements from HTML.

-------------------------------------------------------------------------------
private

   type Data_Type is record
      Button : GGEC.Button_Access;
      Click_Handler : GGB.Action_Event;
      Name : String := "";
   end record;

   Max_Menu_Item : constant Integer := 50;

   type Menu_Array is array (1 .. Max_Menu_Item) of Data_Type;

   type User_Menu_Type is tagged record
      Parent : GGV.View_Access;
      Menu_Table : Menu_Array;
      Last_Index : Integer := 0;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.User_Menu;
-------------------------------------------------------------------------------
