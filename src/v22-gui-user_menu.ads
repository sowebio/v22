-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-gui-user_menu.ads
--  @copyright See authors list below and v22.copyrights file
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

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;
   package Common renames Gnoga.Gui.Element.Common;

   type User_Menu_Type is tagged private;

   procedure Create
     (Instance : in out User_Menu_Type;
      Parent   :        View.View_Type);

   procedure Display (Instance : in out User_Menu_Type);
   --  Should be called every time user menu is opened.

   procedure Add_Element
     (Instance : in out User_Menu_Type;
      Name     :        String;
      On_Click :        Base.Action_Event);
   --  Create a button with customized click handler.

   procedure Clear (Instance : in out User_Menu_Type);
   --  Remove elements from HTML.

private

   type Data_Type is record
      Button        : Common.Button_Access;
      Click_Handler : Base.Action_Event;
      Name          : String := "";
   end record;

   Max_Menu_Item : constant Integer := 50;

   type Menu_Array is array (1 .. Max_Menu_Item) of Data_Type;

   type User_Menu_Type is tagged record
      Parent     : View.View_Access;
      Menu_Table : Menu_Array;
      Last_Index : Integer := 0;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.User_Menu;
-------------------------------------------------------------------------------
