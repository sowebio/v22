-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-gui-footer.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Footer package
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

with Gnoga.Gui.View;

package v22.Gui.Footer is

   package View renames Gnoga.Gui.View;

   type Footer_Type is tagged private;

   procedure Create
     (Instance : in out Footer_Type;
      Parent   : in out View.View_Type);
   --  Should be called every time a user connects.

   procedure Set_State_Text
     (Instance : in out Footer_Type;
      State    :        String := "");
   --  Set user state text.

   procedure Set_Permanent_Text
     (Instance : in out Footer_Type;
      State    :        String := "");
   --  Set user permanent text.

private

   type Footer_Type is tagged record
      State_Text_Parent     : View.View_Access;
      Permanent_Text_Parent : View.View_Access;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.Footer;
-------------------------------------------------------------------------------
