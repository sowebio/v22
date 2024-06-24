-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-footer.ads
--  @copyright See authors list below and README.md file
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

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   package GGV renames Gnoga.Gui.View;

   type Footer_Type is tagged private;

   procedure Create (Instance : in out Footer_Type; Parent   : in out GGV.View_Type);
   --  Should be called every time a user connects.

   procedure Set_Left_Text (Instance : in out Footer_Type; State : String := "");
   --  Set footer left text.

   procedure Set_Right_Text (Instance : in out Footer_Type; State : String := "");
   --  Set footer right text.

-------------------------------------------------------------------------------
private

   type Footer_Type is tagged record
      Left_Text_Parent : GGV.View_Access;
      Right_Text_Parent : GGV.View_Access;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.Footer;
-------------------------------------------------------------------------------
