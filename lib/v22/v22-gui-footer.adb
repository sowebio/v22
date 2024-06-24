-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-footer.adb
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

package body v22.Gui.Footer is

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Create (Instance : in out Footer_Type; Parent : in out GGV.View_Type) is
      Left_Text_Parent : constant GGV.Pointer_To_View_Class := new GGV.View_Type;
      Right_Text_Parent : constant GGV.Pointer_To_View_Class := new GGV.View_Type;
   begin
      Left_Text_Parent.Dynamic;
      Right_Text_Parent.Dynamic;

      Instance.Left_Text_Parent := GGV.View_Access (Left_Text_Parent);
      Instance.Left_Text_Parent.Create (Parent);
      Instance.Left_Text_Parent.Class_Name ("left-text-parent");

      Instance.Right_Text_Parent := GGV.View_Access (Right_Text_Parent);
      Instance.Right_Text_Parent.Create (Parent);
      Instance.Right_Text_Parent.Class_Name ("right-text-parent");
   end Create;

   ----------------------------------------------------------------------------
   procedure Set_Left_Text (Instance : in out Footer_Type; State : String := "") is
   begin
      Instance.Left_Text_Parent.Inner_HTML (State);
   end Set_Left_Text;

   procedure Set_Right_Text (Instance : in out Footer_Type; State : String := "") is
   begin
      Instance.Right_Text_Parent.Inner_HTML (State);
   end Set_Right_Text;

-------------------------------------------------------------------------------
end v22.Gui.Footer;
-------------------------------------------------------------------------------
