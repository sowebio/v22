-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-user_menu.adb
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

package body v22.Gui.User_Menu is

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Create (Instance : in out User_Menu_Type; Parent : GGV.View_Type) is
   begin
      Instance.Parent := Parent'Unrestricted_Access;
   end Create;

   ----------------------------------------------------------------------------
   procedure Display (Instance : in out User_Menu_Type) is
      Button : GGE.Pointer_To_Element_Class;
   begin
      for Index in 1 .. Instance.Last_Index loop
         Button := new GGEC.Button_Type;
         Button.Dynamic;
         GGEC.Button_Access (Button).Create (Instance.Parent.all, Instance.Menu_Table (Index).Name);
         Button.Class_Name ("framework-button");
         Button.On_Click_Handler (Instance.Menu_Table (Index).Click_Handler);
      end loop;
   end Display;

   ----------------------------------------------------------------------------
   procedure Add_Element (Instance : in out User_Menu_Type; Name : String; On_Click : GGB.Action_Event) is
      Menu : Data_Type;
   begin
      Menu.Name := Name;
      Menu.Click_Handler := On_Click;

      Instance.Last_Index := Instance.Last_Index + 1;
      Instance.Menu_Table (Instance.Last_Index) := Menu;
   end Add_Element;

   ----------------------------------------------------------------------------
   procedure Clear (Instance : in out User_Menu_Type) is
   begin
      Instance.Parent.Inner_HTML ("");
   end Clear;

-------------------------------------------------------------------------------
end v22.Gui.User_Menu;
-------------------------------------------------------------------------------
