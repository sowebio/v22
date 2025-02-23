-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testgui-usr.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - GUI test program
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

separate (TestGui) package body Usr is

   ----------------------------------------------------------------------------
   --  Declarations
   ----------------------------------------------------------------------------

   procedure On_Dialog_Confirm (Object : in out GGB.Base_Type'Class);
   procedure On_Dialog_Cancel (Object : in out GGB.Base_Type'Class);

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Connect (Object : in out GGB.Base_Type'Class) is
   begin

      Msg.Info ("TestGui.On_Connect > Login screen");

      Gui.Set_User_Name (Object, "Déconnecté");
      Gui.Put_User_Icon (Object);

      Gui.Footer_Set_Left_Text (Object, Prg.Get_Version & " - " & v22.Get_Version & " - " & v22.Get_Build);
      Gui.Footer_Set_Right_Text (Object, "Sowebio SARL");

      Gui.Header_User_Menu_Add (Object, "Aide", Help'Unrestricted_Access);
      Gui.Header_User_Menu_Add (Object, "Informations", Information'Unrestricted_Access);
      Gui.Header_User_Menu_Add (Object, "Déconnexion", Gui.User_Logout'Unrestricted_Access);
   end Connect;

   ----------------------------------------------------------------------------
   procedure Help (Object : in out GGB.Base_Type'Class) is
   begin
      Gui.Header_Notify_User_Menu_Click (Object);
      Gui.Content_Put_Title (Object, "Aide");
      Gui.Content_Load_HTML (Object, "fr-help-main.html");
   end Help;

   ----------------------------------------------------------------------------
   procedure Information (Object : in out GGB.Base_Type'Class) is
   begin
      Gui.Header_Notify_User_Menu_Click (Object);
      Gui.Dialog_Popup (Object, "Droits d'accès", "Ajouter les droits d'accès ici", "Confirmer",
                         On_Dialog_Confirm'Unrestricted_Access, "Annuler", On_Dialog_Cancel'Unrestricted_Access);
   end Information;

   ----------------------------------------------------------------------------
   procedure On_Dialog_Confirm (Object : in out GGB.Base_Type'Class) is
   begin
      Gnoga.Log ("Dialog: confirmed");
      Gui.Close_Dialog (Object);
   end On_Dialog_Confirm;

   ----------------------------------------------------------------------------
   procedure On_Dialog_Cancel (Object : in out GGB.Base_Type'Class) is
   begin
      Gnoga.Log ("Dialog: cancelled");
      Gui.Close_Dialog (Object);
   end On_Dialog_Cancel;

-------------------------------------------------------------------------------
end Usr;
-------------------------------------------------------------------------------
