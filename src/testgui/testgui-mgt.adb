-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testgui-mgt.adb
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

separate (TestGui) package body Mgt is

   ----------------------------------------------------------------------------
   procedure Update_TTC (Object : in out GGB.Base_Type'Class) is
      Price  : constant Integer := Gui.Content_Group_Number_Get (Object, "Prix HT");
      TVA    : constant Integer := Gui.Content_Group_Number_Get (Object, "TVA");
      Result : Integer;
   begin
      Result := Price + TVA;
      Gui.Content_Group_Text_Set (Object, "TTC", From_UTF_8 (Result'Image) & " €");
   end Update_TTC;

   ----------------------------------------------------------------------------
   procedure Demo_Complex_Form_Create (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Dispensers > Demo_Complex_Form_Create");

      Gui.Main_Menu_Notify_Sub_Element_Click (Object, "Preferences_Create");

      Gui.Content_Put_Title (Object, "Menu mode 2 - Créer");
      Gui.Content_Clear_Text (Object);

      Gui.Main_Menu_Set_Clickable (Object, "Preferences_Create");
      Gui.Main_Menu_Set_Clickable (Object, "Preferences_Edit");
      Gui.Main_Menu_Set_Clickable (Object, "Preferences_Delete");

      Gui.Content_Group_Create (Object, "Spécification du contrat");

      Gui.Content_Group_Add_Title (Object, "Détails du contrat", "Spécification du contrat");

      Gui.Content_Group_Drop_Down_Menu_Add (Object, "Type de contrat", "Spécification du contrat");
      Gui.Content_Group_Drop_Down_Menu_Add_Option (Object, "Type de contrat", "Standard", Enabled => True);
      Gui.Content_Group_Drop_Down_Menu_Add_Option (Object, "Type de contrat", "Modifié");
      Gui.Content_Group_Drop_Down_Menu_Add_Option (Object, "Type de contrat", "Complexe");
      Gui.Content_Group_Date_Add (Object, "Date de création", "Spécification du contrat");
      Gui.Content_Group_Number_Add (Object, "Récurrence", "Spécification du contrat");
      Gui.Content_Group_Add_Title (Object, "Activité", "Spécification du contrat");
      Gui.Content_Group_Number_Add (Object, "Durée d'engagement en jours", "Spécification du contrat");
      Gui.Content_Group_Number_Set (Object, "Durée d'engagement en jours", 30);
      Gui.Content_Group_Check_Box_Add (Object, "Contrat actif", "Spécification du contrat");
      Gui.Content_Group_Check_Box_Checked (Object, "Contrat actif", True);
      Gui.Content_Group_Number_Add (Object, "Durée du contrat en jours", "Spécification du contrat");
      Gui.Content_Group_Number_Set (Object, "Durée du contrat en jours", 30);
      Gui.Content_Group_Item_Lock (Object, "Durée du contrat en jours");

      Gui.Content_Group_Create (Object, "Prix");
      Gui.Content_Group_Number_Add (Object, "Prix HT", "Prix", Update_TTC'Unrestricted_Access);
      Gui.Content_Group_Number_Set (Object, "Prix HT", 0);
      Gui.Content_Group_Number_Add (Object, "TVA", "Prix", Update_TTC'Unrestricted_Access);
      Gui.Content_Group_Number_Set (Object, "TVA", 0);
      Gui.Content_Group_Text_Add (Object, "TTC", "Prix");

      Gui.Content_Group_Create (Object, "Agenda");
      Gui.Content_Group_Date_Add (Object, "Prochaine date de facturation", "Agenda");
      Gui.Content_Group_Date_Add (Object, "Prochaine date d'échéance", "Agenda");

      Gui.Content_Group_Create (Object, "Autres informations");
      Gui.Content_Group_Text_Area_Add (Object, "Note publique", "Autres informations");
      Gui.Content_Group_Text_Area_Add (Object, "Note privée", "Autres informations");

      Gui.Content_Group_Item_Lock (Object, "Prochaine date d'échéance");
      Gui.Content_Group_Item_Lock (Object, "Note privée");
      Gui.Content_Group_Item_Lock (Object, "TTC");
      Gui.Content_Group_Text_Set (Object, "TTC", "Remplissez les données précédentes");
      Gui.Content_Group_Text_Set (Object, "TTC", "0 €");

   end Demo_Complex_Form_Create;

   ----------------------------------------------------------------------------
   procedure Demo_Complex_Form_Edit (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Dispensers > Demo_Complex_Form_Edit");

      Gui.Main_Menu_Notify_Sub_Element_Click (Object, "Preferences_Edit");

      Gui.Content_Put_Title (Object, "Menu mode 2 - Édition");
      Gui.Content_Put_Text (Object, "Mofifier et Supprimer sont clicables, Créer n'est pas clicable");

      Gui.Main_Menu_Set_Unclickable (Object, "Preferences_Create");
      Gui.Main_Menu_Set_Clickable (Object, "Preferences_Edit");
      Gui.Main_Menu_Set_Clickable (Object, "Preferences_Delete");

   end Demo_Complex_Form_Edit;

   ----------------------------------------------------------------------------
   procedure Demo_Complex_Form_Delete (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Dispensers > Demo_Complex_Form_Delete");

      Gui.Main_Menu_Notify_Sub_Element_Click (Object, "Preferences_Delete");

      Gui.Content_Put_Title (Object, "Menu mode 2 - Suppression");
      Gui.Content_Put_Text (Object, "Créer et Supprimer sont clicables, Modifier n'est pas clicable");

      Gui.Main_Menu_Set_Clickable (Object, "Preferences_Create");
      Gui.Main_Menu_Set_Unclickable (Object, "Preferences_Edit");
      Gui.Main_Menu_Set_Clickable (Object, "Preferences_Delete");

   end Demo_Complex_Form_Delete;

   ----------------------------------------------------------------------------
   procedure Demo_Mode_1 (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug ("Dispensers > Demo_Mode_1");

      Gui.Content_Put_Title (Object, "Menu mode 1");
      Gui.Content_Put_Text (Object, "Appel direct à parti du menu");

   end Demo_Mode_1;

-------------------------------------------------------------------------------
end Mgt;
-------------------------------------------------------------------------------
