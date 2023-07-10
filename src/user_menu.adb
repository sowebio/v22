with Gnoga.Gui.Plugin;
with Gnoga.Gui.Plugin.jQueryUI;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings.Conversions; use UXStrings.Conversions;

package body User_Menu is

   type Action_Type is (Dialog, Web);

   type Data_Type is record
      Button : Gnoga.Gui.Element.Common.Button_Access;
      Action : Action_Type;

      Title   : UXString := "";
      Content : UXString := "";

      Confirm_Text    : UXString := "";
      Confirm_Handler : Gnoga.Gui.Base.Action_Event;

      Cancel_Text    : UXString := "";
      Cancel_Handler : Gnoga.Gui.Base.Action_Event;
   end record;

   Max_Menu_Count : constant Integer := 5;
   Menu_Table     : array (1 .. Max_Menu_Count) of Data_Type;
   Next_Id        : Integer          := 1;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   function Value is new Integer_Value (Integer);

   procedure Launch_Web
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Data   :        Data_Type)
   is
   begin
      Object.jQuery_Execute ("gnoga_web = open('" & Data.Content & "', '_blank')");
   end Launch_Web;

   procedure Launch_Dialog
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Data   :        Data_Type)
   is
      Dialog : constant Gnoga.Gui.Plugin.jQueryUI.Widget.Pointer_To_Dialog_Class :=
        new Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Type;
   begin
      Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access (Dialog).Create
        (Object, Data.Title, Data.Content, Height => 240, Width => 240);
   end Launch_Dialog;

   procedure Click_Handler (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Button_Id : constant Integer   := Value (Object.jQuery_Execute ("data('gnoga_id')"));
      Data      : constant Data_Type := Menu_Table (Button_Id);
   begin
      if Data.Action = Dialog then
         Launch_Dialog (Object, Data);
      elsif Data.Action = Web then
         Launch_Web (Object, Data);
      end if;
   end Click_Handler;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   procedure Create (Parent : in out Gnoga.Gui.View.View_Type) is
   begin
      for Index in 1 .. (Next_Id - 1) loop
         declare
            Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
         begin
            Gnoga.Gui.Element.Common.Button_Access (Button).Create (Parent, Menu_Table (Index).Title);
            Button.Dynamic;
            Button.On_Click_Handler (Click_Handler'Unrestricted_Access);
            Button.jQuery_Execute ("data('gnoga_id', " & From_UTF_8 (Index'Image) & " )");
         end;
      end loop;
   end Create;

   -- Buttons are not displayed if their texts are ""
   procedure Add_Dialog
     (Title           : UXString;
      Content         : UXString                    := "";
      Confirm_Text    : UXString                    := "";
      Cancel_Text     : UXString                    := "";
      Confirm_Handler : Gnoga.Gui.Base.Action_Event := null;
      Cancel_Handler  : Gnoga.Gui.Base.Action_Event := null)
   is
      Menu : Data_Type;
   begin
      Menu.Action          := Dialog;
      Menu.Title           := Title;
      Menu.Content         := Content;
      Menu.Confirm_Text    := Confirm_Text;
      Menu.Cancel_Text     := Cancel_Text;
      Menu.Confirm_Handler := Confirm_Handler;
      Menu.Cancel_Handler  := Cancel_Handler;

      Menu_Table (Next_Id) := Menu;
      Next_Id              := Next_Id + 1;
   end Add_Dialog;

   procedure Add_Web
     (Title : UXString;
      URL   : UXString := "")
   is
      Menu : Data_Type;
   begin
      Menu.Action  := Web;
      Menu.Title   := Title;
      Menu.Content := URL;

      Menu_Table (Next_Id) := Menu;
      Next_Id              := Next_Id + 1;
   end Add_Web;

end User_Menu;
