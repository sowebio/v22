with Gnoga.Gui.Plugin;
with Gnoga.Gui.Plugin.jQueryUI;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with Gnoga.Gui.Base;        use Gnoga.Gui.Base;
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

   Max_Menu_Count : constant Integer := 50;
   Menu_Table     : array (1 .. Max_Menu_Count) of Data_Type;
   Next_Id        : Integer          := 1;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   function Value is new Integer_Value (Integer);

   function Replace_All
     (Text        : UXString;
      To_Replace  : Unicode_Character;
      Replacement : UXString)
      return UXString
   is
      Result : UXString := "";
   begin
      for Index in 1 .. Text.Length loop
         if Text.Element (Index) = To_Replace then
            Result := Result & Replacement;
         else
            Result := Result & Text.Element (Index);
         end if;
      end loop;
      return Result;
   end Replace_All;

   procedure Launch_Web
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Data   :        Data_Type)
   is
   begin
      Object.jQuery_Execute ("gnoga_web = open('" & Data.Content & "', '_blank')");
   end Launch_Web;

   procedure Dialog_Confirm_Handler (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Parent : constant Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access := Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access (Object.Parent);
      Unique_Id : constant Integer   := Value (Parent.jQuery_Execute ("data('gnoga_id')"));
      Data      : constant Data_Type := Menu_Table (Unique_Id);
   begin
      Parent.Close;
      Data.Confirm_Handler (Object);
   end Dialog_Confirm_Handler;

   procedure Dialog_Cancel_Handler (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Parent : constant Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access := Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access (Object.Parent);
      Unique_Id : constant Integer   := Value (Parent.jQuery_Execute ("data('gnoga_id')"));
      Data      : constant Data_Type := Menu_Table (Unique_Id);
   begin
      Parent.Close;
      Data.Confirm_Handler (Object);
   end Dialog_Cancel_Handler;

   procedure Launch_Dialog
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Unique_Id : Integer)
   is
      Data : constant Data_Type := Menu_Table (Unique_Id);
      Dialog_Class : constant Gnoga.Gui.Plugin.jQueryUI.Widget.Pointer_To_Dialog_Class :=
        new Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Type;
      Dialog : constant Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access :=
        Gnoga.Gui.Plugin.jQueryUI.Widget.Dialog_Access (Dialog_Class);
   begin
      Dialog.Create (Object, Replace_All (Data.Title, ''', "\'"), Data.Content, Width => 400, Height => 300);
      Dialog.jQuery_Execute ("data('gnoga_id', " & From_UTF_8 (Unique_Id'Image) & " )");

      if Data.Cancel_Handler /= null then
         Gnoga.Gui.Element.Common.Button_Access
           (Dialog.New_Element ("cancel", new Gnoga.Gui.Element.Common.Button_Type))
           .Create
           (Dialog.all, Data.Cancel_Text);
         Gnoga.Gui.Element.Common.Button_Access (Dialog.Element ("cancel")).On_Click_Handler (Dialog_Cancel_Handler'Unrestricted_Access);
         Gnoga.Gui.Element.Common.Button_Access (Dialog.Element ("cancel")).Class_Name ("ui-button ui-corner-all");
         Gnoga.Gui.Plugin.jQueryUI.Position
           (Dialog.Element ("cancel").all, Target => Dialog.all, Using_My => "bottom",
            At_Target                             => "left+70 bottom-10");
      end if;

      if Data.Confirm_Handler /= null then
         Gnoga.Gui.Element.Common.Button_Access
           (Dialog.New_Element ("confirm", new Gnoga.Gui.Element.Common.Button_Type))
           .Create
           (Dialog.all, Data.Confirm_Text);
         Gnoga.Gui.Element.Common.Button_Access (Dialog.Element ("confirm")).Focus;
         Gnoga.Gui.Element.Common.Button_Access (Dialog.Element ("confirm")).On_Click_Handler (Dialog_Confirm_Handler'Unrestricted_Access);
         Gnoga.Gui.Element.Common.Button_Access (Dialog.Element ("confirm")).Class_Name ("ui-button ui-corner-all");
         Gnoga.Gui.Plugin.jQueryUI.Position
           (Dialog.Element ("confirm").all, Target => Dialog.all, Using_My => "bottom",
            At_Target                              => "right-70 bottom-10");
      end if;

   end Launch_Dialog;

   procedure Click_Handler (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Button_Id : constant Integer   := Value (Object.jQuery_Execute ("data('gnoga_id')"));
      Data      : constant Data_Type := Menu_Table (Button_Id);
   begin
      if Data.Action = Dialog then
         Launch_Dialog (Object, Button_Id);
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

   -- Buttons are not displayed if their handlers are "null"
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
