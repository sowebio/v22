with Gnoga.Gui.Plugin;
with Gnoga.Gui.Plugin.jQueryUI;
with Gnoga.Gui.Plugin.jQueryUI.Widget;
with Gnoga.Gui.Base;        use Gnoga.Gui.Base;
with Gnoga.Gui.Element;     use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings.Conversions; use UXStrings.Conversions;

package body User_Menu is

   package jQueryUI renames Gnoga.Gui.Plugin.jQueryUI;
   package Widget renames Gnoga.Gui.Plugin.jQueryUI.Widget;
   package Element renames Gnoga.Gui.Element;

   type Action_Type is (Click, Dialog, Web);

   type Data_Type is record
      Button : Common.Button_Access;
      Action : Action_Type;

      Title   : UXString := "";
      Content : UXString := "";

      Click_Handler : Base.Action_Event;

      Confirm_Text    : UXString := "";
      Confirm_Handler : Base.Action_Event;

      Cancel_Text    : UXString := "";
      Cancel_Handler : Base.Action_Event;
   end record;

   Max_Menu_Amount : constant Integer := 50;
   Menu_Table      : array (1 .. Max_Menu_Amount) of Data_Type;
   Last_Index      : Integer          := 0;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   function Value is new Integer_Value (Integer);

   function To_UXString
     (Value : Integer)
      return UXString
   is
   begin
      return From_UTF_8 (Value'Image).Delete (1, 1);
   end To_UXString;

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

   -----------------------------------------------------------------------------
   --  Handlers
   -----------------------------------------------------------------------------
   procedure Dialog_Confirm_Handler (Object : in out Base.Base_Type'Class) is
      Dialog    : constant Widget.Dialog_Access := Widget.Dialog_Access (Object.Parent);
      Unique_ID : constant Integer              := Value (Dialog.jQuery_Execute ("data('gnoga_id')"));
      Data      : constant Data_Type            := Menu_Table (Unique_ID);
   begin
      Dialog.Remove;
      Data.Confirm_Handler (Object);
   end Dialog_Confirm_Handler;

   procedure Dialog_Cancel_Handler (Object : in out Base.Base_Type'Class) is
      Dialog    : constant Widget.Dialog_Access := Widget.Dialog_Access (Object.Parent);
      Unique_ID : constant Integer              := Value (Dialog.jQuery_Execute ("data('gnoga_id')"));
      Data      : constant Data_Type            := Menu_Table (Unique_ID);
   begin
      Dialog.Remove;
      Data.Cancel_Handler (Object);
   end Dialog_Cancel_Handler;

   procedure Click_Handler (Object : in out Base.Base_Type'Class) is
      Button_ID : constant Integer   := Value (Object.jQuery_Execute ("data('gnoga_id')"));
      Data      : constant Data_Type := Menu_Table (Button_ID);
   begin
      if Data.Action = Click then
         Launch_Button (Object, Button_ID);
      elsif Data.Action = Dialog then
         Launch_Dialog (Object, Button_ID);
      elsif Data.Action = Web then
         Launch_Web (Object, Button_ID);
      end if;
   end Click_Handler;

   procedure Remove_Dialog (Object : in out Base.Base_Type'Class) is
      Dialog_Class : constant Element.Pointer_To_Element_Class := View.View_Access (Object.Parent).Element ("dialog");
      Dialog       : constant Widget.Dialog_Access             := Widget.Dialog_Access (Dialog_Class);
   begin
      Dialog.Remove;
   end Remove_Dialog;

   -----------------------------------------------------------------------------
   --  Launchers
   -----------------------------------------------------------------------------
   procedure Launch_Button
     (Object    : in out Base.Base_Type'Class;
      Unique_ID :        Integer)
   is
      Data : constant Data_Type := Menu_Table (Unique_ID);
   begin
      Data.Click_Handler (Object);
   end Launch_Button;

   procedure Launch_Dialog
     (Object    : in out Base.Base_Type'Class;
      Unique_ID :        Integer)
   is
      Data         : constant Data_Type                        := Menu_Table (Unique_ID);
      Dialog_Class : constant Element.Pointer_To_Element_Class := new Widget.Dialog_Type;
      Dialog       : constant Widget.Dialog_Access             := Widget.Dialog_Access (Dialog_Class);
   begin
      Dialog.Create (Object.Parent.all, Replace_All (Data.Title, ''', "\'"), Data.Content, Width => 400, Height => 300);
      Dialog_Class.Dynamic;
      Dialog.jQuery_Execute ("data('gnoga_id', " & To_UXString (Unique_ID) & " )");

      if Data.Cancel_Handler /= null then
         declare
            Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
         begin
            Common.Button_Access (Button).Create (Dialog.all, Data.Cancel_Text);
            Button.On_Click_Handler (Dialog_Cancel_Handler'Unrestricted_Access);
            Button.Dynamic;
            Button.Class_Name ("ui-button ui-corner-all");
            jQueryUI.Position
              (Button.all, Target => Dialog.all, Using_My => "bottom", At_Target => "left+70 bottom-10");
         end;

      end if;

      if Data.Confirm_Handler /= null then
         declare
            Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
         begin
            Common.Button_Access (Button).Create (Dialog.all, Data.Confirm_Text);
            Button.Dynamic;
            Button.Focus;
            Button.On_Click_Handler (Dialog_Confirm_Handler'Unrestricted_Access);
            Button.Class_Name ("ui-button ui-corner-all");
            jQueryUI.Position
              (Button.all, Target => Dialog.all, Using_My => "bottom", At_Target => "right-70 bottom-10");
         end;
      end if;

      Dialog.On_Close_Handler (Remove_Dialog'Unrestricted_Access);
      View.View_Access (Object.Parent).Add_Element ("dialog", Dialog_Class);

   end Launch_Dialog;

   procedure Launch_Web
     (Object    : in out Base.Base_Type'Class;
      Unique_ID :        Integer)
   is
   begin
      Object.jQuery_Execute ("gnoga_web = open('" & Menu_Table (Unique_ID).Content & "', '_blank')");
   end Launch_Web;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   procedure Display (Parent : in out View.View_Type) is
   begin
      for Index in 1 .. Last_Index loop
         declare
            Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
         begin
            Common.Button_Access (Button).Create (Parent, Menu_Table (Index).Title);
            Button.Dynamic;
            Button.Class_Name ("framework-button");
            Button.On_Click_Handler (Click_Handler'Unrestricted_Access);
            Button.jQuery_Execute ("data('gnoga_id', " & To_UXString (Index) & " )");
         end;
      end loop;
   end Display;

   procedure Add_Button
     (Title    : UXString;
      On_Click : Base.Action_Event)
   is
      Menu : Data_Type;
   begin
      Last_Index := Last_Index + 1;

      Menu.Action        := Click;
      Menu.Title         := Title;
      Menu.Click_Handler := On_Click;

      Menu_Table (Last_Index) := Menu;
   end Add_Button;

   procedure Add_Dialog
     (Title           : UXString;
      Content         : UXString          := "";
      Confirm_Text    : UXString          := "";
      Cancel_Text     : UXString          := "";
      Confirm_Handler : Base.Action_Event := null;
      Cancel_Handler  : Base.Action_Event := null)
   is
      Menu : Data_Type;
   begin
      Last_Index := Last_Index + 1;

      Menu.Action          := Dialog;
      Menu.Title           := Title;
      Menu.Content         := Content;
      Menu.Confirm_Text    := Confirm_Text;
      Menu.Cancel_Text     := Cancel_Text;
      Menu.Confirm_Handler := Confirm_Handler;
      Menu.Cancel_Handler  := Cancel_Handler;

      Menu_Table (Last_Index) := Menu;
   end Add_Dialog;

   procedure Add_Web
     (Title : UXString;
      URL   : UXString)
   is
      Menu : Data_Type;
   begin
      Last_Index := Last_Index + 1;

      Menu.Action  := Web;
      Menu.Title   := Title;
      Menu.Content := URL;

      Menu_Table (Last_Index) := Menu;
   end Add_Web;

   procedure Clear (Parent : in out View.View_Type) is
   begin
      Parent.Inner_HTML ("");
   end Clear;

end User_Menu;
