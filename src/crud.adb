with Ada.Wide_Wide_Characters.Unicode;
with GNAT.Case_Util;
with Gnoga.Gui.Element;     use Gnoga.Gui.Element;
with UXStrings.Conversions; use UXStrings.Conversions;

package body Crud is

   CRUD_Error : exception;

   Root_Parent_Id      : constant Integer           := -1;
   Force_Shortcut_Char : constant Unicode_Character := '~';

   type Data_Type is record
      Parent_Id : Integer  := Root_Parent_Id;
      Icon_SRC  : UXString := "";
      Name      : UXString;

      Handler     : Gnoga.Gui.Base.Action_Event;
      Shortcut_Id : Integer;
   end record;

   Menu_Table : array (1 .. Max_Menu_Count) of Data_Type;
   Next_Id    : Integer := 1;

   --  This is quite ugly as it was wrote in the CSS file
   Icon_Button_Height : constant Integer := 48;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   function Boolean_Value (Text : UXString) return Boolean is
   begin
      if Text = "false" then
         return False;
      elsif Text = "true" then
         return True;
      end if;
      raise CRUD_Error with "Given text is not a boolean";
   end Boolean_Value;

   function Value is new Integer_Value (Integer);

   procedure Remove_Button
     (Parent  : Gnoga.Gui.View.View_Access;
      Data_Id : Integer)
   is
      Button_Name : UXString := "";
   begin
      Button_Name := "Crud_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
      if Parent.Element (Button_Name) /= null then
         Parent.Element (Button_Name).Remove;
      end if;
   end Remove_Button;

   procedure Open_Root
     (Instance  : in out Crud_Type;
      Parent_Id :        Integer)
   is

      function Vertical_Offset
        (Parent_Id : Integer)
         return Integer
      is
         Result : Integer := 0;
      begin
         for Index in 1 .. (Parent_Id - 1) loop
            if Menu_Table (Index).Parent_Id = Root_Parent_Id then
               Result := Result + Icon_Button_Height;
            end if;
         end loop;
         return Result;
      end Vertical_Offset;

      Parent : constant Gnoga.Gui.View.View_Access :=
        Gnoga.Gui.View.View_Access (Instance.Parent.Element ("tools-container"));

      Is_Opened : constant Boolean := Boolean_Value (Parent.jQuery_Execute ("data('gnoga_is_opened')"));
      Current_Root_Id : Integer := Value (Parent.jQuery_Execute ("data('gnoga_root_id')"));
      Will_Open : constant Boolean := not (Is_Opened and then (Current_Root_Id = Parent_Id));

   begin
      for Index in 1 .. (Next_Id - 1) loop
         Instance.Active_Shortcuts (Menu_Table (Index).Shortcut_Id) := False;
         Instance.Active_Menu (Index) := False;
         if Menu_Table (Index).Parent_Id = Root_Parent_Id then
            Instance.Active_Shortcuts (Menu_Table (Index).Shortcut_Id) := True;
            Instance.Active_Menu (Index) := True;
         else
            Remove_Button (Parent, Index);
         end if;
      end loop;

      for Index in 1 .. (Next_Id - 1) loop
         if Will_Open and then Menu_Table (Index).Parent_Id = Parent_Id then
            Instance.Active_Shortcuts (Menu_Table (Index).Shortcut_Id) := True;
            Instance.Active_Menu (Index)                               := True;
         end if;
      end loop;

      if Will_Open then
         for Index in 1 .. (Next_Id - 1) loop
            declare
               Data        : constant Data_Type                                  := Menu_Table (Index);
               Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
               Button_Name : constant UXString := "Crud_" & From_UTF_8 (Index'Image).Delete (1, 1);
            begin
               if Data.Parent_Id = Parent_Id then
                  Gnoga.Gui.Element.Common.Button_Access (Button).Create (Parent.all, Data.Name);
                  Button.Dynamic;
                  Button.On_Click_Handler (Data.Handler);
                  Parent.Add_Element (Button_Name, Button);
               end if;
            end;
         end loop;
         Parent.Style ("max-height", "calc(100% -" & From_UTF_8 (Vertical_Offset (Parent_Id)'Image) & "px)"); --  Could be declared once
         Parent.Top (Vertical_Offset (Parent_Id));
         Parent.jQuery_Execute ("data('gnoga_is_opened', true)");
         Parent.jQuery_Execute ("data('gnoga_root_id'," & From_UTF_8 (Parent_Id'Image) & ")");
         Current_Root_Id := Parent_Id;

      else
         Parent.jQuery_Execute ("data('gnoga_is_opened', false)");
         Parent.jQuery_Execute ("data('gnoga_root_id', -1)");
         Current_Root_Id := -1;
      end if;

      for Button_Id in 1 .. (Next_Id - 1) loop
         if Menu_Table (Button_Id).Parent_Id = Root_Parent_Id then
            declare
               Button : constant Gnoga.Gui.Element.Common.Button_Access :=
                 Gnoga.Gui.Element.Common.Button_Access (Instance.Parent.Element ("Crud_" & From_UTF_8 (Button_Id'Image).Delete (1, 1)));
            begin
               if Button_Id = Current_Root_Id then
                  Button.Add_Class ("toolbar-selected");
               else
                  Button.Remove_Class ("toolbar-selected");
               end if;
            end;
         end if;
      end loop;
   end Open_Root;

   function Code
     (Char : Unicode_Character)
      return Integer
   is
      Base : constant Unicode_Character := 'a';
   begin
      return
        1 + Unicode_Character'Pos (Ada.Wide_Wide_Characters.Unicode.To_Lower_Case (Char)) -
        Unicode_Character'Pos (Base);
   end Code;

   function Code
     (Char : Character)
      return Integer
   is
      Base : constant Character := 'a';
   begin
      return 1 + Character'Pos (GNAT.Case_Util.To_Lower (Char)) - Character'Pos (Base);
   end Code;

   function Is_Alpha
     (Char : Unicode_Character)
      return Boolean
   is
      Char_Code : constant Integer := Unicode_Character'Pos (Char);

      Lower_Char_Start : constant Unicode_Character := 'a';
      Lower_Char_End   : constant Unicode_Character := 'z';
      Upper_Char_Start : constant Unicode_Character := 'A';
      Upper_Char_End   : constant Unicode_Character := 'Z';

      Lower_Start : constant Integer := Unicode_Character'Pos (Lower_Char_Start);
      Lower_End   : constant Integer := Unicode_Character'Pos (Lower_Char_End);
      Upper_Start : constant Integer := Unicode_Character'Pos (Upper_Char_Start);
      Upper_End   : constant Integer := Unicode_Character'Pos (Upper_Char_End);
   begin
      if Char_Code >= Lower_Start and then Char_Code <= Lower_End then
         return True;
      elsif Char_Code >= Upper_Start and then Char_Code <= Upper_End then
         return True;
      end if;
      return False;
   end Is_Alpha;

   function Is_Shortcut_Available
     (Parent_Id : Integer;
      Shortcut  : Unicode_Character)
      return Boolean
   is
      Shortcut_Id : constant Integer := Code (Shortcut);
   begin
      if Parent_Id /= Root_Parent_Id and then Menu_Table (Parent_Id).Shortcut_Id = Shortcut_Id then
         return False;
      end if;
      for Index in 1 .. (Next_Id - 1) loop
         if Menu_Table (Index).Parent_Id = Parent_Id and then Menu_Table (Index).Shortcut_Id = Shortcut_Id then
            return False;
         end if;
      end loop;
      return True;
   end Is_Shortcut_Available;

   function Find_Shortcut
     (Parent_Id : Integer;
      Name      : UXString)
      return Unicode_Character
   is
      Char : Unicode_Character := '0';
   begin
      for Index in 1 .. Name.Length loop
         Char := Name (Index);
         if Is_Alpha (Char) then
            if Is_Shortcut_Available (Parent_Id, Char) then
               return Char;
            end if;
         elsif Char = Force_Shortcut_Char then
            --  This assumes the next character is part of the alphabet
            if Index /= Name.Length then
               return Name (Index + 1);
            end if;
         end if;
      end loop;
      raise CRUD_Error with "Could not find any valid shortcut";
   end Find_Shortcut;

   function HTML_Icon
     (Unique_Id : Integer)
      return UXString
   is
      Data : constant Data_Type := Menu_Table (Unique_Id);
   begin
      return From_UTF_8 ("<img class=""tools-icon"" src=""") & Data.Icon_SRC & From_UTF_8 (""">");
   end HTML_Icon;

   function Complete_Name
     (Unique_Id : Integer;
      Name      : UXString;
      Shortcut  : Unicode_Character)
      return UXString
   is
      Result : UXString           := "";
      Data   : constant Data_Type := Menu_Table (Unique_Id);
   begin
      if Data.Parent_Id = Root_Parent_Id then
         Result := Result & HTML_Icon (Unique_Id);
      end if;
      Result := Result & From_UTF_8 ("<span>");
      for Index in 1 .. Name.Length loop
         if Name (Index) = Shortcut then
            Result := Result & From_UTF_8 ("<span class=""shortcut"">") & Name (Index) & From_UTF_8 ("</span>");
         else
            Result := Result & Name (Index);
         end if;
      end loop;
      return Result & From_UTF_8 ("</span>");
   end Complete_Name;

   procedure Show_With_Code (Code : Integer) is
      Base      : constant Unicode_Character := 'a';
      Char_Code : constant Integer           := Code + Unicode_Character'Pos (Base) - 1;
   begin
      Gnoga.Log (From_Unicode ("" & Unicode_Character'Val (Char_Code)));
   end Show_With_Code;

   function Menu_Name
     (Unique_Id : Integer)
      return UXString
   is
   begin
      return Menu_Table (Unique_Id).Name;
   end Menu_Name;

   procedure Menu_Shortcut (Unique_Id : Integer) is
   begin
      Show_With_Code (Menu_Table (Unique_Id).Shortcut_Id);
   end Menu_Shortcut;

   procedure On_Shortcut_Pressed
     (Instance : in out Crud_Type;
      Key      : in     Character)
   is
      Char_Code : constant Integer := Code (Key);
      Data      : Data_Type;
   begin
      if Char_Code >= 1 and then Char_Code <= 26 then
         Gnoga.Log ("Pressed " & From_ASCII (Key) & " (Code:" & From_UTF_8 (Char_Code'Image) & ")");
         if Instance.Active_Shortcuts (Char_Code) then
            Gnoga.Log ("Shortcut is active");
            for Index in 1 .. (Next_Id - 1) loop
               Data := Menu_Table (Index);
               if Data.Shortcut_Id = Char_Code and then Instance.Active_Menu (Index) then
                  if Data.Parent_Id = Root_Parent_Id then
                     Open_Root (Instance, Index);
                  elsif Data.Handler /= null then
                     Data.Handler (Instance.Parent.all);
                  end if;
                  Gnoga.Log ("handled");
               end if;
            end loop;
         end if;
      end if;
      --  Gnoga.Log (From_UTF_8 (Event.Key_Code));
   end On_Shortcut_Pressed;

   procedure Hide_Tools_Text (Instance : in out Crud_Type) is
      Button_Name : UXString := "";
      Data        : Data_Type;
   begin
      for Index in 1 .. (Next_Id - 1) loop
         Data := Menu_Table (Index);
         if Data.Parent_Id = Root_Parent_Id then
            Button_Name := "Crud_" & From_UTF_8 (Index'Image).Delete (1, 1);
            Instance.Parent.Element (Button_Name).Inner_HTML (HTML_Icon (Index));
         end if;
      end loop;
   end Hide_Tools_Text;

   procedure Show_Tools_Text (Instance : in out Crud_Type) is
      Button_Name : UXString := "";
      Data        : Data_Type;
   begin
      for Index in 1 .. (Next_Id - 1) loop
         Data := Menu_Table (Index);
         if Data.Parent_Id = Root_Parent_Id then
            Button_Name := "Crud_" & From_UTF_8 (Index'Image).Delete (1, 1);
            Instance.Parent.Element (Button_Name).Inner_HTML (Data.Name);
         end if;
      end loop;
   end Show_Tools_Text;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   procedure Create
     (Instance  : in out Crud_Type;
      Parent    : in out Gnoga.Gui.View.View_Type;
      On_Resize :        Gnoga.Gui.Base.Action_Event;
      On_Click  :        Gnoga.Gui.Base.Action_Event)
   is
      Button_Name     : UXString;
      Collapse_Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
      Container       : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.View.View_Type;
   begin
      Gnoga.Gui.Element.Common.Button_Access (Collapse_Button).Create
        (Parent, "<img class=""tools-icon"" src=""css/icons/left_panel_open.png"">");
      Collapse_Button.Dynamic;
      Instance.Expand_Collapse_Button := Gnoga.Gui.Element.Common.Button_Access (Collapse_Button);
      Instance.Expand_Collapse_Button.Style ("position", "absolute");
      Instance.Expand_Collapse_Button.Style ("display", "flex");
      Instance.Expand_Collapse_Button.Style ("align-items", "center");
      Instance.Expand_Collapse_Button.Style ("right", "0");
      Instance.Expand_Collapse_Button.Style ("bottom", "0");
      Instance.Expand_Collapse_Button.On_Click_Handler (On_Resize);
      Instance.Parent := Parent'Unrestricted_Access;

      Gnoga.Gui.View.View_Access (Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("tools-container", Container);
      Container.Class_Name ("tools-container");
      Container.Style ("transform", "translate(100%)");
      Container.Style ("position", "absolute");
      Container.Style ("background-color", "#262635");
      Container.Style ("right", "0");
      Container.Style ("top", "0");
      Container.Style ("overflow", "scroll");
      Container.Dynamic;
      Container.jQuery_Execute ("data('gnoga_is_opened', false)");
      Container.jQuery_Execute ("data('gnoga_root_id', -1)");

      for Data_Id in 1 .. (Next_Id - 1) loop
         declare
            Data   : constant Data_Type                                  := Menu_Table (Data_Id);
            Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
         begin
            Instance.Active_Shortcuts (Menu_Table (Data_Id).Shortcut_Id) := (Data.Parent_Id = Root_Parent_Id);
            Instance.Active_Menu (Data_Id)                               := (Data.Parent_Id = Root_Parent_Id);
            if Data.Parent_Id = Root_Parent_Id then
               Button_Name := "Crud_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
               Gnoga.Gui.Element.Common.Button_Access (Button).Create (Parent, HTML_Icon (Data_Id));
               Button.Dynamic;
               Button.Style ("display", "flex");
               Button.Style ("align-items", "center");
               Button.jQuery_Execute ("data('gnoga_id', " & From_UTF_8 (Data_Id'Image) & " )");
               --  Button.On_Character_Handler (On_Shortcut_Pressed'Unrestricted_Access);
               Parent.Add_Element (Button_Name, Button);
               Button.On_Click_Handler (On_Click);
            end if;
         end;
      end loop;
   end Create;

   --  Default CRUD
   procedure Create_From_Base (Parent : in out Gnoga.Gui.View.View_Type) is
   begin
      null;
   end Create_From_Base;

   function Add_Root
     (Name     : UXString;
      Icon_SRC : UXString := "")
      return Integer
   is
   begin
      Menu_Table (Next_Id).Icon_SRC := Icon_SRC;
      return Add_Child (Name, Root_Parent_Id);
   end Add_Root;

   function Add_Child
     (Name      : UXString;
      Parent_Id : Integer;
      Handler   : Gnoga.Gui.Base.Action_Event := null)
      return Integer
   is
      Shortcut    : constant Unicode_Character := Find_Shortcut (Parent_Id, Name);
      Shortcut_Id : constant Integer           := Code (Shortcut);
   begin
      Menu_Table (Next_Id).Parent_Id := Parent_Id;
      Menu_Table (Next_Id).Name      := Complete_Name (Next_Id, Name, Shortcut);

      Menu_Table (Next_Id).Handler     := Handler;
      Menu_Table (Next_Id).Shortcut_Id := Shortcut_Id;

      --  Shortcut is enabled upon selecting parent
      Next_Id := Next_Id + 1;
      return Next_Id - 1;
   end Add_Child;

   procedure Add_Delimiter (Parent_Id : Integer) is
   begin
      --  Figure out what to put in here (div with bg color ?)
      null;
   end Add_Delimiter;

   procedure Set_Unclickable (Unique_Id : Integer) is
   begin
      --  Gray style (css class ?), no handler accepted or null handler
      null;
   end Set_Unclickable;

   procedure Set_Clickable (Unique_Id : Integer) is
   begin
      --  Handler should be accessible, default style
      null;
   end Set_Clickable;

   procedure Clear is
   begin
      null;
   end Clear;

   procedure Notify_Root_Clicked
     (Instance : in out Crud_Type;
      Object   : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      Parent_Id : constant Integer := Value (Object.jQuery_Execute ("data('gnoga_id')"));
   begin
      Open_Root (Instance, Parent_Id);
   end Notify_Root_Clicked;

   procedure Notify_Key_Pressed
     (Instance : in out Crud_Type;
      Key      :        Character)
   is
      Index : Integer;
      Data  : Data_Type;

      function Find_Associated_Data return Integer is
      begin
         for Index in 1 .. (Next_Id - 1) loop
            if Instance.Active_Menu (Index) and then Menu_Table (Index).Shortcut_Id = Code (Key) then
               return Index;
            end if;
         end loop;
         return -1;
      end Find_Associated_Data;

   begin
      if Instance.Active_Shortcuts (Code (Key)) then
         Index := Find_Associated_Data;
         if Index /= -1 then
            Data := Menu_Table (Index);
            if Data.Parent_Id = Root_Parent_Id then
               Open_Root (Instance, Index);
            else
               Data.Handler (Instance.Parent.all);
            end if;
         end if;
      end if;
   end Notify_Key_Pressed;

   procedure Notify_Resize (Instance : in out Crud_Type) is
   begin
      if Instance.Is_Menu_Expanded then
         Instance.Expand_Collapse_Button.Inner_HTML
           ("<img class=""tools-icon"" src=""css/icons/left_panel_open.png"">");
         Instance.Parent.Remove_Class ("force-expand");
         Instance.Parent.Add_Class ("force-collapse");
         Instance.Hide_Tools_Text;
      else
         Instance.Expand_Collapse_Button.Inner_HTML
           ("<img class=""tools-icon"" src=""css/icons/left_panel_close.png""><span>Fermer</span>");
         Instance.Parent.Remove_Class ("force-collapse");
         Instance.Parent.Add_Class ("force-expand");
         Instance.Show_Tools_Text;
      end if;
      Instance.Is_Menu_Expanded := not Instance.Is_Menu_Expanded;
   end Notify_Resize;

end Crud;
