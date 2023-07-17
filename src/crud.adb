with Ada.Wide_Wide_Characters.Unicode;
with GNAT.Case_Util;
with UXStrings.Conversions; use UXStrings.Conversions;
with Gnoga.Gui.Element;     use Gnoga.Gui.Element;

package body Crud is

   CRUD_Error : exception;

   Root_Parent_Id      : constant Integer           := -1;
   No_Menu             : constant Integer           := -1;
   Force_Shortcut_Char : constant Unicode_Character := '~';

   type Data_Type is record
      Parent_Id : Integer  := Root_Parent_Id;
      Icon_SRC  : UXString := "";
      Name      : UXString;
      Active    : Boolean  := False;

      Handler     : Gnoga.Gui.Base.Action_Event;
      Shortcut_Id : Integer;
   end record;

   Max_Menu_Count : constant Integer := 50;
   Menu_Table     : array (1 .. Max_Menu_Count) of Data_Type;
   Next_Id        : Integer          := 1;
   Selected_Menu  : Integer          := No_Menu;

   --  This is quite ugly as I wrote it in the CSS file
   Icon_Button_Height : constant Integer := 48;

   Active_Shortcuts : array (1 .. 26) of Boolean := (others => False);

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

   function Compute_Offset
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
   end Compute_Offset;

   procedure Update_Container (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Parent_Id : constant Integer                    := Value (Object.jQuery_Execute ("data('gnoga_id')"));
      Parent    : constant Gnoga.Gui.View.View_Access :=
        Gnoga.Gui.View.View_Access (Gnoga.Gui.View.View_Access (Object.Parent).Element ("tools-container"));
   begin

      for Index in 1 .. (Next_Id - 1) loop
         Remove_Button (Parent, Index);
      end loop;

      if Selected_Menu /= Parent_Id then
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
         Parent.Top (Compute_Offset (Parent_Id));
         Selected_Menu := Parent_Id;

      else
         Selected_Menu := No_Menu;
      end if;

   end Update_Container;

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

   procedure Update_Shortcuts is
   begin
      for Index in Menu_Table'Range loop
         declare
            Data : constant Data_Type := Menu_Table (Index);
         begin
            Active_Shortcuts (Data.Shortcut_Id) := Data.Active;
         end;
      end loop;
      -- for elm in menu_table
      -- used_shortcut [elm.shortcut_id] = True
      -- used_shortcut [elm.shortcut_id].handler = ...
   end Update_Shortcuts;

   procedure On_Shortcut_Pressed
     (Object : in out Gnoga.Gui.Base.Base_Type'Class;
      Key    : in     Character)
   is
      Char_Code : constant Integer := Code (Key);
   begin
      if Char_Code >= 1 and then Char_Code <= 26 then
         Gnoga.Log ("Pressed " & From_ASCII (Key) & " (Code:" & From_UTF_8 (Char_Code'Image) & ")");
         if Active_Shortcuts (Char_Code) then
            --  Menu_Table (Active_Shortcuts (Char_Code).Data_Id).Handler (Object);
            null;
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
      --  Reset every toolbar buttons to use smt like this:
      --  "<img class=""tools-icon"" src=""css/icons/left_panel_close.png""><span class=""tools-text"">Fermer</span>"
   end Show_Tools_Text;

   procedure Create
     (Instance  : in out Crud_Type;
      Parent    : in out Gnoga.Gui.View.View_Type;
      On_Resize :        Gnoga.Gui.Base.Action_Event)
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
      Container.Style ("width", "140px");
      Container.Style ("position", "absolute");
      Container.Style ("background-color", "#262635");
      Container.Style ("right", "-140px");
      Container.Style ("top", "0");
      Container.Dynamic;

      for Data_Id in 1 .. (Next_Id - 1) loop
         declare
            Data   : constant Data_Type                                  := Menu_Table (Data_Id);
            Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
         begin
            if Data.Parent_Id = Root_Parent_Id then
               Button_Name := "Crud_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
               Gnoga.Gui.Element.Common.Button_Access (Button).Create (Parent, HTML_Icon (Data_Id));
               Button.Dynamic;
               Button.Style ("display", "flex");
               Button.Style ("align-items", "center");
               Button.jQuery_Execute ("data('gnoga_id', " & From_UTF_8 (Data_Id'Image) & " )");
               Parent.Add_Element (Button_Name, Button);
               Button.On_Click_Handler (Data.Handler);
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
      return Add_Child (Name, Root_Parent_Id, Update_Container'Unrestricted_Access);
   end Add_Root;

   function Add_Child
     (Name      : UXString;
      Parent_Id : Integer;
      Handler   : Gnoga.Gui.Base.Action_Event)
      return Integer
   is
      Shortcut    : constant Unicode_Character := Find_Shortcut (Parent_Id, Name);
      Shortcut_Id : constant Integer           := Code (Shortcut);
   begin
      Menu_Table (Next_Id).Parent_Id   := Parent_Id;
      Menu_Table (Next_Id).Name        := Complete_Name (Next_Id, Name, Shortcut);
      Menu_Table (Next_Id).Active      := (Parent_Id = Root_Parent_Id);
      Menu_Table (Next_Id).Handler     := Handler;
      Menu_Table (Next_Id).Shortcut_Id := Shortcut_Id;

      --  Shortcut is enabled upon selecting parent
      Active_Shortcuts (Shortcut_Id) := False;

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

   function Remove_Shortcut_Marker
     (Text : UXString)
      return UXString
   is
      Result : UXString := "";
   begin
      for Index in 1 .. Text.Length loop
         if not (Text.Element (Index) = '~') then
            Result := Result & Text.Element (Index);
         end if;
      end loop;
      return Result;
   end Remove_Shortcut_Marker;

   procedure Notify_Click (Unique_Id : Integer) is
   begin
      null;
   end Notify_Click;

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
