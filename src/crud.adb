with Ada.Wide_Wide_Characters.Unicode;
with GNAT.Case_Util;
with Gnoga.Gui.Element;     use Gnoga.Gui.Element;
with UXStrings.Conversions; use UXStrings.Conversions;

package body Crud is

   CRUD_Error : exception;

   -----------------------------------------------------------------------------
   --  Conversions
   -----------------------------------------------------------------------------
   function Value is new Integer_Value (Integer);

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
      First_Lower : constant Unicode_Character := 'a';
      Last_Lower : constant Unicode_Character := 'z';
      First_Upper : constant Unicode_Character := 'A';
      Last_Upper : constant Unicode_Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Alpha;

   function Is_Alpha
     (Char : Character)
      return Boolean
   is
      First_Lower : constant Character := 'a';
      Last_Lower : constant Character := 'z';
      First_Upper : constant Character := 'A';
      Last_Upper : constant Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Alpha;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
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
      Clicked_Button : constant Gnoga.Gui.Element.Common.Button_Access :=
        Gnoga.Gui.Element.Common.Button_Access
          (Instance.Tools_Roots_Container.Element ("Crud_" & From_UTF_8 (Parent_Id'Image).Delete (1, 1)));
      Will_Open       : constant Boolean := not (Instance.Is_Opened and then (Instance.Current_Root = Parent_Id));
      Vertical_Offset : constant Integer := Clicked_Button.Offset_From_Top - Instance.Parent.Offset_From_Top;
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_Id) := False;
         Instance.Active_Menu (Index)                                        := False;
         if Instance.Menu_Table (Index).Parent_Id = Root_Parent_Id then
            Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_Id) := True;
            Instance.Active_Menu (Index)                                        := True;
         else
            Remove_Button (Instance.Tools_Container, Index);
         end if;
      end loop;

      for Index in 1 .. (Instance.Next_Id - 1) loop
         if Will_Open and then Instance.Menu_Table (Index).Parent_Id = Parent_Id then
            Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_Id) := True;
            Instance.Active_Menu (Index)                                        := True;
         end if;
      end loop;

      if Will_Open then
         for Index in 1 .. (Instance.Next_Id - 1) loop
            declare
               Data        : constant Data_Type                                  := Instance.Menu_Table (Index);
               Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
               Button_Name : constant UXString := "Crud_" & From_UTF_8 (Index'Image).Delete (1, 1);
            begin
               if Data.Parent_Id = Parent_Id then
                  Gnoga.Gui.Element.Common.Button_Access (Button).Create (Instance.Tools_Container.all, Data.Name);
                  Button.Dynamic;
                  Button.On_Click_Handler (Data.Handler);
                  Instance.Tools_Container.Add_Element (Button_Name, Button);
               end if;
            end;
         end loop;
         Instance.Tools_Container.all.Style ("max-height", "calc(100% -" & From_UTF_8 (Vertical_Offset'Image) & "px)");
         Instance.Tools_Container.all.Top (Vertical_Offset);
         Instance.Is_Opened := True;
         Instance.Current_Root := Parent_Id;
      else
         Instance.Is_Opened := False;
         Instance.Current_Root := Root_Parent_Id;
      end if;

      for Button_Id in 1 .. (Instance.Next_Id - 1) loop
         if Instance.Menu_Table (Button_Id).Parent_Id = Root_Parent_Id then
            declare
               Button : constant Gnoga.Gui.Element.Common.Button_Access :=
                 Gnoga.Gui.Element.Common.Button_Access
                   (Instance.Tools_Roots_Container.Element ("Crud_" & From_UTF_8 (Button_Id'Image).Delete (1, 1)));
            begin
               if Button_Id = Instance.Current_Root then
                  Button.Class_Name ("toolbar-selected");
               else
                  Button.Class_Name ("");
               end if;
            end;
         end if;
      end loop;
   end Open_Root;

   function HTML_Icon
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
      return UXString
   is
      Data : constant Data_Type := Instance.Menu_Table (Unique_Id);
   begin
      return From_UTF_8 ("<img class=""tools-icon"" src=""") & Data.Icon_SRC & From_UTF_8 (""">");
   end HTML_Icon;

   function Complete_Name
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer;
      Name      :        UXString;
      Shortcut  :        Unicode_Character)
      return UXString
   is
      Result : UXString           := "";
      Data   : constant Data_Type := Instance.Menu_Table (Unique_Id);
      Marked : Boolean := False;
   begin
      if Data.Parent_Id = Root_Parent_Id then
         Result := Result & HTML_Icon (Instance, Unique_Id);
      end if;
      Result := Result & From_UTF_8 ("<span>");
      for Index in 1 .. Name.Length loop
         if Name (Index) = Shortcut and then not Marked then
            Result := Result & From_UTF_8 ("<span class=""shortcut"">") & Name (Index) & From_UTF_8 ("</span>");
            Marked := True;
         elsif Name (Index) /= Force_Shortcut_Char then
            Result := Result & Name (Index);
         end if;
      end loop;
      return Result & From_UTF_8 ("</span>");
   end Complete_Name;

   procedure Hide_Tools_Text (Instance : in out Crud_Type) is
      Button_Name : UXString := "";
      Data        : Data_Type;
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_Id = Root_Parent_Id then
            Button_Name := "Crud_" & From_UTF_8 (Index'Image).Delete (1, 1);
            Instance.Tools_Roots_Container.Element (Button_Name).Inner_HTML (HTML_Icon (Instance, Index));
         end if;
      end loop;
   end Hide_Tools_Text;

   procedure Show_Tools_Text (Instance : in out Crud_Type) is
      Button_Name : UXString := "";
      Data        : Data_Type;
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_Id = Root_Parent_Id then
            Button_Name := "Crud_" & From_UTF_8 (Index'Image).Delete (1, 1);
            Instance.Tools_Roots_Container.Element (Button_Name).Inner_HTML (Data.Name);
         end if;
      end loop;
   end Show_Tools_Text;

   -----------------------------------------------------------------------------
   --  Shortcuts
   -----------------------------------------------------------------------------
   function Is_Shortcut_Available
     (Instance  : in out Crud_Type;
      Parent_Id :        Integer;
      Shortcut  :        Unicode_Character)
      return Boolean
   is
      Shortcut_Id : constant Integer := Code (Shortcut);
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         if Instance.Menu_Table (Index).Shortcut_Id = Shortcut_Id then
            if Instance.Menu_Table (Index).Parent_Id = Root_Parent_Id then
               return False;
            elsif Instance.Menu_Table (Index).Parent_Id = Parent_Id then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Shortcut_Available;

   function Find_Shortcut
     (Instance  : in out Crud_Type;
      Parent_Id :        Integer;
      Name      :        UXString)
      return Unicode_Character
   is
      Default : constant Unicode_Character := '°';
      Char    : Unicode_Character          := Default;
      Result  : Unicode_Character          := Default;
   begin
      for Index in 1 .. Name.Length loop
         Char := Name (Index);
         if Is_Alpha (Char) and Result = Default then
            if Is_Shortcut_Available (Instance, Parent_Id, Char) then
               Result := Char;
            end if;
         elsif Char = Force_Shortcut_Char and then Index /= Name.Length then
            if Is_Alpha (Name (Index + 1)) then
               if Is_Shortcut_Available (Instance, Parent_Id, Name (Index + 1)) then
                  return Name (Index + 1);
               else
                  Gnoga.Log (Name);
                  raise CRUD_Error with "Forced shortcut already exists";
               end if;
            else
               raise CRUD_Error with "'~' needs to be placed before a letter";
            end if;
         end if;
      end loop;
      if Result = Default then
         raise CRUD_Error with "Could not find any valid shortcut";
      end if;
      return Result;
   end Find_Shortcut;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   procedure Create
     (Instance  : in out Crud_Type;
      Parent    : in out Gnoga.Gui.View.View_Type;
      On_Resize :        Gnoga.Gui.Base.Action_Event;
      On_Click  :        Gnoga.Gui.Base.Action_Event)
   is
      Collapse_Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
      Container       : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.View.View_Type;
      Root_Container  : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.View.View_Type;
   begin
      Gnoga.Gui.Element.Common.Button_Access (Collapse_Button).Create
        (Parent, "<img class=""tools-icon"" src=""css/icons/left_panel_open.png"">");
      Collapse_Button.Dynamic;
      Instance.Expand_Collapse_Button := Gnoga.Gui.Element.Common.Button_Access (Collapse_Button);
      Instance.Expand_Collapse_Button.Class_Name ("expand-collapse-button");
      Instance.Expand_Collapse_Button.On_Click_Handler (On_Resize);
      Instance.Parent := Parent'Unrestricted_Access;

      Gnoga.Gui.View.View_Access (Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("tools-container", Container);
      Container.Class_Name ("tools-container");
      Container.Dynamic;
      Container.jQuery_Execute ("data('gnoga_is_opened', false)");
      Container.jQuery_Execute ("data('gnoga_root_id', -1)");
      Instance.Tools_Container := Gnoga.Gui.View.View_Access (Container);

      Gnoga.Gui.View.View_Access (Root_Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("tools-roots-container", Root_Container);
      Root_Container.Class_Name ("tools-roots-container");
      Root_Container.Style ("height", "calc(100% - " & Instance.Expand_Collapse_Button.Minimum_Height & " - 8px)");
      Root_Container.Dynamic;
      Instance.Tools_Roots_Container := Gnoga.Gui.View.View_Access (Root_Container);

      Instance.On_Click := On_Click;
   end Create;

   procedure Load (Instance : in out Crud_Type) is
   begin
      Instance.Is_Opened := False;
      for Data_Id in 1 .. (Instance.Next_Id - 1) loop
         declare
            Button_Name : UXString;
            Data        : constant Data_Type                                  := Instance.Menu_Table (Data_Id);
            Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
            Text : constant UXString := (if Instance.Is_Expanded then Data.Name else HTML_Icon (Instance, Data_Id));
         begin
            if Data.Parent_Id = Root_Parent_Id then
               Button_Name := "Crud_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
               Gnoga.Gui.Element.Common.Button_Access (Button).Create
                 (Instance.Tools_Roots_Container.all, Text);
               Button.Dynamic;
               Button.Style ("display", "flex");
               Button.Style ("align-items", "center");
               Button.jQuery_Execute ("data('gnoga_id', " & From_UTF_8 (Data_Id'Image) & " )");
               Instance.Tools_Roots_Container.Add_Element (Button_Name, Button);
               Button.On_Click_Handler (Instance.On_Click);
            end if;
         end;
      end loop;
   end Load;

   procedure Clear (Instance : in out Crud_Type) is
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         Remove_Button (Instance.Tools_Container, Index);
         Remove_Button (Instance.Tools_Roots_Container, Index);
      end loop;
      Instance.Next_Id := 1;
   end Clear;

   function Add_Root
     (Instance : in out Crud_Type;
      Name     :        UXString;
      Icon_SRC :        UXString := "")
      return Integer
   is
   begin
      Instance.Menu_Table (Instance.Next_Id).Icon_SRC := Icon_SRC;
      return Add_Child (Instance, Name, Root_Parent_Id);
   end Add_Root;

   function Add_Child
     (Instance  : in out Crud_Type;
      Name      :        UXString;
      Parent_Id :        Integer;
      Handler   :        Gnoga.Gui.Base.Action_Event := null)
      return Integer
   is
      Shortcut    : constant Unicode_Character := Find_Shortcut (Instance, Parent_Id, Name);
      Shortcut_Id : constant Integer           := Code (Shortcut);
   begin
      Instance.Menu_Table (Instance.Next_Id).Parent_Id   := Parent_Id;
      Instance.Menu_Table (Instance.Next_Id).Name        := Complete_Name (Instance, Instance.Next_Id, Name, Shortcut);
      Instance.Menu_Table (Instance.Next_Id).Handler     := Handler;
      Instance.Menu_Table (Instance.Next_Id).Shortcut_Id := Shortcut_Id;
      Instance.Active_Menu (Instance.Next_Id) := (Parent_Id = Root_Parent_Id);
      Instance.Active_Shortcuts (Shortcut_Id) := (Parent_Id = Root_Parent_Id);

      Instance.Next_Id := Instance.Next_Id + 1;
      return Instance.Next_Id - 1;
   end Add_Child;

   procedure Add_Delimiter (Parent_Id : Integer) is
   begin
      --  Figure out what to put in here (div with bg color ?)
      null;
   end Add_Delimiter;

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------
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

   -----------------------------------------------------------------------------
   --  Getters
   -----------------------------------------------------------------------------
   function Menu_Name
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
      return UXString
   is
   begin
      return Instance.Menu_Table (Unique_Id).Name;
   end Menu_Name;

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------
   procedure On_Key_Pressed
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
            for Index in 1 .. (Instance.Next_Id - 1) loop
               Data := Instance.Menu_Table (Index);
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
   end On_Key_Pressed;

   procedure Notify_Root_Click
     (Instance : in out Crud_Type;
      Object   : in out Gnoga.Gui.Base.Base_Type'Class)
   is
      Parent_Id : constant Integer := Value (Object.jQuery_Execute ("data('gnoga_id')"));
   begin
      Open_Root (Instance, Parent_Id);
   end Notify_Root_Click;

   procedure Notify_Click
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
   is
   begin
      Open_Root (Instance, Instance.Menu_Table (Unique_Id).Parent_Id);
   end Notify_Click;

   procedure Notify_Key_Pressed
     (Instance : in out Crud_Type;
      Key      :        Character)
   is
      Index : Integer;
      Data  : Data_Type;

      function Find_Associated_Data return Integer is
      begin
         for Index in 1 .. (Instance.Next_Id - 1) loop
            if Instance.Active_Menu (Index) and then Instance.Menu_Table (Index).Shortcut_Id = Code (Key) then
               return Index;
            end if;
         end loop;
         return -1;
      end Find_Associated_Data;

   begin
      if Is_Alpha (Key) then
         if Instance.Active_Shortcuts (Code (Key)) then
            Index := Find_Associated_Data;
            if Index /= -1 then
               Data := Instance.Menu_Table (Index);
               if Data.Parent_Id = Root_Parent_Id then
                  Open_Root (Instance, Index);
               else
                  Data.Handler (Instance.Parent.all);
               end if;
            end if;
         end if;
      end if;
   end Notify_Key_Pressed;

   procedure Notify_Resize (Instance : in out Crud_Type) is
   begin
      if Instance.Is_Expanded then
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
      Instance.Is_Expanded := not Instance.Is_Expanded;
   end Notify_Resize;

end Crud;
