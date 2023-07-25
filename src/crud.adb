with Ada.Wide_Wide_Characters.Unicode;
with GNAT.Case_Util;
with UXStrings.Conversions; use UXStrings.Conversions;

package body CRUD is

   package Element renames Gnoga.Gui.Element;
   package Common renames Gnoga.Gui.Element.Common;

   CRUD_Error : exception;

   -----------------------------------------------------------------------------
   --  Conversions
   -----------------------------------------------------------------------------

   function Value is new Integer_Value (Integer);

   function To_UXString
     (Value : Integer)
      return UXString
   is
   begin
      return From_UTF_8 (Value'Image).Delete (1, 1);
   end To_UXString;

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
      Last_Lower  : constant Unicode_Character := 'z';
      First_Upper : constant Unicode_Character := 'A';
      Last_Upper  : constant Unicode_Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Alpha;

   function Is_Alpha
     (Char : Character)
      return Boolean
   is
      First_Lower : constant Character := 'a';
      Last_Lower  : constant Character := 'z';
      First_Upper : constant Character := 'A';
      Last_Upper  : constant Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Alpha;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------

   function Button_Name (Index : Integer) return UXString is
   begin
      return "Crud_" & To_UXString (Index);
   end Button_Name;

   function Delimiter_Name (Index : Integer) return UXString is
   begin
      return "Delimiter_" & To_UXString (Index);
   end Delimiter_Name;

   function HTML_Icon
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
      return UXString
   is
      Data : constant Data_Type := Instance.Menu_Table (Unique_Id);
   begin
      return "<img class=""crud-icon"" src=""" & Data.Icon_SRC & """>";
   end HTML_Icon;

   function HTML_Complete
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer;
      Name      :        UXString;
      Shortcut  :        Unicode_Character)
      return UXString
   is
      Result : UXString           := "";
      Data   : constant Data_Type := Instance.Menu_Table (Unique_Id);
      Marked : Boolean            := False;
   begin
      if Data.Parent_Id = Root_Parent_Id then
         Result := Result & HTML_Icon (Instance, Unique_Id);
      end if;
      Result := Result & "<span>";
      for Index in 1 .. Name.Length loop
         if Name (Index) = Shortcut and then not Marked then
            Result := Result & "<span class=""crud-shortcut"">" & Name (Index) & "</span>";
            Marked := True;
         elsif Name (Index) /= Force_Shortcut_Char then
            Result := Result & Name (Index);
         end if;
      end loop;
      return Result & "</span>";
   end HTML_Complete;

   procedure Remove_Button
     (Parent  : View.View_Access;
      Data_Id : Integer)
   is
   begin
      if Parent.Element (Button_Name (Data_Id)) /= null then
         Parent.Element (Button_Name (Data_Id)).Remove;
      end if;
      if Parent.Element (Delimiter_Name (Data_Id)) /= null then
         Parent.Element (Delimiter_Name (Data_Id)).Remove;
      end if;
   end Remove_Button;

   procedure Open_Element
     (Instance  : in out Crud_Type;
      Parent_Id :        Integer)
   is
      Clicked_Button : constant Common.Button_Access :=
        Common.Button_Access (Instance.Elements_Parent.Element (Button_Name (Parent_Id)));
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
            Remove_Button (Instance.Sub_Elements_Parent, Index);
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
               Data        : constant Data_Type                        := Instance.Menu_Table (Index);
               Button      : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
            begin
               if Data.Parent_Id = Parent_Id then
                  if Data.Delimiter_Above then
                     declare
                        Delimiter : constant Element.Pointer_To_Element_Class := new View.View_Type;
                     begin
                        View.View_Access (Delimiter).Create (Instance.Sub_Elements_Parent.all);
                        Delimiter.Class_Name ("crud-delimiter");
                        Instance.Sub_Elements_Parent.Add_Element (Delimiter_Name (Index), Delimiter);
                     end;
                  end if;
                  Common.Button_Access (Button).Create (Instance.Sub_Elements_Parent.all, Data.Name);
                  Button.Class_Name ("framework-button");
                  Button.Dynamic;

                  if not Data.Clickable then
                     Button.Add_Class ("unclickable");
                  end if;

                  Button.On_Click_Handler (Data.Handler);
                  Instance.Sub_Elements_Parent.Add_Element (Button_Name (Index), Button);
               end if;
            end;
         end loop;
         Instance.Sub_Elements_Parent.all.Style ("max-height", "calc(100% - " & To_UXString (Vertical_Offset) & "px)");
         Instance.Sub_Elements_Parent.all.Top (Vertical_Offset);
         Instance.Is_Opened    := True;
         Instance.Current_Root := Parent_Id;
      else
         Instance.Is_Opened    := False;
         Instance.Current_Root := Root_Parent_Id;
      end if;

      for Button_Id in 1 .. (Instance.Next_Id - 1) loop
         if Instance.Menu_Table (Button_Id).Parent_Id = Root_Parent_Id then
            declare
               Button : constant Common.Button_Access :=
                 Common.Button_Access (Instance.Elements_Parent.Element (Button_Name (Button_Id)));
            begin
               if Button_Id = Instance.Current_Root then
                  Button.Add_Class ("crud-selected-button");
               else
                  Button.Remove_Class ("crud-selected-button");
               end if;
            end;
         end if;
      end loop;
   end Open_Element;

   procedure Hide_Elements_Text (Instance : in out Crud_Type) is
      Data        : Data_Type;
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_Id = Root_Parent_Id then
            Instance.Elements_Parent.Element (Button_Name (Index)).Inner_HTML (HTML_Icon (Instance, Index));
         end if;
      end loop;
   end Hide_Elements_Text;

   procedure Show_Elements_Text (Instance : in out Crud_Type) is
      Data        : Data_Type;
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_Id = Root_Parent_Id then
            Instance.Elements_Parent.Element (Button_Name (Index)).Inner_HTML (Data.Name);
         end if;
      end loop;
   end Show_Elements_Text;

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
      Parent    : in out View.View_Type;
      On_Resize :        Base.Action_Event;
      On_Click  :        Base.Action_Event)
   is
      Extend_Shrink_Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
      Container            : constant Element.Pointer_To_Element_Class := new View.View_Type;
      Root_Container       : constant Element.Pointer_To_Element_Class := new View.View_Type;
   begin
      Common.Button_Access (Extend_Shrink_Button).Create
        (Parent, "<img class=""crud-icon"" src=""css/icons/left_panel_open.png"">");
      Extend_Shrink_Button.Dynamic;
      Instance.Extend_Shrink_Button := Common.Button_Access (Extend_Shrink_Button);
      Instance.Extend_Shrink_Button.Class_Name ("framework-button");
      Instance.Extend_Shrink_Button.Add_Class ("crud-extend-shrink-button");
      Instance.Extend_Shrink_Button.On_Click_Handler (On_Resize);
      Instance.Parent := Parent'Unrestricted_Access;

      View.View_Access (Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("subelements", Container);
      Container.Class_Name ("crud-sub-elements-parent");
      Container.Dynamic;
      Container.jQuery_Execute ("data('gnoga_is_opened', false)");
      Container.jQuery_Execute ("data('gnoga_root_id', -1)");
      Instance.Sub_Elements_Parent := View.View_Access (Container);

      View.View_Access (Root_Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("elements", Root_Container);
      Root_Container.Class_Name ("crud-elements-parent");
      Root_Container.Style ("height", "calc(100% - " & Instance.Extend_Shrink_Button.Minimum_Height & " - 8px)");
      Root_Container.Dynamic;
      Instance.Elements_Parent := View.View_Access (Root_Container);

      Instance.On_Click := On_Click;
   end Create;

   procedure Load (Instance : in out Crud_Type) is
      Data : Data_Type;
   begin
      Instance.Is_Opened := False;
      for Data_Id in 1 .. (Instance.Next_Id - 1) loop
         Data := Instance.Menu_Table (Data_Id);
         declare
            Button      : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
            Text : constant UXString := (if Instance.Is_Expanded then Data.Name else HTML_Icon (Instance, Data_Id));
         begin
            if Data.Parent_Id = Root_Parent_Id then
               Common.Button_Access (Button).Create (Instance.Elements_Parent.all, Text);
               Button.Class_Name ("framework-button");
               Button.Dynamic;

               if not Data.Clickable then
                  Button.Add_Class ("unclickable");
               end if;

               Button.Style ("display", "flex");
               Button.Style ("align-items", "center");
               Button.jQuery_Execute ("data('gnoga_id', " & To_UXString (Data_Id) & " )");
               Instance.Elements_Parent.Add_Element (Button_Name (Data_Id), Button);
               Button.On_Click_Handler (Instance.On_Click);
            end if;
         end;
      end loop;
   end Load;

   procedure Clear (Instance : in out Crud_Type) is
   begin
      for Index in 1 .. (Instance.Next_Id - 1) loop
         Remove_Button (Instance.Sub_Elements_Parent, Index);
         Remove_Button (Instance.Elements_Parent, Index);
      end loop;
      Instance.Next_Id := 1;
   end Clear;

   function Add_Element
     (Instance : in out Crud_Type;
      Name     :        UXString;
      Icon_SRC :        UXString)
      return Integer
   is
   begin
      Instance.Menu_Table (Instance.Next_Id).Icon_SRC := Icon_SRC;
      return Add_Sub_Element (Instance, Name, Root_Parent_Id);
   end Add_Element;

   function Add_Sub_Element
     (Instance  : in out Crud_Type;
      Name      :        UXString;
      Parent_Id :        Integer;
      Handler   :        Base.Action_Event := null)
      return Integer
   is
      Shortcut    : constant Unicode_Character := Find_Shortcut (Instance, Parent_Id, Name);
      Shortcut_Id : constant Integer           := Code (Shortcut);
   begin
      Instance.Menu_Table (Instance.Next_Id).Parent_Id       := Parent_Id;
      Instance.Menu_Table (Instance.Next_Id).Name := HTML_Complete (Instance, Instance.Next_Id, Name, Shortcut);
      Instance.Menu_Table (Instance.Next_Id).Handler         := Handler;
      Instance.Menu_Table (Instance.Next_Id).Shortcut_Id     := Shortcut_Id;
      Instance.Menu_Table (Instance.Next_Id).Clickable       := True;
      Instance.Menu_Table (Instance.Next_Id).Delimiter_Above := False;
      Instance.Active_Menu (Instance.Next_Id)                := (Parent_Id = Root_Parent_Id);
      Instance.Active_Shortcuts (Shortcut_Id)                := (Parent_Id = Root_Parent_Id);

      Instance.Next_Id := Instance.Next_Id + 1;
      return Instance.Next_Id - 1;
   end Add_Sub_Element;

   procedure Add_Delimiter_Above
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
   is
   begin
      if Instance.Menu_Table (Unique_Id).Parent_Id /= Root_Parent_Id then
         Instance.Menu_Table (Unique_Id).Delimiter_Above := True;
      end if;
   end Add_Delimiter_Above;

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------

   procedure Set_Unclickable
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
   is
      Sub_Elm : constant Element.Pointer_To_Element_Class := Instance.Sub_Elements_Parent.Element (Button_Name (Unique_Id));
      Elm : constant Element.Pointer_To_Element_Class := Instance.Elements_Parent.Element (Button_Name (Unique_Id));
   begin
      Instance.Menu_Table (Unique_Id).Clickable := False;
      if Elm /= null then
         Elm.Add_Class ("unclickable");
      elsif Sub_Elm /= null then
         Sub_Elm.Add_Class ("unclickable");
      end if;
   end Set_Unclickable;

   procedure Set_Clickable
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
   is
      Sub_Elm : constant Element.Pointer_To_Element_Class := Instance.Sub_Elements_Parent.Element (Button_Name (Unique_Id));
      Elm : constant Element.Pointer_To_Element_Class := Instance.Elements_Parent.Element (Button_Name (Unique_Id));
   begin
      Instance.Menu_Table (Unique_Id).Clickable := True;
      if Elm /= null then
         Elm.Remove_Class ("unclickable");
      elsif Sub_Elm /= null then
         Sub_Elm.Remove_Class ("unclickable");
      end if;
   end Set_Clickable;

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   procedure Notify_Element_Click
     (Instance : in out Crud_Type;
      Object   : in out Base.Base_Type'Class)
   is
      Parent_Id : constant Integer := Value (Object.jQuery_Execute ("data('gnoga_id')"));
   begin
      Open_Element (Instance, Parent_Id);
   end Notify_Element_Click;

   procedure Notify_Sub_Element_Click
     (Instance  : in out Crud_Type;
      Unique_Id :        Integer)
   is
   begin
      Open_Element (Instance, Instance.Menu_Table (Unique_Id).Parent_Id);
   end Notify_Sub_Element_Click;

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
      if Is_Alpha (Key) and then Instance.Active_Shortcuts (Code (Key)) then
         Index := Find_Associated_Data;
         if Index /= -1 then
            Data := Instance.Menu_Table (Index);
            if Data.Clickable then
               if Data.Parent_Id = Root_Parent_Id then
                  Open_Element (Instance, Index);
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
         Instance.Extend_Shrink_Button.Inner_HTML ("<img class=""crud-icon"" src=""css/icons/left_panel_open.png"">");
         Instance.Parent.Remove_Class ("crud-force-extend");
         Instance.Hide_Elements_Text;
      else
         Instance.Extend_Shrink_Button.Inner_HTML
           ("<img class=""crud-icon"" src=""css/icons/left_panel_close.png""><span>Fermer</span>");
         Instance.Parent.Add_Class ("crud-force-extend");
         Instance.Show_Elements_Text;
      end if;
      Instance.Is_Expanded := not Instance.Is_Expanded;
   end Notify_Resize;

end CRUD;
