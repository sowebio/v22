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

   function Is_Letter
     (Char : Unicode_Character)
      return Boolean
   is
      First_Lower : constant Unicode_Character := 'a';
      Last_Lower  : constant Unicode_Character := 'z';
      First_Upper : constant Unicode_Character := 'A';
      Last_Upper  : constant Unicode_Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Letter;

   function Is_Letter
     (Char : Character)
      return Boolean
   is
      First_Lower : constant Character := 'a';
      Last_Lower  : constant Character := 'z';
      First_Upper : constant Character := 'A';
      Last_Upper  : constant Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Letter;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------

   function Button_Name
     (Index : Integer)
      return UXString
   is
   begin
      return "Crud_" & To_UXString (Index);
   end Button_Name;

   function Delimiter_Name
     (Index : Integer)
      return UXString
   is
   begin
      return "Delimiter_" & To_UXString (Index);
   end Delimiter_Name;

   function HTML_Icon
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer)
      return UXString
   is
      Data : constant Data_Type := Instance.Menu_Table (Unique_ID);
   begin
      return "<img class=""crud-icon"" src=""" & Data.Icon_SRC & """>";
   end HTML_Icon;

   function HTML_Complete
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer;
      Name      :        UXString;
      Shortcut  :        Unicode_Character)
      return UXString
   is
      Result : UXString           := "";
      Data   : constant Data_Type := Instance.Menu_Table (Unique_ID);
      Marked : Boolean            := False;
   begin
      if Data.Parent_ID = Root_Parent_ID then
         Result := Result & HTML_Icon (Instance, Unique_ID);
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
      Data_ID : Integer)
   is
   begin
      if Parent.Element (Button_Name (Data_ID)) /= null then
         Parent.Element (Button_Name (Data_ID)).Remove;
      end if;
      if Parent.Element (Delimiter_Name (Data_ID)) /= null then
         Parent.Element (Delimiter_Name (Data_ID)).Remove;
      end if;
   end Remove_Button;

   procedure Highlight_Selected_Element (Instance : in out CRUD_Type) is
   begin
      for Button_ID in 1 .. Instance.Last_ID loop
         if Instance.Menu_Table (Button_ID).Parent_ID = Root_Parent_ID then
            declare
               Button : constant Common.Button_Access :=
                 Common.Button_Access (Instance.Elements_Parent.Element (Button_Name (Button_ID)));
            begin
               if Button_ID = Instance.Current_Root then
                  Button.Add_Class ("crud-selected-button");
               else
                  Button.Remove_Class ("crud-selected-button");
               end if;
            end;
         end if;
      end loop;
   end Highlight_Selected_Element;

   procedure Update_Shortcuts
     (Instance  : in out CRUD_Type;
      Parent_ID :        Integer)
   is
   begin
      for Index in 1 .. Instance.Last_ID loop
         Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_ID) := False;
         Instance.Active_Menu (Index)                                        := False;
         if Instance.Menu_Table (Index).Parent_ID = Root_Parent_ID then
            Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_ID) := True;
            Instance.Active_Menu (Index)                                        := True;
         else
            Remove_Button (Instance.Sub_Elements_Parent, Index);
         end if;
      end loop;

      for Index in 1 .. Instance.Last_ID loop
         if Instance.Is_Opened and then Instance.Menu_Table (Index).Parent_ID = Parent_ID then
            Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_ID) := True;
            Instance.Active_Menu (Index)                                        := True;
         end if;
      end loop;
   end Update_Shortcuts;

   procedure Open_Element
     (Instance  : in out CRUD_Type;
      Parent_ID :        Integer)
   is
      Data    : Data_Type;
      Clicked : constant Common.Button_Access :=
        Common.Button_Access (Instance.Elements_Parent.Element (Button_Name (Parent_ID)));
      Offset : constant Integer := Clicked.Offset_From_Top - Instance.Parent.Offset_From_Top;
   begin
      Instance.Is_Opened := not (Instance.Is_Opened and then (Instance.Current_Root = Parent_ID));
      Update_Shortcuts (Instance, Parent_ID);
      if Instance.Is_Opened then
         for Index in 1 .. Instance.Last_ID loop
            Data := Instance.Menu_Table (Index);
            if Data.Parent_ID = Parent_ID then
               declare
                  Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
               begin
                  if Data.Delimiter_Above then
                     declare
                        Delimiter : constant Element.Pointer_To_Element_Class := new View.View_Type;
                     begin
                        View.View_Access (Delimiter).Create (Instance.Sub_Elements_Parent.all);
                        Delimiter.Class_Name ("crud-delimiter");
                        Instance.Sub_Elements_Parent.Add_Element (Delimiter_Name (Index), Delimiter);
                     end;
                  end if;

                  Common.Button_Access (Button).Create (Instance.Sub_Elements_Parent.all, Data.HTML);
                  Instance.Sub_Elements_Parent.Add_Element (Button_Name (Index), Button);
                  Button.Class_Name ("framework-button");
                  Button.On_Click_Handler (Data.Handler);
                  Button.Dynamic;

                  if not Data.Clickable then
                     Button.Add_Class ("unclickable");
                  end if;
               end;
            end if;
         end loop;
         Instance.Sub_Elements_Parent.all.Style ("max-height", "calc(100% - " & To_UXString (Offset) & "px)");
         Instance.Sub_Elements_Parent.all.Top (Offset);
         Instance.Current_Root := Parent_ID;
      else
         Instance.Current_Root := Root_Parent_ID;
      end if;

      Highlight_Selected_Element (Instance);
   end Open_Element;

   procedure Hide_Elements_Text (Instance : in out CRUD_Type) is
      Data : Data_Type;
   begin
      for Index in 1 .. Instance.Last_ID loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_ID = Root_Parent_ID then
            Instance.Elements_Parent.Element (Button_Name (Index)).Inner_HTML (HTML_Icon (Instance, Index));
         end if;
      end loop;
   end Hide_Elements_Text;

   procedure Show_Elements_Text (Instance : in out CRUD_Type) is
      Data : Data_Type;
   begin
      for Index in 1 .. Instance.Last_ID loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_ID = Root_Parent_ID then
            Instance.Elements_Parent.Element (Button_Name (Index)).Inner_HTML (Data.HTML);
         end if;
      end loop;
   end Show_Elements_Text;

   -----------------------------------------------------------------------------
   --  Shortcuts
   -----------------------------------------------------------------------------

   function Is_Shortcut_Available
     (Instance  : in out CRUD_Type;
      Parent_ID :        Integer;
      Shortcut  :        Unicode_Character)
      return Boolean
   is
      Shortcut_ID : constant Integer := Code (Shortcut);
   begin
      for Index in 1 .. Instance.Last_ID loop
         if Instance.Menu_Table (Index).Shortcut_ID = Shortcut_ID then
            if Instance.Menu_Table (Index).Parent_ID = Root_Parent_ID then
               return False;
            elsif Instance.Menu_Table (Index).Parent_ID = Parent_ID then
               return False;
            end if;
         end if;
      end loop;
      return True;
   end Is_Shortcut_Available;

   function Find_Shortcut
     (Instance  : in out CRUD_Type;
      Parent_ID :        Integer;
      Name      :        UXString)
      return Unicode_Character
   is
      Default : constant Unicode_Character := '°';
      Char    : Unicode_Character          := Default;
      Result  : Unicode_Character          := Default;
   begin
      for Index in 1 .. Name.Length loop
         Char := Name (Index);
         if Is_Letter (Char) and Result = Default then
            if Is_Shortcut_Available (Instance, Parent_ID, Char) then
               Result := Char;
            end if;
         elsif Char = Force_Shortcut_Char and then Index /= Name.Length then
            if Is_Letter (Name (Index + 1)) then
               if Is_Shortcut_Available (Instance, Parent_ID, Name (Index + 1)) then
                  return Name (Index + 1);
               else
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
     (Instance  : in out CRUD_Type;
      Parent    : in out View.View_Type;
      On_Resize :        Base.Action_Event;
      On_Click  :        Base.Action_Event)
   is
      Extend_Shrink  : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
      Container      : constant Element.Pointer_To_Element_Class := new View.View_Type;
      Root_Container : constant Element.Pointer_To_Element_Class := new View.View_Type;
   begin
      Instance.Parent   := Parent'Unrestricted_Access;
      Instance.On_Click := On_Click;

      Common.Button_Access (Extend_Shrink).Create
        (Parent, "<img class=""crud-icon"" src=""css/icons/left_panel_open.png"">");
      Extend_Shrink.Class_Name ("framework-button");
      Extend_Shrink.Add_Class ("crud-extend-shrink-button");
      Extend_Shrink.On_Click_Handler (On_Resize);
      Extend_Shrink.Dynamic;
      Instance.Extend_Shrink_Button := Common.Button_Access (Extend_Shrink);

      View.View_Access (Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("subelements", Container);
      Container.Class_Name ("crud-sub-elements-parent");
      Container.jQuery_Execute ("data('gnoga_is_opened', false)");
      Container.jQuery_Execute ("data('gnoga_root_id', -1)");
      Container.Dynamic;
      Instance.Sub_Elements_Parent := View.View_Access (Container);

      View.View_Access (Root_Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("elements", Root_Container);
      Root_Container.Class_Name ("crud-elements-parent");
      Root_Container.Style ("height", "calc(100% - " & Instance.Extend_Shrink_Button.Minimum_Height & " - 8px)");
      Root_Container.Dynamic;
      Instance.Elements_Parent := View.View_Access (Root_Container);
   end Create;

   procedure Load (Instance : in out CRUD_Type) is
      Data   : Data_Type;
      Text   : UXString;
      Parent : constant View.View_Access := Instance.Elements_Parent;
   begin
      for Data_ID in 1 .. Instance.Last_ID loop
         Data := Instance.Menu_Table (Data_ID);
         Text := Data.HTML;
         if not Instance.Is_Extended then
            Text := HTML_Icon (Instance, Data_ID);
         end if;
         if Data.Parent_ID = Root_Parent_ID then
            declare
               Button : constant Element.Pointer_To_Element_Class := new Common.Button_Type;
            begin
               Common.Button_Access (Button).Create (Parent.all, Text);
               Button.Class_Name ("framework-button");
               Button.Style ("display", "flex");
               Button.Style ("align-items", "center");
               Button.jQuery_Execute ("data('gnoga_id', " & To_UXString (Data_ID) & " )");
               Button.On_Click_Handler (Instance.On_Click);
               Button.Dynamic;
               Parent.Add_Element (Button_Name (Data_ID), Button);

               if not Data.Clickable then
                  Button.Add_Class ("unclickable");
               end if;
            end;
         end if;
      end loop;
   end Load;

   procedure Clear (Instance : in out CRUD_Type) is
   begin
      for Index in 1 .. Instance.Last_ID loop
         Remove_Button (Instance.Sub_Elements_Parent, Index);
         Remove_Button (Instance.Elements_Parent, Index);
      end loop;
      Instance.Last_ID := 0;
   end Clear;

   function Add_Element
     (Instance : in out CRUD_Type;
      Name     :        UXString;
      Icon_SRC :        UXString)
      return Integer
   is
   begin
      Instance.Menu_Table (Instance.Last_ID + 1).Icon_SRC := Icon_SRC;
      return Add_Sub_Element (Instance, Name, Root_Parent_ID);
   end Add_Element;

   function Add_Sub_Element
     (Instance  : in out CRUD_Type;
      Name      :        UXString;
      Parent_ID :        Integer;
      Handler   :        Base.Action_Event := null)
      return Integer
   is
      Shortcut    : constant Unicode_Character := Find_Shortcut (Instance, Parent_ID, Name);
      Shortcut_ID : constant Integer           := Code (Shortcut);
   begin
      Instance.Last_ID := Instance.Last_ID + 1;

      Instance.Menu_Table (Instance.Last_ID).Parent_ID       := Parent_ID;
      Instance.Menu_Table (Instance.Last_ID).HTML := HTML_Complete (Instance, Instance.Last_ID, Name, Shortcut);
      Instance.Menu_Table (Instance.Last_ID).Handler         := Handler;
      Instance.Menu_Table (Instance.Last_ID).Shortcut_ID     := Shortcut_ID;
      Instance.Menu_Table (Instance.Last_ID).Clickable       := True;
      Instance.Menu_Table (Instance.Last_ID).Delimiter_Above := False;

      Instance.Active_Menu (Instance.Last_ID) := (Parent_ID = Root_Parent_ID);
      Instance.Active_Shortcuts (Shortcut_ID) := (Parent_ID = Root_Parent_ID);

      return Instance.Last_ID;
   end Add_Sub_Element;

   procedure Add_Delimiter_Above
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer)
   is
   begin
      if Instance.Menu_Table (Unique_ID).Parent_ID /= Root_Parent_ID then
         Instance.Menu_Table (Unique_ID).Delimiter_Above := True;
      end if;
   end Add_Delimiter_Above;

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------

   procedure Set_Unclickable
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer)
   is
      Sub_Elm : constant Element.Pointer_To_Element_Class :=
        Instance.Sub_Elements_Parent.Element (Button_Name (Unique_ID));
      Elm : constant Element.Pointer_To_Element_Class := Instance.Elements_Parent.Element (Button_Name (Unique_ID));
   begin
      Instance.Menu_Table (Unique_ID).Clickable := False;
      if Elm /= null then
         Elm.Add_Class ("unclickable");
      elsif Sub_Elm /= null then
         Sub_Elm.Add_Class ("unclickable");
      end if;
   end Set_Unclickable;

   procedure Set_Clickable
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer)
   is
      Sub_Elm : constant Element.Pointer_To_Element_Class :=
        Instance.Sub_Elements_Parent.Element (Button_Name (Unique_ID));
      Elm : constant Element.Pointer_To_Element_Class := Instance.Elements_Parent.Element (Button_Name (Unique_ID));
   begin
      Instance.Menu_Table (Unique_ID).Clickable := True;
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
     (Instance : in out CRUD_Type;
      Object   : in out Base.Base_Type'Class)
   is
      Parent_ID : constant Integer := Value (Object.jQuery_Execute ("data('gnoga_id')"));
   begin
      Open_Element (Instance, Parent_ID);
   end Notify_Element_Click;

   procedure Notify_Sub_Element_Click
     (Instance  : in out CRUD_Type;
      Unique_ID :        Integer)
   is
   begin
      Open_Element (Instance, Instance.Menu_Table (Unique_ID).Parent_ID);
   end Notify_Sub_Element_Click;

   procedure Notify_Key_Pressed
     (Instance : in out CRUD_Type;
      Key      :        Character)
   is
      Index : Integer;
      Data  : Data_Type;

      function Find_Associated_Data return Integer is
      begin
         for Index in 1 .. Instance.Last_ID loop
            if Instance.Active_Menu (Index) and then Instance.Menu_Table (Index).Shortcut_ID = Code (Key) then
               return Index;
            end if;
         end loop;
         return -1;
      end Find_Associated_Data;

   begin
      if Is_Letter (Key) and then Instance.Active_Shortcuts (Code (Key)) then
         Index := Find_Associated_Data;
         if Index /= -1 then
            Data := Instance.Menu_Table (Index);
            if Data.Clickable then
               if Data.Parent_ID = Root_Parent_ID then
                  Open_Element (Instance, Index);
               else
                  Data.Handler (Instance.Parent.all);
               end if;
            end if;
         end if;
      end if;
   end Notify_Key_Pressed;

   procedure Notify_Resize (Instance : in out CRUD_Type) is
   begin
      if Instance.Is_Extended then
         Instance.Extend_Shrink_Button.Inner_HTML ("<img class=""crud-icon"" src=""css/icons/left_panel_open.png"">");
         Instance.Parent.Remove_Class ("crud-force-extend");
         Instance.Hide_Elements_Text;
      else
         Instance.Extend_Shrink_Button.Inner_HTML
           ("<img class=""crud-icon"" src=""css/icons/left_panel_close.png""><span>Fermer</span>");
         Instance.Parent.Add_Class ("crud-force-extend");
         Instance.Show_Elements_Text;
      end if;
      Instance.Is_Extended := not Instance.Is_Extended;
   end Notify_Resize;

end CRUD;
