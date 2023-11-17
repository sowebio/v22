-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-main_menu.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Main_Menu package
--
--  @description
--
--  @authors
--  Théodore Gigault - tg - developpement@soweb.io
--  Arthur Le Floch - alf - developpement@soweb.io
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Wide_Wide_Characters.Unicode;
with GNAT.Case_Util;
with UXStrings.Conversions; use UXStrings.Conversions;

with v22.Msg;

package body v22.Gui.Main_Menu is

   Main_Menu_Error : exception;

   -----------------------------------------------------------------------------
   --  Conversions
   -----------------------------------------------------------------------------

   --  Error: generic subprogram cannot be called
   function Int_Value is new Integer_Value (Integer);

   ----------------------------------------------------------------------------
   function Code (Char : Unicode_Character) return Integer is
      Base : constant Unicode_Character := 'a';
   begin
      return 1 + Unicode_Character'Pos (Ada.Wide_Wide_Characters.Unicode.To_Lower_Case (Char)) - Unicode_Character'Pos (Base);
   end Code;

   function Code (Char : Character) return Integer is
      Base : constant Character := 'a';
   begin
      return 1 + Character'Pos (GNAT.Case_Util.To_Lower (Char)) - Character'Pos (Base);
   end Code;

   ----------------------------------------------------------------------------
   function Is_Letter (Char : Unicode_Character) return Boolean is
      First_Lower : constant Unicode_Character := 'a';
      Last_Lower : constant Unicode_Character := 'z';
      First_Upper : constant Unicode_Character := 'A';
      Last_Upper : constant Unicode_Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Letter;

   function Is_Letter (Char : Character) return Boolean
   is
      First_Lower : constant Character := 'a';
      Last_Lower : constant Character := 'z';
      First_Upper : constant Character := 'A';
      Last_Upper : constant Character := 'Z';
   begin
      return (Char in First_Lower .. Last_Lower) or else (Char in First_Upper .. Last_Upper);
   end Is_Letter;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Button_Name (Index : Integer) return String is
   begin
      return "Main_Menu_" & To_String_Unsigned (Index);
   end Button_Name;

   ----------------------------------------------------------------------------
   function Delimiter_Name (Index : Integer) return String is
   begin
      return "Delimiter_" & To_String_Unsigned (Index);
   end Delimiter_Name;

   ----------------------------------------------------------------------------
   function HTML_Icon (Instance  : in out Main_Menu_Type; Unique_ID : Integer) return String is
      Data : constant Data_Type := Instance.Menu_Table (Unique_ID);
   begin
      return "<img class=""main-menu-icon"" src=""" & Data.Icon_SRC & """>";
   end HTML_Icon;

   ----------------------------------------------------------------------------
   function HTML_Full (Instance : in out Main_Menu_Type; Unique_ID : Integer;
                       Name : String; Shortcut : Unicode_Character) return String is
      Result : String := "";
      Data   : constant Data_Type := Instance.Menu_Table (Unique_ID);
      Marked : Boolean := False;
   begin
      if Data.Parent_ID = Root_Parent_ID then
         Result := Result & HTML_Icon (Instance, Unique_ID);
      end if;
      Result := Result & "<span>";
      for Index in 1 .. Name.Length loop
         if Name (Index) = Shortcut and then not Marked then
            Result := Result & "<span class=""main-menu-shortcut"">" & Name (Index) & "</span>";
            Marked := True;
         elsif Name (Index) /= Force_Shortcut_Char then
            Result := Result & Name (Index);
         end if;
      end loop;
      return Result & "</span>";
   end HTML_Full;

   ----------------------------------------------------------------------------
   procedure Remove_Button (Parent  : GGV.View_Access; Data_ID : Integer) is
   begin
      if Parent.Element (Button_Name (Data_ID)) /= null then
         Parent.Element (Button_Name (Data_ID)).Remove;
      end if;
      if Parent.Element (Delimiter_Name (Data_ID)) /= null then
         Parent.Element (Delimiter_Name (Data_ID)).Remove;
      end if;
   end Remove_Button;

   ----------------------------------------------------------------------------
   procedure Update_Selected_Element (Instance : in out Main_Menu_Type) is
      Button : GGEC.Button_Access;
   begin
      for Button_ID in 1 .. Instance.Last_ID loop
         if Instance.Menu_Table (Button_ID).Parent_ID = Root_Parent_ID then
            Button := GGEC.Button_Access (Instance.Elements_Parent.Element (Button_Name (Button_ID)));
            if Button_ID = Instance.Current_Root then
               Button.Add_Class ("main-menu-selected-button");
            else
               Button.Remove_Class ("main-menu-selected-button");
            end if;
         end if;
      end loop;
   end Update_Selected_Element;

   ----------------------------------------------------------------------------
   procedure Update_Shortcuts (Instance  : in out Main_Menu_Type; Parent_ID : Integer) is
   begin
      for Index in 1 .. Instance.Last_ID loop
         Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_ID) := False;
         Instance.Active_Menu (Index) := False;
         if Instance.Menu_Table (Index).Parent_ID = Root_Parent_ID then
            Instance.Active_Shortcuts (Instance.Menu_Table (Index).Shortcut_ID) := True;
            Instance.Active_Menu (Index) := True;
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

   ----------------------------------------------------------------------------
   procedure Open_Element (Instance : in out Main_Menu_Type; Parent_ID : Integer) is
      Data : Data_Type;
      Clicked : constant GGEC.Button_Access := GGEC.Button_Access (Instance.Elements_Parent.Element (Button_Name (Parent_ID)));
      Offset : constant Integer := Clicked.Offset_From_Top - Instance.Parent.Offset_From_Top;
      Button : GGE.Pointer_To_Element_Class;
      Delimiter : GGE.Pointer_To_Element_Class;
   begin
      Instance.Is_Opened := True;
      Instance.Sub_Elements_Parent.all.Style ("max-height", "calc(100% - " & To_String_Unsigned (Offset) & "px)");
      Instance.Sub_Elements_Parent.all.Top (Offset);
      Instance.Current_Root := Parent_ID;
      Update_Shortcuts (Instance, Parent_ID);
      Update_Selected_Element (Instance);

      for Index in 1 .. Instance.Last_ID loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_ID = Parent_ID then
            -- Display a submenu
            Button := new GGEC.Button_Type;
            if Data.Delimiter_Above then
               Delimiter := new GGV.View_Type;
               GGV.View_Access (Delimiter).Create (Instance.Sub_Elements_Parent.all);
               Delimiter.Class_Name ("main-menu-delimiter");
               Delimiter.Dynamic;
               Instance.Sub_Elements_Parent.Add_Element (Delimiter_Name (Index), Delimiter);
            end if;
            GGEC.Button_Access (Button).Create (Instance.Sub_Elements_Parent.all, Data.HTML);
            Instance.Sub_Elements_Parent.Add_Element (Button_Name (Index), Button);
            Button.Class_Name ("framework-button");
            Button.On_Click_Handler (Data.Handler);
            Button.Dynamic;
            if not Data.Clickable then
               Button.Add_Class ("framework-unclickable");
               Button.On_Click_Handler (null);
            end if;
         else
            --  Display a new menu by calling the registered handler
            if Index = Parent_ID then -- Only for the right command
               if Data.Clickable then
                  if Data.Parent_ID = Root_Parent_ID then
                     if Data.Handler /= null then
                        Instance.Is_Opened := False; -- Reset the submenu opening flag. Must stay above the Data.handler call
                        Data.Handler (Instance.Parent.all);
                        exit; -- To avoid opening, in the new menu, the first time, the submenu of the same number
                     end if;
                  end if;
               end if;
            end if;
         end if;
      end loop;

   end Open_Element;

   ----------------------------------------------------------------------------
   procedure Toggle_Menu_Visibility (Instance  : in out Main_Menu_Type; Parent_ID : Integer) is
      Will_Open : constant Boolean := not (Instance.Current_Root = Parent_ID and then Instance.Is_Opened);
   begin
      --  Msg.Debug ("Toggle_Menu_Visibility");
      if Instance.Is_Opened then
         Close_Menu (Instance);
      end if;
      if Will_Open then
         Open_Element (Instance, Parent_ID);
      end if;
   end Toggle_Menu_Visibility;

   ----------------------------------------------------------------------------
   procedure Hide_Elements_Text (Instance : in out Main_Menu_Type) is
      Data : Data_Type;
   begin
      for Index in 1 .. Instance.Last_ID loop
         Data := Instance.Menu_Table (Index);
         if Data.Parent_ID = Root_Parent_ID then
            Instance.Elements_Parent.Element (Button_Name (Index)).Inner_HTML (HTML_Icon (Instance, Index));
         end if;
      end loop;
   end Hide_Elements_Text;

   ----------------------------------------------------------------------------
   procedure Show_Elements_Text (Instance : in out Main_Menu_Type) is
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

   ----------------------------------------------------------------------------
   function Is_Shortcut_Available (Instance : in out Main_Menu_Type; Parent_ID : Integer;
                                   Shortcut : Unicode_Character) return Boolean is
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

   ----------------------------------------------------------------------------
   function Find_Shortcut (Instance  : in out Main_Menu_Type; Parent_ID : Integer; Name : String) return Unicode_Character is
      Default : constant Unicode_Character := '°';
      Char : Unicode_Character := Default;
      Result : Unicode_Character := Default;
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
                  raise Main_Menu_Error with "Forced shortcut already exists";
               end if;
            else
               raise Main_Menu_Error with "'" & To_Latin_1 (From_Unicode (Force_Shortcut_Char)) &
                                          "' needs to be placed before a letter";
            end if;
         end if;
      end loop;
      if Result = Default then
         raise Main_Menu_Error with "Could not find any valid shortcut";
      end if;
      return Result;
   end Find_Shortcut;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Create (Instance : in out Main_Menu_Type; Parent : in out GGV.View_Type; On_Resize : GGB.Action_Event;
                     On_Click : GGB.Action_Event) is
      Extend_Shrink : constant GGE.Pointer_To_Element_Class := new GGEC.Button_Type;
      Container : constant GGE.Pointer_To_Element_Class := new GGV.View_Type;
      Root_Container : constant GGE.Pointer_To_Element_Class := new GGV.View_Type;
   begin
      Instance.Parent := Parent'Unrestricted_Access;
      Instance.On_Click := On_Click;

      GGEC.Button_Access (Extend_Shrink).Create (Parent, "<img class=" & DQ & "main-menu-icon" & DQ &
                                                               " src=" & DQ & Image_Gnoga_Root.Delete (1, 1) &
                                                               "ico-left_panel_open.png"">");
      Extend_Shrink.Class_Name ("framework-button");
      Extend_Shrink.Add_Class ("main-menu-extend-shrink-button");
      Extend_Shrink.On_Click_Handler (On_Resize);
      Extend_Shrink.Dynamic;
      Instance.Extend_Shrink_Button := GGEC.Button_Access (Extend_Shrink);

      GGV.View_Access (Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("subelements", Container);
      Container.Class_Name ("main-menu-sub-elements-parent");
      Container.jQuery_Execute ("data('gnoga_is_opened', false)");
      Container.jQuery_Execute ("data('gnoga_root_id', -1)");
      Container.Dynamic;
      Instance.Sub_Elements_Parent := GGV.View_Access (Container);

      GGV.View_Access (Root_Container).Create (Instance.Parent.all);
      Instance.Parent.Add_Element ("elements", Root_Container);
      Root_Container.Class_Name ("main-menu-elements-parent");
      Root_Container.Style ("height", "calc(100% - " & Instance.Extend_Shrink_Button.Minimum_Height & " - 8px)");
      Root_Container.Dynamic;
      Instance.Elements_Parent := GGV.View_Access (Root_Container);
   end Create;

   ----------------------------------------------------------------------------
   procedure Load (Instance : in out Main_Menu_Type) is
      Data : Data_Type;
      Text : String;
      Parent : constant GGV.View_Access := Instance.Elements_Parent;
      Button : GGE.Pointer_To_Element_Class;
   begin
      for Data_ID in 1 .. Instance.Last_ID loop
         Data := Instance.Menu_Table (Data_ID);
         Text := Data.HTML;
         if not Instance.Is_Extended then
            Text := HTML_Icon (Instance, Data_ID);
         end if;
         if Data.Parent_ID = Root_Parent_ID then
            Button := new GGEC.Button_Type;
            GGEC.Button_Access (Button).Create (Parent.all, Text);
            Button.Class_Name ("framework-button");
            Button.Style ("display", "flex");
            Button.Style ("align-items", "center");
            Button.jQuery_Execute ("data('gnoga_id', " & To_String_Unsigned (Data_ID) & " )");
            Button.On_Click_Handler (Instance.On_Click);
            Button.Dynamic;
            Parent.Add_Element (Button_Name (Data_ID), Button);
            if not Data.Clickable then
               Button.Add_Class ("framework-unclickable");
               Button.On_Click_Handler (null);
            end if;
         end if;
      end loop;
   end Load;

   ----------------------------------------------------------------------------
   procedure Close_Menu (Instance : in out Main_Menu_Type) is
   begin
      Instance.Is_Opened := False;
      Instance.Current_Root := Root_Parent_ID;
      Update_Shortcuts (Instance, Root_Parent_ID);
      Update_Selected_Element (Instance);
   end Close_Menu;

   ----------------------------------------------------------------------------
   procedure Clear (Instance : in out Main_Menu_Type) is
   begin
      for Index in 1 .. Instance.Last_ID loop
         Remove_Button (Instance.Sub_Elements_Parent, Index);
         Remove_Button (Instance.Elements_Parent, Index);
      end loop;
      Instance.Last_ID := 0;
      Instance.Is_Opened := False;
      Instance.Current_Root := Root_Parent_ID;
   end Clear;

   ----------------------------------------------------------------------------
   function Add_Element (Instance : in out Main_Menu_Type; Name : String; Icon_SRC : String;
                         Handler : GGB.Action_Event := null) return Integer is
   begin
      Instance.Menu_Table (Instance.Last_ID + 1).Icon_SRC := Icon_SRC;
      return Add_Sub_Element (Instance, Name, Root_Parent_ID, Handler);
   end Add_Element;

   ----------------------------------------------------------------------------
   function Add_Sub_Element (Instance : in out Main_Menu_Type; Name : String; Parent_ID : Integer;
                             Handler : GGB.Action_Event := null) return Integer is
      Shortcut : constant Unicode_Character := Find_Shortcut (Instance, Parent_ID, Name);
      Shortcut_ID : constant Integer := Code (Shortcut);
   begin
      Instance.Last_ID := Instance.Last_ID + 1;

      Instance.Menu_Table (Instance.Last_ID).Parent_ID := Parent_ID;
      Instance.Menu_Table (Instance.Last_ID).HTML := HTML_Full (Instance, Instance.Last_ID, Name, Shortcut);
      Instance.Menu_Table (Instance.Last_ID).Handler := Handler;
      Instance.Menu_Table (Instance.Last_ID).Shortcut_ID := Shortcut_ID;
      Instance.Menu_Table (Instance.Last_ID).Clickable := True;
      Instance.Menu_Table (Instance.Last_ID).Delimiter_Above := False;

      Instance.Active_Menu (Instance.Last_ID) := (Parent_ID = Root_Parent_ID);
      Instance.Active_Shortcuts (Shortcut_ID) := (Parent_ID = Root_Parent_ID);

      return Instance.Last_ID;
   end Add_Sub_Element;

   ----------------------------------------------------------------------------
   procedure Add_Delimiter_Above (Instance : in out Main_Menu_Type; Unique_ID : Integer) is
   begin
      if Instance.Menu_Table (Unique_ID).Parent_ID /= Root_Parent_ID then
         Instance.Menu_Table (Unique_ID).Delimiter_Above := True;
      end if;
   end Add_Delimiter_Above;

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Set_Unclickable (Instance : in out Main_Menu_Type; Unique_ID : Integer) is
      Sub_Elm : constant GGE.Pointer_To_Element_Class := Instance.Sub_Elements_Parent.Element (Button_Name (Unique_ID));
      Elm : constant GGE.Pointer_To_Element_Class := Instance.Elements_Parent.Element (Button_Name (Unique_ID));
   begin
      Instance.Menu_Table (Unique_ID).Clickable := False;
      if Elm /= null then
         Elm.Add_Class ("framework-unclickable");
         Elm.On_Click_Handler (null);
      elsif Sub_Elm /= null then
         Sub_Elm.Add_Class ("framework-unclickable");
         Sub_Elm.On_Click_Handler (null);
      end if;
   end Set_Unclickable;

   ----------------------------------------------------------------------------
   procedure Set_Clickable
     (Instance  : in out Main_Menu_Type;
      Unique_ID : Integer)
   is
      Sub_Elm : constant GGE.Pointer_To_Element_Class :=
        Instance.Sub_Elements_Parent.Element (Button_Name (Unique_ID));
      Elm : constant GGE.Pointer_To_Element_Class := Instance.Elements_Parent.Element (Button_Name (Unique_ID));
   begin
      Instance.Menu_Table (Unique_ID).Clickable := True;
      if Elm /= null then
         Elm.Remove_Class ("framework-unclickable");
         Elm.On_Click_Handler (Instance.Menu_Table (Unique_ID).Handler);
      elsif Sub_Elm /= null then
         Sub_Elm.Remove_Class ("framework-unclickable");
         Sub_Elm.On_Click_Handler (Instance.Menu_Table (Unique_ID).Handler);
      end if;
   end Set_Clickable;

   ----------------------------------------------------------------------------
   procedure Enable_Shortcuts (Instance : in out Main_Menu_Type) is
   begin
      Instance.Are_Shortcuts_Enabled := True;
   end Enable_Shortcuts;

   ----------------------------------------------------------------------------
   procedure Disable_Shortcuts (Instance : in out Main_Menu_Type) is
   begin
      Instance.Are_Shortcuts_Enabled := False;
   end Disable_Shortcuts;

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Notify_Element_Click (Instance : in out Main_Menu_Type;  Object : in out GGB.Base_Type'Class) is
      Parent_ID : constant Integer := Int_Value (Object.jQuery_Execute ("data('gnoga_id')"));
   begin
      Toggle_Menu_Visibility (Instance, Parent_ID);
   end Notify_Element_Click;

   ----------------------------------------------------------------------------
   procedure Notify_Sub_Element_Click (Instance : in out Main_Menu_Type; Unique_ID : Integer) is
      Parent_ID : constant Integer := Instance.Menu_Table (Unique_ID).Parent_ID;
   begin
      Toggle_Menu_Visibility (Instance, Parent_ID);
   end Notify_Sub_Element_Click;

   ----------------------------------------------------------------------------
   procedure Notify_Key_Pressed (Instance : in out Main_Menu_Type; Key : Character) is
      Index : Integer;
      Data : Data_Type;

      function Find_Data return Integer is
      begin
         for Index in 1 .. Instance.Last_ID loop
            if Instance.Active_Menu (Index) and then Instance.Menu_Table (Index).Shortcut_ID = Code (Key) then
               return Index;
            end if;
         end loop;
         return -1;
      end Find_Data;

   begin
      if Instance.Are_Shortcuts_Enabled then
         if Is_Letter (Key) and then Instance.Active_Shortcuts (Code (Key)) then
            Index := Find_Data;
            if Index /= -1 then
               Data := Instance.Menu_Table (Index);
               if Data.Clickable then
                  if Data.Parent_ID = Root_Parent_ID then
                     Toggle_Menu_Visibility (Instance, Index);
                  else
                     Data.Handler (Instance.Parent.all);
                  end if;
               end if;
            end if;
         end if;
      end if;
   end Notify_Key_Pressed;

   ----------------------------------------------------------------------------
   procedure Notify_Resize (Instance : in out Main_Menu_Type) is
   begin
      if Instance.Is_Extended then
         Instance.Extend_Shrink_Button.Inner_HTML
                           ("<img class=""main-menu-icon"" src=""" &
                            Image_Gnoga_Root.Delete (1, 1) & "ico-left_panel_open.png"">");
         Instance.Parent.Remove_Class ("main-menu-force-extend");
         Instance.Hide_Elements_Text;
      else
         Instance.Extend_Shrink_Button.Inner_HTML
                          ("<img class=""main-menu-icon"" src=""" &
                           Image_Gnoga_Root.Delete (1, 1) & "ico-left_panel_close.png""><span>Réduire</span>");
         Instance.Parent.Add_Class ("main-menu-force-extend");
         Instance.Show_Elements_Text;
      end if;
      Instance.Is_Extended := not Instance.Is_Extended;
   end Notify_Resize;

-------------------------------------------------------------------------------
end v22.Gui.Main_Menu;
-------------------------------------------------------------------------------
