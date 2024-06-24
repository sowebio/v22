-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-main-menu.ads
--  @copyright See authors list below and README.md file
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

with Gnoga.Gui.Base; use Gnoga.Gui.Base;
with Gnoga.Gui.View;
with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

with v22.Prg;
with v22.Uxs; use v22.Uxs;

package v22.Gui.Main_Menu is

   package GGB renames Gnoga.Gui.Base;
   package GGV renames Gnoga.Gui.View;
   package GGE renames Gnoga.Gui.Element;
   package GGEC renames Gnoga.Gui.Element.Common;

   type Main_Menu_Type is tagged private;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Create (Instance  : in out Main_Menu_Type; Parent : in out GGV.View_Type;
                     On_Resize : GGB.Action_Event; On_Click : GGB.Action_Event);
   --  Should be called every time a user connects.
   --  NOTE: On_Resize and On_Click allows to access user-defined
   --  connection data thus allowing access to current Instance...

   procedure Load (Instance : in out Main_Menu_Type);
   --  Load Main_Menu on screen, necessary once Main_Menu is fully built with
   --  Add_Element and Add_Sub_Element.

   procedure Close_Menu (Instance : in out Main_Menu_Type);
   --  Closes sub-elements panel.

   procedure Clear (Instance : in out Main_Menu_Type);
   --  Clear every Main_Menu element and sub-elements from screen.

   function Add_Element (Instance : in out Main_Menu_Type; Name : String; Icon_SRC : String;
                         Handler : GGB.Action_Event := null) return Integer;
   --  Add element which will be visible directly on screen. Elements come
   --  with an icon.
   --  NOTE: Elements must be created before sub-elements, and returned value
   --  (Parent_ID) allows to create sub-elements for this element.

   function Add_Sub_Element (Instance : in out Main_Menu_Type; Name : String; Parent_ID : Integer;
                             Handler : GGB.Action_Event := null) return Integer;
   --  Add sub-element, accessible only when the user clicks on an element
   --  created with Add_Element.
   --  NOTE: Handler must trigger Notify_Sub_Element_Click.

   procedure Add_Delimiter_Above (Instance : in out Main_Menu_Type; Unique_ID : Integer);
   --  Add a delimiter above the sub-element with "Unique_ID" identifier.

   -----------------------------------------------------------------------------
   --  Setters
   -----------------------------------------------------------------------------

   procedure Set_Unclickable (Instance : in out Main_Menu_Type; Unique_ID : Integer);
   --  Set an element or sub-element unclickable and unaccessible using
   --  keyboard shortcuts.

   procedure Set_Clickable (Instance  : in out Main_Menu_Type; Unique_ID :        Integer);
   --  Set an element or sub-element clickable again, working before and after
   --  load, assuming Main_Menu was built with Add_Element and Add_Sub_Element.

   procedure Enable_Shortcuts (Instance : in out Main_Menu_Type);
   --  Enable Main_Menu shortcuts, allowing both elements and sub-elements shortcuts.

   procedure Disable_Shortcuts (Instance : in out Main_Menu_Type);
   --  Disable Main_Menu shortcuts, for both elements and sub-elements.

   -----------------------------------------------------------------------------
   --  Callbacks
   -----------------------------------------------------------------------------

   --  NOTE: These callbacks allow the v22.adb scope, thus accessing App_Data
   --  and thus the Main_Menu instance can be accessed.

   procedure Notify_Element_Click (Instance : in out Main_Menu_Type; Object : in out GGB.Base_Type'Class);

   procedure Notify_Sub_Element_Click (Instance : in out Main_Menu_Type; Unique_ID : Integer);
   --  Callback to place in sub-elements' handlers.

   --procedure Notify_Key_Pressed (Instance : in out Main_Menu_Type; Key : Character);
   --  Callback to place in window's Character_Handler callback.

   procedure Notify_Resize (Instance : in out Main_Menu_Type);
   --  Callback to place in On_Resize, parameter of the Create procedure.
   --  Must be triggered when user clicks to resize Main_Menu panel.

--------------------------------------------------------------------------------
private

   Root_Parent_ID      : constant Integer           := -1;
   Force_Shortcut_Char : constant Unicode_Character := '~';

   type Data_Type is record
      Parent_ID : Integer  := Root_Parent_ID;
      Icon_SRC  : String := "";
      HTML : String;
      Clickable : Boolean := True;
      Delimiter_Above : Boolean := False;
      Handler : GGB.Action_Event;
      --Shortcut_ID : Integer;
   end record;

   Max_Menu_Amount : constant Integer := 50;
   type Menu_Array is array (1 .. Max_Menu_Amount) of Data_Type;

   type Boolean_Array is array (Integer range <>) of Boolean;

   type Main_Menu_Type is tagged record
      Active_Shortcuts : Boolean_Array (1 .. 26) := (others => False);
      Active_Menu : Boolean_Array (1 .. Max_Menu_Amount) := (others => False);
      Menu_Table : Menu_Array;
      Last_ID : Integer := 0;
      Current_Root : Integer := Root_Parent_ID;
      Is_Opened : Boolean := False;
      Are_Shortcuts_Enabled : Boolean := True;
      On_Click : GGB.Action_Event;
      Parent : GGV.View_Access;
      Sub_Elements_Parent : GGV.View_Access;
      Elements_Parent : GGV.View_Access;
      Extend_Shrink_Button : GGEC.Button_Access;
      Is_Extended : Boolean := False;
   end record;

-------------------------------------------------------------------------------
end v22.Gui.Main_Menu;
-------------------------------------------------------------------------------
