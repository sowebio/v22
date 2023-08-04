with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package User_Menu is

   package View renames Gnoga.Gui.View;
   package Base renames Gnoga.Gui.Base;

   procedure Display (Parent : in out View.View_Type);
   --  Should be called every time the menu is opened

   procedure Add_Button
     (Title    : UXString;
      on_Click : Base.Action_Event);
      --  Function to create a button with customized click handler

   procedure Add_Dialog
     (Title           : UXString;
      Content         : UXString          := "";
      Confirm_Text    : UXString          := "";
      Cancel_Text     : UXString          := "";
      Confirm_Handler : Base.Action_Event := null;
      Cancel_Handler  : Base.Action_Event := null);
      --  Function to create a button on the user menu, creating a jQuery dialog.
      --  Two default buttons are available : Cancel and Confirm
      --  Buttons are not displayed on dialog if corresponding text is empty

   procedure Add_Web
     (Title : UXString;
      URL   : UXString);
      --  Function to create a button on the user menu, which opens a new web page

   procedure Clear (Parent : in out View.View_Type);

private

   procedure Launch_Button
     (Object    : in out Base.Base_Type'Class;
      Unique_ID :        Integer);

   procedure Launch_Dialog
     (Object    : in out Base.Base_Type'Class;
      Unique_ID :        Integer);

   procedure Launch_Web
     (Object    : in out Base.Base_Type'Class;
      Unique_ID :        Integer);

end User_Menu;
