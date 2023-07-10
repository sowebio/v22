with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package User_Menu is

   procedure Create (Parent : in out Gnoga.Gui.View.View_Type);

   -- Buttons should not be displayed if their texts are ""
   procedure Add_Dialog
     (Title           : UXString;
      Content         : UXString                    := "";
      Confirm_Text    : UXString                    := "";
      Cancel_Text     : UXString                    := "";
      Confirm_Handler : Gnoga.Gui.Base.Action_Event := null;
      Cancel_Handler  : Gnoga.Gui.Base.Action_Event := null);

   procedure Add_Web
     (Title : UXString;
      URL   : UXString := "");

end User_Menu;
