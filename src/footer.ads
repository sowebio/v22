with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package Footer is

   package View renames Gnoga.Gui.View;

   type Footer_Data is tagged record
      State_Text_Parent     : View.View_Access;
      Permanent_Text_Parent : View.View_Access;
   end record;
   type Footer_Type is new Footer_Data with null record;

   procedure Create
     (Instance : in out Footer_Type;
      Parent   : in out View.View_Type);
   --  Should be called every time a user connects

   procedure Set_State_Text
     (Instance : in out Footer_Type;
      State    :        UXString := "");
   --  Set user state text

   procedure Set_Permanent_Text
     (Instance : in out Footer_Type;
      State    :        UXString := "");
   --  Set user permanent text

private

end Footer;
