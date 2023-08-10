package body User_Menu is

   package Element renames Gnoga.Gui.Element;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   procedure Create
     (Instance : in out User_Menu_Type;
      Parent   :        View.View_Type)
   is
   begin
      Instance.Parent := Parent'Unrestricted_Access;
   end Create;

   procedure Display (Instance : in out User_Menu_Type) is
      Button : Element.Pointer_To_Element_Class;
   begin
      for Index in 1 .. Instance.Last_Index loop
         Button := new Common.Button_Type;
         Button.Dynamic;
         Common.Button_Access (Button).Create (Instance.Parent.all, Instance.Menu_Table (Index).Name);
         Button.Class_Name ("framework-button");
         Button.On_Click_Handler (Instance.Menu_Table (Index).Click_Handler);
      end loop;
   end Display;

   procedure Add_Element
     (Instance : in out User_Menu_Type;
      Name     :        UXString;
      On_Click :        Base.Action_Event)
   is
      Menu : Data_Type;
   begin
      Menu.Name          := Name;
      Menu.Click_Handler := On_Click;

      Instance.Last_Index := Instance.Last_Index + 1;
      Instance.Menu_Table (Instance.Last_Index) := Menu;
   end Add_Element;

   procedure Clear (Instance : in out User_Menu_Type) is
   begin
      Instance.Parent.Inner_HTML ("");
   end Clear;

end User_Menu;
