package body Footer is

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------

   procedure Create
     (Instance : in out Footer_Type;
      Parent   : in out View.View_Type)
   is
      State_Text_Parent     : constant View.Pointer_To_View_Class := new View.View_Type;
      Permanent_Text_Parent : constant View.Pointer_To_View_Class := new View.View_Type;
   begin
      State_Text_Parent.Dynamic;
      Permanent_Text_Parent.Dynamic;

      Instance.State_Text_Parent := View.View_Access (State_Text_Parent);
      Instance.State_Text_Parent.Create (Parent);
      Instance.State_Text_Parent.Class_Name ("state-text-parent");

      Instance.Permanent_Text_Parent := View.View_Access (Permanent_Text_Parent);
      Instance.Permanent_Text_Parent.Create (Parent);
      Instance.Permanent_Text_Parent.Class_Name ("permanent-text-parent");
   end Create;

   procedure Set_State_Text
     (Instance : in out Footer_Type;
      State    :        UXString := "")
   is
   begin
      Instance.State_Text_Parent.Inner_HTML (State);
   end Set_State_Text;

   procedure Set_Permanent_Text
     (Instance : in out Footer_Type;
      State    :        UXString := "")
   is
   begin
      Instance.Permanent_Text_Parent.Inner_HTML (State);
   end Set_Permanent_Text;

end Footer;
