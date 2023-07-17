with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

package body Menu is

   Menu_Error : exception;

   Root_Parent_Id : constant Integer := -1;
   Root_Depth     : constant Integer := 0;

   type Data_Type is record
      Parent_Id : Integer  := Root_Parent_Id;
      Name      : UXString := "";
      Depth     : Integer  := Root_Depth;
      Index     : Integer  := 0;

      On_Open : Gnoga.Gui.Base.Action_Event;
   end record;

   Max_Menu_Count : constant Integer := 50;

   type Menu_Table_Type is array (1 .. Max_Menu_Count) of Data_Type;

   Menu_Table : Menu_Table_Type;
   Next_Id    : Integer := 1;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   function Is_Leaf
     (Unique_ID : Integer)
      return Boolean
   is
   begin
      for Index in Menu_Table'Range loop
         if Menu_Table (Index).Parent_Id = Unique_ID then
            return False;
         end if;
      end loop;
      return True;
   end Is_Leaf;

   procedure Remove_Button
     (Instance : in out Menu_Type;
      Data_Id  :        Integer)
   is
      Button_Name : UXString := "";
   begin
      Button_Name := "Menu_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
      if Instance.Parent.Element (Button_Name) /= null then
         Instance.Parent.Element (Button_Name).Remove;
      end if;
   end Remove_Button;

   procedure Update
     (Instance  : in out Menu_Type;
      Parent_Id :        Integer)
   is
      Button_Name : UXString;
   begin
      for Data_Id in Menu_Table'Range loop
         Instance.Remove_Button (Data_Id);
         if Menu_Table (Data_Id).Parent_Id = Parent_Id then
            declare
               Data   : constant Data_Type                                  := Menu_Table (Data_Id);
               Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
            begin
               Button_Name := "Menu_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
               Gnoga.Gui.Element.Common.Button_Access (Button).Create (Instance.Parent.all, Data.Name);
               Button.Dynamic;
               Instance.Parent.Add_Element (Button_Name, Button);
               Button.On_Click_Handler (Data.On_Open);
            end;
         end if;
      end loop;
   end Update;

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------
   function Create
     (Parent, Breadcrumb_Parent : in out Gnoga.Gui.View.View_Type)
      return Menu_Type
   is
      Instance : Menu_Type;
      Root     : constant Data_Type := Menu_Table (1);
   begin
      Instance.Parent             := Parent'Unrestricted_Access;
      Instance.Breadcrumb_Parent  := Breadcrumb_Parent'Unrestricted_Access;
      Instance.Breadcrumb_Content := Breadcrumb.Create (Breadcrumb_Parent);
      Instance.Breadcrumb_Content.Update (Root.On_Open, Root.Name);
      return Instance;
   end Create;

   function Set_Root
     (Name    : UXString;
      On_Open : Gnoga.Gui.Base.Action_Event)
      return Integer
   is
      Root : Data_Type;
   begin
      if Next_Id /= 1 then
         raise Menu_Error with "Root must be created before childs";
      end if;
      Root.Name            := Name;
      Root.On_Open         := On_Open;
      Root.Index           := Next_Id;
      Menu_Table (Next_Id) := Root;
      Next_Id              := Next_Id + 1;
      return Root.Index;
   end Set_Root;

   function Add_Child
     (Parent_Id : Integer;
      Name      : UXString;
      On_Open   : Gnoga.Gui.Base.Action_Event)
      return Integer
   is
      Child : Data_Type;
   begin
      if Next_Id = Max_Menu_Count then
         raise Menu_Error with "Too much menus, increase Max_Menu_Count";
      end if;
      Child.Parent_Id      := Parent_Id;
      Child.Name           := Name;
      Child.Depth          := Menu_Table (Parent_Id).Depth + 1;
      Child.On_Open        := On_Open;
      Child.Index          := Next_Id;
      Menu_Table (Next_Id) := Child;
      Next_Id              := Next_Id + 1;
      return Child.Index;
   end Add_Child;

   procedure Notify_Click
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer)
   is
      Data : constant Data_Type := Menu_Table (Unique_Id);
   begin
      Gnoga.Log ("Clicked on " & Data.Name);
      Instance.Breadcrumb_Content.Update (Data.On_Open, Data.Name, Data.Depth);
      if not Is_Leaf (Unique_Id) then
         Instance.Update (Unique_Id);
      end if;
   end Notify_Click;

   procedure Set_Menu
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer)
   is
   begin
      Menu_Table (Unique_Id).On_Open (Instance.Parent.all);
      --  Instance.Notify_Click (Unique_Id);
   end Set_Menu;

end Menu;
