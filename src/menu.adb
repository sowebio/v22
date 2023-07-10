with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

package body Menu is

   Menu_Error : exception;

   -----------------------------------------------------------------------------
   --  Utils
   -----------------------------------------------------------------------------
   function Get_Data
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer)
      return Data_Type
   is
   begin
      for Index in Instance.Menu_Table'Range loop
         if Instance.Menu_Table (Index).Unique_Id = Unique_Id then
            return Instance.Menu_Table (Index);
         end if;
      end loop;
      raise Menu_Error with "Unable to find data";
   end Get_Data;

   function Is_Leaf
     (Instance  : in out Menu_Type;
      Unique_ID :        Integer)
      return Boolean
   is
   begin
      for Index in Instance.Menu_Table'Range loop
         if Instance.Menu_Table (Index).Parent_Id = Unique_ID then
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
      for Data_Id in Instance.Menu_Table'Range loop
         Instance.Remove_Button (Data_Id);
         if Instance.Menu_Table (Data_Id).Parent_Id = Parent_Id then
            declare
               Data   : constant Data_Type                                  := Instance.Menu_Table (Data_Id);
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
   begin
      Instance.Parent             := Parent'Unrestricted_Access;
      Instance.Breadcrumb_Parent  := Breadcrumb_Parent'Unrestricted_Access;
      Instance.Breadcrumb_Content := Breadcrumb.Create (Breadcrumb_Parent);
      return Instance;
   end Create;

   procedure Set_Root
     (Instance  : in out Menu_Type;
      Name      :        UXString;
      Unique_Id :        Integer;
      On_Open   :        Gnoga.Gui.Base.Action_Event)
   is
      Root : Data_Type;
   begin
      if Instance.Next_Id /= 1 then
         raise Menu_Error with "Root must be created before childs";
      end if;
      Root.Name                              := Name;
      Root.On_Open                           := On_Open;
      Root.Index                             := Instance.Next_Id;
      Root.Unique_Id                         := Unique_Id;
      Instance.Menu_Table (Instance.Next_Id) := Root;
      Instance.Next_Id                       := Instance.Next_Id + 1;
      Instance.Breadcrumb_Content.Update (Root.On_Open, Root.Name);
   end Set_Root;

   procedure Add_Child
     (Instance  : in out Menu_Type;
      Parent_Id :        Integer;
      Name      :        UXString;
      Unique_Id :        Integer;
      On_Open   :        Gnoga.Gui.Base.Action_Event)
   is
      Child : Data_Type;
   begin
      if Instance.Next_Id = Max_Menu_Count then
         raise Menu_Error with "Too much menus, increase Max_Menu_Count";
      end if;
      Child.Parent_Id                        := Parent_Id;
      Child.Name                             := Name;
      Child.Depth                            := Instance.Get_Data (Parent_Id).Depth + 1;
      Child.On_Open                          := On_Open;
      Child.Index                            := Instance.Next_Id;
      Child.Unique_Id                        := Unique_Id;
      Instance.Menu_Table (Instance.Next_Id) := Child;
      Instance.Next_Id                       := Instance.Next_Id + 1;
   end Add_Child;

   procedure Notify_Click
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer)
   is
      Data : constant Data_Type := Get_Data (Instance, Unique_Id);
   begin
      Instance.Breadcrumb_Content.Update (Data.On_Open, Data.Name, Data.Depth);
      if not Instance.Is_Leaf (Data.Unique_Id) then
         Instance.Update (Data.Unique_Id);
      end if;
   end Notify_Click;

   procedure Set_Menu
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer)
   is
   begin
      Instance.Get_Data (Unique_Id).On_Open (Instance.Parent.all);
      --  Instance.Notify_Click (Unique_Id);
   end Set_Menu;

end Menu;
