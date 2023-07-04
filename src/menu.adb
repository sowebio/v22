with Gnoga.Gui.Element; use Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;

package body Menu is

   type Data_Type is record
      Parent_Id : Integer  := -1;
      Name      : UXString := "";
      Depth     : Integer  := 0;
      Index     : Integer  := 0;
      Unique_Id : Integer;

      On_Open : Gnoga.Gui.Base.Action_Event;
   end record;

   Menu_Error : exception;
   Max_Menu_Count : constant Integer := 50;
   Menu_Table     : array (1 .. Max_Menu_Count) of Data_Type;
   Next_Id        : Integer          := 1;

   function Init_Breadcrumb
     (Parent : in out Gnoga.Gui.View.View_Type)
      return Breadcrumb.Breadcrumb_Type
   is
      Result : Breadcrumb.Breadcrumb_Type;
   begin
      if Next_Id = 1 then
         raise Menu_Error with "A root must be created before with Create_Parent";
      end if;
      Breadcrumb.Add (Result, Parent, Menu_Table (1).On_Open, Menu_Table (1).Name);
      return Result;
   end Init_Breadcrumb;

   procedure Create_Parent
     (Name      : UXString;
      Unique_Id : Integer;
      On_Open   : Gnoga.Gui.Base.Action_Event)
   is
      Parent : Data_Type;
   begin
      if Next_Id /= 1 then
         raise Menu_Error with "Parent must be created before childs";
      end if;
      Parent.Name          := Name;
      Parent.On_Open       := On_Open;
      Parent.Index         := Next_Id;
      Parent.Unique_Id     := Unique_Id;
      Parent.Parent_Id     := -1;
      Menu_Table (Next_Id) := Parent;
      Next_Id              := Next_Id + 1;
   end Create_Parent;

   function Get_Data
     (Unique_Id : Integer)
      return Data_Type
   is
   begin
      for Index in Menu_Table'Range loop
         if Menu_Table (Index).Unique_Id = Unique_Id then
            return Menu_Table (Index);
         end if;
      end loop;
      raise Menu_Error with "Unable to find data";
   end Get_Data;

   procedure Add_Child
     (Parent_Id : Integer;
      Name      : UXString;
      Unique_Id : Integer;
      On_Open   : Gnoga.Gui.Base.Action_Event)
   is
      Child : Data_Type;
   begin
      if Next_Id = Max_Menu_Count then
         raise Menu_Error with "Too much menus, increase Max_Menu_Count";
      end if;
      Child.Parent_Id      := Parent_Id;
      Child.Name           := Name;
      Child.Depth          := Get_Data (Parent_Id).Depth + 1;
      Child.On_Open        := On_Open;
      Child.Index          := Next_Id;
      Child.Unique_Id      := Unique_Id;
      Menu_Table (Next_Id) := Child;
      Next_Id              := Next_Id + 1;
   end Add_Child;

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
     (Container : in out Gnoga.Gui.View.View_Type;
      Data_Id   :        Integer)
   is
      Button_Name : UXString := "";
   begin
      Button_Name := "Menu_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
      if Container.Element (Button_Name) /= null then
         Container.Element (Button_Name).Remove;
      end if;
   end Remove_Button;

   --  Does not work using this, segfault
   --  function On_Open
   --    (Container, Breadcrumb_Container : in out Gnoga.Gui.View.View_Type;
   --     Data                            :        Data_Type)
   --     return Gnoga.Gui.Base.Action_Event
   --  is
   --     procedure Result (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
   --     begin
   --        Data.On_Open (Object);
   --        Update (Container, Breadcrumb_Container, Data.Unique_Id);
   --     end Result;
   --  begin
   --     return Result'Unrestricted_Access;
   --  end On_Open;

   procedure Update
     (Container : in out Gnoga.Gui.View.View_Type;
      Parent_Id :        Integer)
   is
      Button_Name : UXString;
   begin
      for Data_Id in Menu_Table'Range loop
         Remove_Button (Container, Data_Id);
      end loop;
      for Data_Id in Menu_Table'Range loop
         if Menu_Table (Data_Id).Parent_Id = Parent_Id then
            declare
               Data   : constant Data_Type                                  := Menu_Table (Data_Id);
               Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
            begin
               Button_Name := "Menu_" & From_UTF_8 (Data_Id'Image).Delete (1, 1);
               Gnoga.Gui.Element.Common.Button_Access (Button).Create (Container, Data.Name);
               Container.Add_Element (Button_Name, Button);
               Button.Dynamic;
               Button.On_Click_Handler (Data.On_Open);
            end;
         end if;
      end loop;
   end Update;

   procedure Notify_Click
     (Container, Breadcrumb_Container : in out Gnoga.Gui.View.View_Type;
      Breadcrumb_Content              : in out Breadcrumb.Breadcrumb_Type;
      Handler                         :        Gnoga.Gui.Base.Action_Event;
      Unique_Id                       :        Integer)
   is
      Data : constant Data_Type := Get_Data (Unique_Id);
   begin
      Breadcrumb.Update (Breadcrumb_Content, Breadcrumb_Container, Handler, Data.Name, Data.Depth);
      if not Is_Leaf (Data.Unique_Id) then
         Menu.Update (Container, Data.Unique_Id);
      end if;
   end Notify_Click;

end Menu;
