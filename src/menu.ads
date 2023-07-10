with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;
with Breadcrumb;

package Menu is

   type Menu_Type is tagged private;

   function Create
     (Parent, Breadcrumb_Parent : in out Gnoga.Gui.View.View_Type)
      return Menu_Type;

   procedure Set_Root
     (Instance  : in out Menu_Type;
      Name      :        UXString;
      Unique_Id :        Integer;
      On_Open   :        Gnoga.Gui.Base.Action_Event);

   procedure Add_Child
     (Instance  : in out Menu_Type;
      Parent_Id :        Integer;
      Name      :        UXString;
      Unique_Id :        Integer;
      On_Open   :        Gnoga.Gui.Base.Action_Event);

   procedure Notify_Click
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer);

   procedure Set_Menu
     (Instance  : in out Menu_Type;
      Unique_Id :        Integer);

private

   Root_Parent_Id : Integer := -1;
   Root_Depth     : Integer := 0;

   type Data_Type is record
      Parent_Id : Integer  := Root_Parent_Id;
      Name      : UXString := "";
      Depth     : Integer  := Root_Depth;
      Index     : Integer  := 0;
      Unique_Id : Integer;

      On_Open : Gnoga.Gui.Base.Action_Event;
   end record;

   Max_Menu_Count : constant Integer := 20;

   type Menu_Table_Type is array (1 .. Max_Menu_Count) of Data_Type;

   type Menu_Type is tagged record
      Parent             : Gnoga.Gui.View.View_Access;
      Breadcrumb_Parent  : Gnoga.Gui.View.View_Access;
      Breadcrumb_Content : Breadcrumb.Breadcrumb_Type;

      Menu_Table : Menu_Table_Type;
      Next_Id    : Integer := 1;
   end record;

end Menu;
