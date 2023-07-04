with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;
with Breadcrumb;

package Menu is

   procedure Create_Parent
     (Name      : UXString;
      Unique_Id : Integer;
      On_Open   : Gnoga.Gui.Base.Action_Event);

   procedure Add_Child
     (Parent_Id : Integer;
      Name      : UXString;
      Unique_Id : Integer;
      On_Open   : Gnoga.Gui.Base.Action_Event);

   function Init_Breadcrumb
     (Parent : in out Gnoga.Gui.View.View_Type)
      return Breadcrumb.Breadcrumb_Type;

   procedure Update
     (Container : in out Gnoga.Gui.View.View_Type;
      Parent_Id :        Integer);

   procedure Notify_Click
     (Container, Breadcrumb_Container : in out Gnoga.Gui.View.View_Type;
      Breadcrumb_Content              : in out Breadcrumb.Breadcrumb_Type;
      Handler                         :        Gnoga.Gui.Base.Action_Event;
      Unique_Id                       :        Integer);

end Menu;
