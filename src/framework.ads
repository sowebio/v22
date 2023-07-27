with Gnoga.Gui.Base;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package Framework is

   package Base renames Gnoga.Gui.Base;
   package View renames Gnoga.Gui.View;

   procedure Setup
     (On_User_Connect       : Base.Action_Event;
      Title                 : UXString;
      Server_Closed_Content : UXString);

   procedure Set_App_Icon
     (Object   : in out Base.Base_Type'Class;
      Icon_SRC :        UXString);

   procedure Set_User_Icon
     (Object   : in out Base.Base_Type'Class;
      Icon_SRC :        UXString);

   procedure Set_User_Name
     (Object : in out Base.Base_Type'Class;
      Name   :        UXString);

      -----------------------------------------------------------------------------
      --  Getters
      -----------------------------------------------------------------------------

   function Get_Content_Text
     (Object : in out Base.Base_Type'Class)
      return View.View_Access;

   -----------------------------------------------------------------------------
   --  Header
   -----------------------------------------------------------------------------

   procedure Header_Set_Root
     (Key      : UXString;
      Name     : UXString;
      On_Click : Base.Action_Event);

   procedure Header_Add_Child
     (Key        : UXString;
      Name       : UXString;
      Parent_Key : UXString;
      On_Click   : Base.Action_Event);

   procedure Header_Add_Dialog
     (Title        : UXString;
      Content      : UXString          := "";
      Confirm_Text : UXString          := "";
      Cancel_Text  : UXString          := "";
      On_Confirm   : Base.Action_Event := null;
      On_Cancel    : Base.Action_Event := null);

   procedure Header_Add_Web
     (Title : UXString;
      URL   : UXString);

   procedure Header_Add_Button
     (Title    : UXString;
      On_Click : Base.Action_Event);

      -----------------
      --  Callbacks  --
      -----------------

   procedure Header_Notify_Menu_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

      -----------------------------------------------------------------------------
      --  CRUD
      -----------------------------------------------------------------------------

   procedure CRUD_Load (Object : in out Base.Base_Type'Class);

   procedure CRUD_Add_Element
     (Object   : in out Base.Base_Type'Class;
      Key      :        UXString;
      Name     :        UXString;
      Icon_SRC :        UXString);

   procedure CRUD_Add_Sub_Element
     (Object     : in out Base.Base_Type'Class;
      Key        :        UXString;
      Name       :        UXString;
      Parent_Key :        UXString;
      On_Click   :        Base.Action_Event);

   procedure CRUD_Add_Delimiter_Above
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

   procedure CRUD_Set_Unclickable
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

   procedure CRUD_Notify_Sub_Element_Click
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString);

      -----------------------------------------------------------------------------
      --  Content
      -----------------------------------------------------------------------------

   procedure Content_Set_Title
     (Object : in out Base.Base_Type'Class;
      Title  :        UXString);

   procedure Content_Set_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString);

      -----------------------------------------------------------------------------
      --  Footer
      -----------------------------------------------------------------------------

   procedure Footer_Set_State_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "");

   procedure Footer_Set_Permanent_Text
     (Object : in out Base.Base_Type'Class;
      Text   :        UXString := "");

      -----------------------------------------------------------------------------
      --  User relative data
      -----------------------------------------------------------------------------

   procedure Set
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString;
      Value  :        Integer);

   function Get
     (Object : in out Base.Base_Type'Class;
      Key    :        UXString)
      return Integer;

end Framework;
