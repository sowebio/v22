-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-gui-footer.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Crud package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Gnoga.Gui.View;
--  with Gnoga.Server.Database.MySQL;
--  with Gnoga.Server.Database.SQLite;

package v22.Gui.Crud is

   package GGB renames Gnoga.Gui.Base;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   function Get (Object : in out Gnoga.Gui.Base.Base_Type'Class; Parameter : String) return String;
   --  Get CRUD parameter from Init procedure.

   function List_Length_Get return Positive;
   --  Get list length in CRUD screen.

   procedure Init (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                   Db_Name : String;
                   Db_Table : String;
                   Db_Key : String;
                   List_Name : String;
                   List_Key : String;
                   List_Header : String;
                   List_Columns : String;
                   List_Join : String := "";
                   List_Where : String := "";
                   List_Order : String := "";
                   List_Limit : Positive := List_Length_Get);
   --  CRUD screen initialization
   --
   --  Notes:
   --  Msg.Info ("List_Key: " & Gui.Crud.Get (Object, "List_Key"));                           --  Places_Crud
   --  Msg.Info ("Data_Index: " & Data_Index);                                                --  5 (list index)
   --  Msg.Info ("Data_Value: " & Gui.Crud.Get (Object, "List_Key") & "_" & Data_Index);      --  Places_Crud_5 => 59 (db key index)
   --  Msg.Info ("Set " &  Gui.Crud.Get (Object, "List_Key") & "_Select with " & Data_Value); --  59 => Places_Crud_Select
   --
   --  With List_Key = Places_Crud
   --  Data_Index : constant String := To_String_Unsigned (Gui.Content_List_Selected_Row (Object, Gui.Crud.Get (Object, "List_Key")));
   --  Data_Index = 5 (index in displayed lines of list, 5 = fifth line of the list)
   --  Data_Value : constant String := Gui.Connection_Data_Get (Object, Gui.Crud.Get (Object, "List_Key") & "_" & Data_Index);
   --  Places_Crud_5 = 59 (db key index, 59 is the value to seek)
   --  Then store 59 in Places_Crud_Select key for further use in search for updating or deletion.

   procedure List (Object : in out GGB.Base_Type'Class; Key : String := "");
   --  Display list in CRUD screen.

   procedure List_Length_Set (Length : Positive);
   --  Set list length in CRUD screen.

   procedure List_Reset (Object : in out GGB.Base_Type'Class);
   --  Reset list cursor to the list's beginning.

   procedure On_Cancel (Object : in out GGB.Base_Type'Class);
   --  Handle cancel in CRUD screen.

   procedure On_Down (Object : in out GGB.Base_Type'Class);
   --  List scroll down in CRUD screen.

   procedure On_Filter_Off (Object : in out GGB.Base_Type'Class);
   --  Unfilter the current filtered list.

   procedure On_Refresh (Object : in out GGB.Base_Type'Class);
   --  List refresh in CRUD screens.

   procedure On_Up (Object : in out GGB.Base_Type'Class);
   --  List scroll up in CRUD screen.

   procedure On_Validate_Delete (Object : in out Gnoga.Gui.Base.Base_Type'Class);
   --  Delete record in CRUD screen.

   procedure Put_Error_Message (Object : in out Gnoga.Gui.Base.Base_Type'Class; Error : String);
   --  Display a error message at the bottom of the screen.

   function Read (Object : in out Gnoga.Gui.Base.Base_Type'Class; Key : String; Query : String; Where : String := "") return String;
   --  Read a record in CRUD screens.
   -- /!\ Key is assumed to be already space trimmed. This function doesn't trim Key,
   --     as Key must already be space trimmed in the Query, to ensure that there are
   --     no space in the written key, and that subsequent searches will be accurate.

   function Read_By_Id (Object : in out Gnoga.Gui.Base.Base_Type'Class; Query : String) return String;
   --  Read by Id a record in CRUD screens.

   function Search (Object : in out Gnoga.Gui.Base.Base_Type'Class; Key : String; Message_Not_Found : String) return Boolean;
   --  Search on Key in CRUD screen.

   procedure Set_Limits (Object : in out GGB.Base_Type'Class);
   --  Compute list limits Id.

   procedure Title (Object : in out Gnoga.Gui.Base.Base_Type'Class; Mode : String; Title : String := "");
   --  List title in CRUD screens.

   procedure Write (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                    Operation : String;
                    Key : String;
                    Query : String;
                    Message_Key_Already_Exists : String;
                    Message_Key_Is_Missing : String);
   --  Write a record in CRUD screens.

------------------------------------------------------------------------------
private

   function Build_Condition (Object : in out GGB.Base_Type'Class;
                             Key : String := "";
                             Order_Direction : String := "") return String;

   --procedure Set_Limits (Object : in out GGB.Base_Type'Class);

   List_Length : Positive := 20;

-------------------------------------------------------------------------------
end v22.Gui.Crud;
-------------------------------------------------------------------------------
