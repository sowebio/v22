-------------------------------------------------------------------------------
--  @file      v22-gui-crud.adb
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Gnoga User Interface - Crud management
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

package body v22.Gui.Crud is

   ----------------------------------------------------------------------------
   function Get (Object : in out Gnoga.Gui.Base.Base_Type'Class; Parameter : String) return String is
   begin
      return Gui.Connection_Data_Get (Object, Parameter);
   end Get;

   ----------------------------------------------------------------------------
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
                   List_Limit : Positive := List_Length_Get) is
   begin
      Gui.Connection_Data_Set (Object, "Db_Name", Db_Name);
      Gui.Connection_Data_Set (Object, "Db_Table", Db_Table);
      Gui.Connection_Data_Set (Object, "Db_Key", Db_Table & "." & Db_Key);
      Gui.Connection_Data_Set (Object, "List_Name", List_Name);
      Gui.Connection_Data_Set (Object, "List_Name_Memory", List_Name);
      Gui.Connection_Data_Set (Object, "List_Key", List_Key);
      Gui.Connection_Data_Set (Object, "List_Header", List_Header);
      Gui.Connection_Data_Set (Object, "List_Columns", List_Columns);
      Gui.Connection_Data_Set (Object, "List_Join", List_Join);
      Gui.Connection_Data_Set (Object, "List_Where", List_Where);
      Gui.Connection_Data_Set (Object, "List_Where_Memory", List_Where);
      Gui.Connection_Data_Set (Object, "List_Order", List_Order);
      Gui.Connection_Data_Set (Object, "List_Limit", To_String_Unsigned (List_Limit));
      Set_Limits (Object);
   end Init;

   ----------------------------------------------------------------------------
   procedure List (Object : in out GGB.Base_Type'Class; Key : String := "") is
   begin
      Gui.Content_Clear_Title (Object);
      Gui.Content_Clear_Text (Object);
      Gui.List (Object, Get (Object, "Db_Name"),
                        Get (Object, "Db_Table"),
                        Get (Object, "List_Name"),
                        Get (Object, "List_Key"),
                        Get (Object, "List_Header"),
                        Get (Object, "List_Columns"),
                        Build_Condition (Object, Key));
   end List;

   ----------------------------------------------------------------------------
   procedure List_Length_Set (Length : Positive) is
   begin
      List_Length := Length;
   end List_Length_Set;

   ----------------------------------------------------------------------------
   function List_Length_Get return Positive is
   begin
      return List_Length;
   end List_Length_Get;

   ----------------------------------------------------------------------------
   procedure List_Reset (Object : in out GGB.Base_Type'Class) is
      List_Limit : String := Get (Object, "List_Limit");
      List_Length : Natural;
      List_Name : String := Get (Object, "List_Name") & "_";
   begin
      Gui.Connection_Data_Delete (Object, List_Name & "First");
      Gui.Connection_Data_Delete (Object, List_Name & "Last");
      Gui.Connection_Data_Delete (Object, List_Name & "Select");
      --  Delete List_Name_Key_1 to List_Name_Key_n
      if Is_Numeric (List_Limit) then
         List_Length := To_Integer (List_Limit);
         for I in 1 .. List_Length loop
            Gui.Connection_Data_Delete (Object, List_Name & To_String_Unsigned (I));
         end loop;
      end if;
   end List_Reset;

   ----------------------------------------------------------------------------
   procedure On_Cancel (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Data_Value : String := Get (Object, Get (Object, "List_Key") & "_Select");
   begin
      Msg.Debug ("v22.Crud > On_Cancel > " & Get (Object, "Package_Name") & " > Db_List_Key_Select: " & Data_Value);
      if Is_Empty (Data_Value) then
         On_Up (Object);
      else
         List (Object, Data_Value);
      end if;
   end On_Cancel;

   ----------------------------------------------------------------------------
   procedure On_Down (Object : in out GGB.Base_Type'Class) is
      Last_Line_Value : String := Get (Object, Get (Object, "List_Key") & "_Last");
      Query_Result : String := "";
      Db_Key : String := Get (Object, "Db_Key");
      --Query_Result_Count : Natural;
      --List_Limit : String := Get (Object, "List_Limit");
   begin
      if Gui.Get_Display_In_Progress (Object) then
         Msg.Debug ("v22.Crud > On_Down > Display in progress, action deleted");
         delay 1.0; --  Avoid display collision
         Gui.Set_Display_In_Progress (Object, False);
      else
         Msg.Debug ("v22.Crud > On_Down > " & Get (Object, "List_Key"));

         --  Ascending order
         if Index (Get (Object, "List_Order"), "DESC") = 0 then
            --  From Last_Line_Value Take LIMIT results after in ascending order
            Query_Result := Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Db_Key, Build_Condition (Object, Last_Line_Value));

            --  Crop to LIMIT value to display a complete list - buggy, counter intuitive then deleted
            --  Query_Result_Count := Field_Count (Query_Result, RD);
            --  if Query_Result_Count < To_Integer (List_Limit) then
            --     --  Get the last obtained key result
            --     Last_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
            --     --  Go back LIMIT results in descending order
            --     Query_Result := Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Db_Key, Build_Condition (Object, Last_Line_Value, "DESC"));
            --     Query_Result_Count := Field_Count (Query_Result, RD);
            --     --  Get the last obtained key result, which will become the first, since it is the result
            --     --  of a descending request and the request to be made will be an ascending one.
            --     Last_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
            --  end if;

         --  Descending order
         else
            --  From Last_Line_Value Take 10 results after in descending order
            Query_Result := Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Db_Key, Build_Condition (Object, Last_Line_Value, "DESC"));

            --  Crop to LIMIT value to display a complete list - buggy, counter intuitive then deleted
            --  Query_Result_Count := Field_Count (Query_Result, RD);
            --  if Query_Result_Count < To_Integer (List_Limit) then
            --     --  Get the last obtained key result
            --     Last_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
            --     --  Go back LIMIT results in ascending order
            --     Query_Result := Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Db_Key, Build_Condition (Object, Last_Line_Value));
            --     Query_Result_Count := Field_Count (Query_Result, RD);
            --     --  Get the last obtained key result, which will become the first, since it is the result
            --     --  of an ascending request and the request to be made will be a descending one.
            --     Last_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
            --  end if;

         end if;
         List (Object, Last_Line_Value);
      end if;
   end On_Down;

   ----------------------------------------------------------------------------
   procedure On_Up (Object : in out GGB.Base_Type'Class) is
      First_Line_Value : String := Get (Object, Get (Object, "List_Key") & "_First");
      Query_Result : String := "";
      Query_Result_Count : Natural;
      Db_Key : String := Get (Object, "Db_Key");
      List_Limit : String := Get (Object, "List_Limit");
   begin
      if Gui.Get_Display_In_Progress (Object) then
         Msg.Debug ("v22.Crud > On_Up > Display in progress, action deleted");
         delay 1.0; --  Avoid display collision
         Gui.Set_Display_In_Progress (Object, False);
      else
         Msg.Debug ("v22.Crud > On_Up > " & Get (Object, "List_Key"));
         --  Ascending order
         if Index (Get (Object, "List_Order"), "DESC") = 0 then
            --  From First_Line_Value Take 10 results before in descending order
            Query_Result := Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Db_Key, Build_Condition (Object, First_Line_Value, "DESC"));
            --  Count the results obtained
            Query_Result_Count := Field_Count (Query_Result, RD);
            --  Get the last obtained key result
            First_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
            --  Crop to LIMIT value to display a complete list
            if Query_Result_Count < To_Integer (List_Limit) then
               --  Get the last obtained key result, which will become the first, since it is the result
               --  of a descending request and the request to be made will be an ascending one.
               Query_Result_Count := Field_Count (Query_Result, RD);
               First_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
            end if;
         --  Descending order
         else
            --  From First_Line_Value Take LIMIT results before in ascending order
            Query_Result := Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Db_Key, Build_Condition (Object, First_Line_Value, "FORWARD"));
            --  Count the results obtained
            Query_Result_Count := Field_Count (Query_Result, RD);
            --  Get the last obtained key result
            First_Line_Value := Field_By_Index (Query_Result, Query_Result_Count, RD);
         end if;
         List (Object, First_Line_Value);
      end if;
   end On_Up;

   ----------------------------------------------------------------------------
   procedure On_Filter_Off (Object : in out GGB.Base_Type'Class) is
   begin
      Gui.Connection_Data_Set (Object, "List_Where", Get (Object, "List_Where_Memory"));
      Gui.Connection_Data_Set (Object, "List_Name", Get (Object, "List_Name_Memory"));
      Set_Limits (Object);
      On_Refresh (Object);
   end On_Filter_Off;

   ----------------------------------------------------------------------------
   procedure On_Refresh (Object : in out GGB.Base_Type'Class) is
   begin
      Msg.Debug (Get (Object, "List_Key") & " > On_Refresh");
      List (Object);
   end On_Refresh;

   ----------------------------------------------------------------------------
   procedure On_Validate_Delete (Object : in out Gnoga.Gui.Base.Base_Type'Class) is
      Data_Value : String := Gui.Connection_Data_Get (Object, Gui.Crud.Get (Object, "List_Key") & "_Select");
   begin
      Msg.Debug (Get (Object, "List_Key") & " > On_Validate_Delete > Key: " & Sql.Escape_String (Data_Value));
      Sql.Delete (Get (Object, "Db_Name"), Get (Object, "Db_Table"),
                      Get (Object, "Db_Key") & " = '" & Sql.Escape_String (Data_Value) & "'");
      List (Object);
   end On_Validate_Delete;

   ----------------------------------------------------------------------------
   procedure Put_Error_Message (Object : in out Gnoga.Gui.Base.Base_Type'Class; Error : String) is
   begin
      Gui.Content_Group_Warning_Set (Object, "register-error", Error);
   end Put_Error_Message;

   ----------------------------------------------------------------------------
   function Read (Object : in out Gnoga.Gui.Base.Base_Type'Class; Key : String; Query : String; Where : String := "") return String is
      DB_Where, DB_Result : String;
   begin
      --  Set read condition
      DB_Where := "WHERE " & Gui.Crud.Get (Object, "Db_Key") & " = '" & Sql.Escape_String (Key) & "'";
      if not Is_Empty (Where) then
         DB_Where := DB_Where & " AND " & Where;
      end if;
      Msg.Debug (Get (Object, "List_Key") & " > Put_Form > Key: " & Get (Object, "List_Key") & "_" & Key);
      --  Keep original Key value for update check
      DB_Result := Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Get (Object, "Db_Key"), DB_Where);
      Gui.Connection_Data_Set (Object, "Db_Key_Original", DB_Result);
      Msg.Debug (Get (Object, "List_Key") & " > Put_Form > Db_Key_Original: " & Get (Object, "Db_Key_Original"));
      --  Record read
     return Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Query, DB_Where);
   end Read;

   ----------------------------------------------------------------------------
   function Read_By_Id (Object : in out Gnoga.Gui.Base.Base_Type'Class; Query : String) return String is
      Id : String := Get (Object, Get (Object, "List_Key") & "_Select_Id");
      DB_Where, DB_Result : String;
   begin
      --  Set read condition
      DB_Where := "WHERE Id = " & Id;
      Msg.Debug (Get (Object, "List_Key") & " > Put_Form > Id: " & Id);
      --  Record read
      return Sql.Read (Get (Object, "Db_Name"), Get (Object, "Db_Table"), Query, DB_Where);
   end Read_By_Id;

   ----------------------------------------------------------------------------
   function Search (Object : in out Gnoga.Gui.Base.Base_Type'Class; Key : String; Message_Not_Found : String) return Boolean is
      Result : Boolean := False;
   begin
      if not Is_Empty (Key) then
         if Sql.Search (Get (Object, "Db_Name"),
                        Get (Object, "Db_Table"),
                        Get (Object, "Db_Key") & " = '" & Sql.Escape_String (Key) & "'") then
            Result := True;
         else
            Put_Error_Message (Object, Message_Not_Found);
         end if;
      end if;
      return Result;
   end Search;

   ----------------------------------------------------------------------------
   procedure Set_Limits (Object : in out GGB.Base_Type'Class) is
      Descending : Boolean := (Index (Get (Object, "List_Order"), "DESC") > 0);
      Join : String := Get (Object, "List_Join");
      Where : String := Get (Object, "List_Where");
      Order : String := Get (Object, "List_Order");
      First,First_Save,Last : String;
   begin
      --  Join ----------------------------------------------------------------
      if not Is_Empty (Join) then
         Join := " INNER JOIN " & Join & " ";
      end if;

      --  Where ---------------------------------------------------------------
      if not Is_Empty (Where) then
         Where := " WHERE " & Where & " ";
      end if;

      --  Order ---------------------------------------------------------------
      if Is_Empty (Order) then
         Order := Get (Object, "Db_Key");
      end if;
      Order := Replace (Replace (Order, "DESC", ""), "ASC", "");
      Order := " ORDER BY " & Order & " ";

      --  Assemble final condition --------------------------------------------
      First := "SELECT " & Get (Object, "Db_Table") & ".Id FROM " & Get (Object, "Db_Table") & Join & Where & Order & " " & "ASC" & " LIMIT 1";
      Last := "SELECT " & Get (Object, "Db_Table") & ".Id FROM " & Get (Object, "Db_Table") & Join & Where & Order & " " & "DESC" & " LIMIT 1";
      if Descending then
         First_Save := First;
         First := Last;
         Last := First_Save;
      end if;
      Msg.Debug ("First query: " & First);
      Msg.Debug ("Last query: " & Last);

      --  Get results ---------------------------------------------------------
      declare
         DBT : Sql.Database_Line_Type;
      begin
         DBT := Sql.Properties (Get (Object, "Db_Name"));
         if DBT.Brand = Sql.MySQL then
            declare
               RS : GSD.Recordset'Class := DBT.DBM.Query (First);
            begin
               First := (if RS.Next then RS.Field_Value (1) else "0");
               RS.Close;
            end;
            declare
               RS : GSD.Recordset'Class := DBT.DBM.Query (Last);
            begin
               Last := (if RS.Next then RS.Field_Value (1) else "0");
               RS.Close;
            end;
         elsif DBT.Brand = Sql.SQLite then
            declare
               RS : GSD.Recordset'Class := DBT.DBM.Query (First);
            begin
               First := (if RS.Next then RS.Field_Value (1) else "0");
               RS.Close;
            end;
            declare
               RS : GSD.Recordset'Class := DBT.DBM.Query (Last);
            begin
               Last := (if RS.Next then RS.Field_Value (1) else "0");
               RS.Close;
            end;
         else
            Msg.Error ("Gui.Crud.Set_Limits > Properties not found for " & Get (Object, "Db_Table"));
         end if;
      end;
      Msg.Debug ("First result: " & First);
      Msg.Debug ("Last result: " & Last);
      Gui.Connection_Data_Set (Object, Get (Object, "List_Key") & "_First_Id", First);
      Gui.Connection_Data_Set (Object, Get (Object, "List_Key") & "_Last_Id", Last);
   end Set_Limits;

   ----------------------------------------------------------------------------
   procedure Title (Object : in out Gnoga.Gui.Base.Base_Type'Class; Mode : String; Title : String := "") is
      Edit_Title : String := Title & " - ";
   begin
      Gui.Content_Clear_Title (Object);
      Gui.Content_Clear_Text (Object);
      if Mode = DB_CREATE then
         Edit_Title := Edit_Title & "Création";
      elsif Mode = DB_READ then
         Edit_Title := Edit_Title & "Visualisation";
      elsif Mode = DB_UPDATE then
         Edit_Title := Edit_Title & "Modification";
      elsif Mode = DB_DELETE then
         Edit_Title := Edit_Title & "Suppression";
      elsif Mode = DB_SEARCH then
         Edit_Title := Edit_Title & "Recherche";
      elsif Mode = DB_FILTER then
         Edit_Title := Edit_Title & "Filtrage";
      elsif Mode = DB_EXPORT then
         Edit_Title := Edit_Title & "Exportation";
      else
         Edit_Title := Edit_Title & Mode;
      end if;
      Gui.Content_Put_Title (Object, Edit_Title);
      Msg.Debug (Get (Object, "List_Key") & " > " & Edit_Title);
   end Title;

   ----------------------------------------------------------------------------
   procedure Write (Object : in out Gnoga.Gui.Base.Base_Type'Class;
                    Operation : String;
                    Key : String;
                    Query : String;
                    Message_Key_Already_Exists : String;
                    Message_Key_Is_Missing : String) is
      Data_Value : String;

      procedure Update is
         Data_Value : String;
      begin
         Sql.Update (Get (Object, "Db_Name"),
                     Get (Object, "Db_Table"),
                     Query,
                     Get (Object, "Db_Key") & " = '" &  Sql.Escape_String (Get (Object, "Db_Key_Original")) & "'");
         Data_Value := Gui.Connection_Data_Get (Object, Get (Object, "List_Key") & "_Select");
         --  If key changed, don't return to the original value to avoid a blank list if the original value was the last entry
         if Get (Object, "Db_Key_Original") /= Key then
            Data_Value := "";
         end if;
         List (Object, Data_Value);
      end Update;
   begin
      if not Is_Empty (Key) then
         if not Sql.Search (Get (Object, "Db_Name"),
                            Get (Object, "Db_Table"), Get (Object, "Db_Key") & " = '" & Sql.Escape_String (Key) & "'") then
            if Operation = DB_CREATE then
               Sql.Insert (Get (Object, "Db_Name"),
                           Get (Object, "Db_Table"), Query);
               List (Object);
            elsif Operation = DB_UPDATE then
               Update;
            end if;
         else
            --  If original key and actual key values are identical, we can update
            --  since we are now shure the record found is the record beeing modified
            if Get (Object, "Db_Key_Original") = Sql.Escape_String (Key) then
               Update;
            else
               Put_Error_Message (Object, Message_Key_Already_Exists);
            end if;
         end if;
      else
         Put_Error_Message (Object, Message_Key_Is_Missing);
      end if;
   end Write;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Build_Condition (Object : in out GGB.Base_Type'Class;
                             Key : String := "";
                             Order_Direction : String := "") return String is
      --  Order_Direction :
      --  DESC: suppresses List_Join and List_Order for On_Up operation.
      --  FORWARD: select operators
      Join : String := Get (Object, "List_Join");
      Where_Part_1, Where : String := "";
      Where_Part_2 : String := Get (Object, "List_Where");
      Order : String :="";
      Condition, Operator : String;
   begin
      --  Join ----------------------------------------------------------------
      if not Is_Empty (Join) then
         Join := "INNER JOIN " & Join & " ";
      end if;

      --  Where ---------------------------------------------------------------
      if not Is_Empty (Key) then
         --  Ascending order
         if (Index (Get (Object, "List_Order"), "DESC") = 0) and (Index (Order_Direction, "FORWARD") = 0) then
            Operator := From_Latin_1 ((if Is_Empty (Order_Direction) then ">=" else "<="));
         --  Descending order
         else
            Operator := From_Latin_1 ((if Is_Empty (Order_Direction) then "<=" else ">="));
         end if;
         Where_Part_1 := Get (Object, "Db_Key") & " " & Operator & " '" & Sql.Escape_String (Key) & "'";
      end if;
      if not Is_Empty (Where_Part_2) then
         Where_Part_2 := From_Latin_1 ((if Is_Empty (Where_Part_1) then "" else " AND ")) & Where_Part_2;
      end if;
      if not (Is_Empty (Where_Part_1) and Is_Empty (Where_Part_2)) then
         Where := "WHERE " & Where_Part_1 & Where_Part_2 & " ";
      end if;

      --  Order ---------------------------------------------------------------
      --  If no Order_Direction specified, get List_Order
      if Is_Empty (Order_Direction) then
         Order := Get (Object, "List_Order");
      end if;
      --  If no order specified - list order is empty or no Order_Direction - choose Db_Key as default order
      if Is_Empty (Order) then
         Order := Get (Object, "Db_Key");
      end if;
      --  Add Order_Direction if specified
      if not Is_Empty (Order_Direction) then
         Order := Order & " " & Order_Direction;
      end if;
      Order := Trim_Both (Order);
      --  If Order exists
      if not Is_Empty (Order) then
         --  Assemble Order query
         Order := "ORDER BY " & Order & " ";
         --  Suppress Descending Order if Forward specified - for On_Up command list with descending order
         if Index (Order_Direction, "FORWARD") > 0 then
            Order := Replace (Order, "DESC", "");
         end if;
         --  Filter Forward keyword to avoid SQL error
         Order := Replace (Order, "FORWARD", "");
      end if;

      --  Assemble final condition --------------------------------------------
      Condition := Join & Where & Order & "LIMIT " & Get (Object, "List_Limit");

      Msg.Debug ("Gui.Crud.Build_Condition > Condition: " & Condition);
      return Condition;
   end Build_Condition;

-------------------------------------------------------------------------------
end v22.Gui.Crud;
-------------------------------------------------------------------------------
