-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-sql.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - MariaDB & SQLite binding
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io (high level SQLite binding, some low level binding hacks)
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Exceptions;
with Ada.IO_Exceptions;

with Ada.Iterator_Interfaces; -- Vérifier si nécessaire

with Ada.Tags;

with v22.Fls;
with v22.Msg;
with v22.Tio;

package body v22.Sql is

   --package AE  renames Ada.Exceptions;

   --  Private subprograms

   ----------------------------------------------------------------------------

   --  Public subprograms

   ----------------------------------------------------------------------------
   function Brand (DB : in out GSD.Connection'Class) return Database_Brand is
      --  {GNOGA.SERVER.DATABASE.MYSQL.CONNECTION object}
      --  {GNOGA.SERVER.DATABASE.SQLITE.CONNECTION object}
      DB_Brand : String := Field_By_Index (From_Latin_1 (Ada.Tags.External_Tag (DB'Tag)),4,".");
      Result : Database_Brand := Unknown;
   begin
      if DB_Brand = "MYSQL" then
         Result := MySQL;
      elsif DB_Brand = "SQLITE" then
         Result := SQLite;
      end if;
      return Result;
   end Brand;

   ----------------------------------------------------------------------------
   procedure Close (DB : in out GSD.Connection'Class) is
   begin
      GSD.Disconnect (DB);
   end Close;

   ----------------------------------------------------------------------------
   procedure Close is
   begin
      for C in Databases.Iterate loop
         if Databases(C).Brand = MySQL then
            Databases(C).DBM.Disconnect;
         elsif Databases(C).Brand = SQLite then
            Databases(C).DBS.Disconnect;
         end if;
         Msg.Std ("Closing " & From_Latin_1 (Databases(C).Brand'Image) & " database: " & Databases(C).Name);
      end loop;
   end Close;

   ----------------------------------------------------------------------------
   function Column_Exists (DB : in out GSD.Connection'Class;
                           Table_Name : String;
                           Column_Name : String) return Boolean is
      Fields : Gnoga.Types.Data_Array_Type;
      Result : Boolean := False;
   begin
      if Table_Exists (DB, Table_Name) then
         Fields := DB.List_Fields_Of_Table (Table_Name);
         for I in 1 .. Natural (Fields.Length) loop
            if Fields.Element (I) = Column_Name then
               Result := True;
               exit;
            end if;
         end loop;
      else
         Msg.Err ("Sql.Column_Exists > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
      return Result;
   end Column_Exists;

   ----------------------------------------------------------------------------
   procedure Delete (DB : in out GSD.Connection'Class;
                     Table_Name : String;
                     Where_Condition : String) is
      Request : String;
   begin
      if Table_Exists (DB, Table_Name) then
         Request := "DELETE FROM " & Table_Name & " WHERE " & Where_Condition;
         Msg.Dbg ("Delete_From: " & Request);
         DB.Execute_Query (Request);
      else
         Msg.Err ("Sql.Delete > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   function From_Money (DB_Money : Money) return Integer is
   begin
      return Integer (DB_Money * 100);
   end From_Money;

   ----------------------------------------------------------------------------
   function Get_Config (DB : in out GSD.Connection'Class;
                        Parameter : String) return String is
      Result_Statement : String := "";
   begin
      declare
         RS : GSD.Recordset'Class := DB.Query (
         "SELECT Value FROM " & Table_Sys_Config & " WHERE Parameter='" & Parameter & "'");
      begin
         if Sql.Table_Exists (DB, Table_Sys_Config) then
            if Sql.Column_Exists (DB, Table_Sys_Config, "Parameter") and
              Sql.Column_Exists (DB, Table_Sys_Config, "Value") then
               while RS.Next loop
                  Result_Statement := RS.Field_Value (1);
               end loop;
               RS.Close;
            end if;
         end if;
      end;
      return Result_Statement;
   exception
      when Error : GSD.Query_Error =>
         -- To handle first launch when database not created yet
         return Result_Statement;
   end Get_Config;

   ----------------------------------------------------------------------------
   function Get_Version (DB : in out GSD.Connection'Class) return String is
      DB_Type : Database_Brand := Brand (DB);
      DB_Version : String := "";
   begin
      if DB_Type = MySQL then
         declare
            RS : GSD.Recordset'Class := DB.Query ("SELECT @@Version");
         begin
            while RS.Next loop
               DB_Version := "MySQL: v" & RS.Field_Value (1);
            end loop;
            RS.Close;
         end;
      elsif DB_Type = SQLite then
         DB_Version := "SQLite: v" & From_Latin_1 (ICS.Value (Sqlite3_Libversion));
      elsif DB_Type = Unknown then
         DB_Version := "Database Unknown";
      end if;
      return DB_Version;
   end Get_Version;

   ----------------------------------------------------------------------------
   function Index_Exists (DB : in out GSD.Connection'Class;
                          Table_Name : String;
                          Index_Name : String) return Boolean is
      DB_Type : Database_Brand := Brand (DB);
      Result : Boolean;
   begin
      if Table_Exists (DB, Table_Name) then
         if DB_Type = MySQL then
            Result := (DB.Execute_Update ("SHOW INDEX FROM " & Table_Name &
                              " WHERE Key_name = '" & Index_Name & "'") = 1);
         elsif DB_Type = SQLite then
            -- When SQLite API fixed, replace by:
            -- Result := (DB.Execute_Update ("SELECT * FROM sqlite_master WHERE type = 'index'" &
            -- " and tbl_name = '" & Table_Name  & "' and name = '" & Index_Name & "'") = 1);
            declare
               RS : GSD.Recordset'Class := DB.Query ("SELECT * FROM sqlite_master WHERE type = 'index'" &
                                   " and tbl_name = '" & Table_Name  & "' and name = '" & Index_Name & "'");
            begin
               Result := RS.Next;
               RS.Close;
            end;
         end if;
      else
         Msg.Err ("Sql.Index_Exists > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
      return Result;
   end Index_Exists;

   ----------------------------------------------------------------------------
   procedure Insert (DB : in out GSD.Connection'Class;
                     Table_Name : String;
                     Columns_Values : String) is
      Description_List : GSD.Field_Description_Array_Type;
      Description : GSD.Field_Description;
      Current_Column, Current_Value, Current_Type, Insert_Columns_Names,
      Insert_Columns_Values, Sql_Request : String := "";
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);
   begin
      if Table_Exists (DB, Table_Name) then
         Description_List := DB.Field_Descriptions (Table_Name);
         --  Msg.Dbg ("Counter_Columns: " & To_String (Natural (Description_List.Last_Index)));
         --  Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            --  Iterate through each column
            for I in Description_List.First_Index .. Description_List.Last_Index loop
               Description := Description_List.Element (I);
               Current_Column := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND);
               --  Msg.Std (I);
               --  Msg.Std (Description_List.Last_Index);
               --  Msg.Dbg ("Current_Columns_Values: " & Current_Column);
               --  Msg.Dbg ("Current_Column_Database: " & To_Upper (Description.Column_Name));
               --  If field name and column name match
               if To_Upper (Current_Column) = To_Upper (Description.Column_Name) then
                  --  Fill Name and Value, according to field type
                  Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
                  Current_Type := To_Upper (Slice (Description.Data_Type, 1, 3));
                  Insert_Columns_Names := Insert_Columns_Names & Current_Column & ",";
                  --  Msg.Dbg  ("Field Type: " & Current_Type);
                  --  Apply, depending of the column type
                  if Current_Type = "INT" then -- MySQL INT but SQLite INTEGER
                     Insert_Columns_Values := Insert_Columns_Values & Current_Value & ",";
                  elsif Current_Type = "TEX" then
                     --  Single quotes outside string and, inside string, escape single quote with pair of single quotes
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Replace_Pattern (Current_Value, "'", "''") & "',";
                  elsif Current_Type = "BLO" then
                     --  Single quotes outside string and, inside string, escape single quote with pair of single quotes
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Replace_Pattern (Current_Value, "'", "''") & "',";
                  else
                     Msg.Err ("Sql.Insert > Field: " & Current_Column & " does not handle Type: " & Current_Type);
                  end if;
                  exit; --  No need to iterate further after match
               else
                  if I = Description_List.Last_Index then
                     Msg.Err ("Sql.Insert > Field: " & Current_Column & " does not exists in Table: " & Table_Name);
                  end if;
               end if;
            end loop;
         end loop;
         --  Msg.Dbg ("Insert_Columns_Names: " & Insert_Columns_Names);
         --  Msg.Dbg ("Insert_Columns_Values: " & Insert_Columns_Values);
         --  If at least one Field/Value pair has been processed
         if (Index (Insert_Columns_Names, ",") > 0) and
            (Index (Insert_Columns_Values, ",") > 0) then
            --  Trailing comma deletion
            Insert_Columns_Names := Slice (Insert_Columns_Names, 1, Length (Insert_Columns_Names) - 1);
            Insert_Columns_Values := Slice (Insert_Columns_Values, 1, Length (Insert_Columns_Values) - 1);

            Sql_Request := "INSERT INTO " & Table_Name & " (" & Insert_Columns_Names & ") VALUES (" & Insert_Columns_Values & ");";
            --  Msg.Dbg ("Insert_Into: " & Sql_Request);
            DB.Execute_Query (Sql_Request);
         end if;
      else
         Msg.Err ("Sql.Insert > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
   end Insert;

   ----------------------------------------------------------------------------
   function Last_RowID (DB : in out GSD.Connection'Class;
                        Table_Name : String) return Integer is
      DB_Type : Database_Brand := Brand (DB);
      Result : Natural := 0;
   begin
      if Table_Exists (DB, Table_Name) then
         if DB_Type = MySQL then
            Result := DB.Execute_Update ("SELECT MAX(id) FROM " & Table_Name);
         elsif DB_Type = SQLite then
            declare
               RS : GSD.Recordset'Class := DB.Query ("SELECT MAX(rowid) FROM " & Table_Name);
            begin
               if RS.Next then
                  Result := To_Integer (RS.Field_Value (1));
               end if;
               RS.Close;
            end;
         end if;
      else
         Msg.Err ("Sql.Last_RowID > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
      return Result;
   end Last_RowID;

   ----------------------------------------------------------------------------
   function DB_Index (DB_Name : String) return Databases_List.Extended_Index is
      use Databases_List; -- for operators
      DB_Index : Databases_List.Extended_Index := Databases_List.No_Index;
   begin
      for C in Databases.Iterate loop
         --Msg.Dbg ("Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (To_Index (C))));
         if  Databases(C).Name = DB_Name then
              DB_Index := Databases_List.Extended_Index (To_Index (C));
            exit;
         end if;
      end loop;
      return DB_Index;
   end DB_Index;

   ----------------------------------------------------------------------------
   function Open (DBM : in out GSD.MySQL.Connection;
                   URI : String := "";
                   Version : String := "") return Database_Status is
      DB_Brand : Database_Brand := MySQL;
      DB_Status : Database_Status := None;
      DB_URI : String := URI;
      DB_Name, DB_Host, DB_User, DB_Password, DB_File, DB_Temp : String;
      DB_Port : Natural := 0;
      DB_URI_Parameter, DB_URI_Parameter_Name : String := "";
   begin
      --  MySQL:  db:db_name?host=192.168.0.243&port=3306&user=user_name&password=user_password
      --  db_name
      DB_Name := Field_By_Index (Field_By_Index (DB_URI, 2, ":"), 1, "?");
      --  host=192.168.0.243&port=3306&user=user_name&password=user_password
      DB_Temp := Field_By_Index (DB_URI, 2, "?");
      if Field_Count (DB_Temp, "&") >= 1 then
         --  Default port for MySQL
         DB_Port := 3306;
         for I in 1 .. Field_Count (DB_Temp, "&") loop
            DB_URI_Parameter := Field_By_Index (DB_Temp, I, "&");
            DB_URI_Parameter_Name := To_Upper (Field_By_Index (DB_URI_Parameter, 1, "="));
            if DB_URI_Parameter_Name = "HOST" then
               DB_Host := Field_By_Index (DB_URI_Parameter, 2, "=");
            elsif DB_URI_Parameter_Name = "PORT" then
               DB_Port := To_Integer (Field_By_Index (DB_URI_Parameter, 2, "="));
            elsif DB_URI_Parameter_Name = "USER" then
               DB_User := Field_By_Index (DB_URI_Parameter, 2, "=");
            elsif DB_URI_Parameter_Name = "PASSWORD" then
               DB_Password := Field_By_Index (DB_URI_Parameter, 2, "=");
            end if;
         end loop;
         declare
         begin
            DBM.Connect (Database => DB_Name,
                        Host => DB_Host,
                        Port => DB_Port,
                        User => DB_User,
                        Password => DB_Password);
            DB_Status := Open_Success;
         exception
            when Error : GSD.Connection_Error =>
               DB_Status := Open_Failed;
               Msg.Err ("v22.Sql.Open > MySQL database " & DB_Name & " failed" &
                        (if Empty (DBM.Error_Message) then From_ASCII ("") else From_ASCII (" > ") & DBM.Error_Message));
         end;
      end if;
      if (DB_Status = Open_Success) then
         Open_Load (DBM, SQLite, DB_Status, DB_URI, DB_Name, DB_Host, DB_Port, DB_User, DB_Password, DB_File, Version);
      end if;
      return DB_Status;
   end Open;

   ----------------------------------------------------------------------------
   function Open (DBS : in out GSD.SQLite.Connection;
                   URI : String := "";
                   Version : String := "") return Database_Status is
      DB_Brand : Database_Brand := SQLite;
      DB_Status : Database_Status := None;
      DB_URI : String := URI;
      DB_Name, DB_Host, DB_User, DB_Password, DB_File, DB_Temp : String;
      DB_Port : Natural := 0;
   begin
      -- SQLite: file:data.db or file:data.db?mode=ro&cache=private
      -- SQLite has been set in RFC 3986 URI mode, almost nothing to do
      DB_Temp := Field_By_Index (DB_URI, 2, ":");
      DB_File := Field_By_Index (DB_Temp, 1, "?");
      DB_Name := Field_By_Index (Tail_After_Match (DB_File, "/"), 1, ".");
      declare
      begin
         DBS.Connect (DB_URI);
         DB_Status := Open_Success;
      exception
         when Error : GSD.Connection_Error =>
            DB_Status := Open_Failed;
            Msg.Err ("v22.Sql.Open > SQlite Database " & DB_Name & " failed" &
                     (if Empty (DBS.Error_Message) then From_ASCII(" ") else From_ASCII(" > ") & DBS.Error_Message));
      end;
      if (DB_Status = Open_Success) then
         Open_Load (DBS, SQLite, DB_Status, DB_URI, DB_Name, DB_Host, DB_Port, DB_User, DB_Password, DB_File, Version);
      end if;
      return DB_Status;
   end Open;

   ----------------------------------------------------------------------------
   procedure Open_Load (DB : in out GSD.Connection'Class;
                        DB_Brand : Database_Brand;
                        DB_Status : in out Database_Status;
                        DB_URI : String;
                        DB_Name : String;
                        DB_Host: String;
                        DB_Port : Natural;
                        DB_User : String;
                        DB_Password : String;
                        DB_File : String;
                        DB_Version : String) is
      Get_Schema_Version : String;
      DB_Schema_Version : Natural;
      --DB_DBC : GSD.Connection;
      --DB_DBU : GSD.Connection_Access'Class;
      DB_DBS : GSD.SQLite.Connection;
      DB_DBM : GSD.MySQL.Connection;
      DB_Major : constant Natural := To_Integer (Field_By_Index (DB_Version,1,"."));
      DB_Minor : constant Natural := To_Integer (Field_By_Index (DB_Version,2,"."));
      Schema_Version : constant Natural := (DB_Major * 10) + DB_Minor;
   begin
      --  Preload Database definition
      Schema_Load (Database_Name, DB_Name);
      if DB_Brand = MySQL then
         DB_DBM := GSD.MySQL.Connection (DB);
         null;
      elsif DB_Brand = SQLite then
         DB_DBS := GSD.SQLite.Connection (DB);
         Schema_Load (Database_Pragma,"journal_mode","WAL");
         -- sqlite doc : As of SQLite version 3.6.19, the default setting for foreign key enforcement is OFF.
         Schema_Load (Database_Pragma,"foreign_keys","ON");
         -- Open DB and eventually apply pragmas
         for I of Schema loop
            if I.Command = Database_Name then
               --  Do nothing
               null;
            elsif I.Command = Database_Pragma then
               Msg.Dbg ("Load Database_Pragma: " & I.Name & "=" & I.Attribute);
               DB.Execute_Query ("PRAGMA " & I.Name & "=" & I.Attribute);
            else
               exit;
            end if;
         end loop;
      end if;

      -- Preload Sys_Config table definition
      Schema_Load (Table_Name, Table_Sys_Config);
      Schema_Load (Column_Name, "Parameter", "TEXT");
      Schema_Load (Column_Constraint, "Parameter", "UNIQUE");
      Schema_Load (Table_Constraint, "Parameter", "PRIMARY KEY");
      Schema_Load (Column_Name, "Value", "TEXT");
      Schema_Load (Index_Name, "Idx_Config_Parameter","Parameter");

      -- Preload Sys_Schema table definition
      Schema_Load (Table_Name, Table_Sys_Schema);
      Schema_Load (Column_Name, "Table_Name", "TEXT");
      Schema_Load (Column_Name, "Column_Name", "TEXT");
      Schema_Load (Column_Name, "Column_Type", "TEXT");
      Schema_Load (Column_Name, "Column_Constraint", "TEXT");
      Schema_Load (Column_Name, "Version", "TEXT");
      Schema_Load (Column_Name, "Comment", "TEXT");

      -- Load if exists current database schema version else Database_Version=0
      Get_Schema_Version := Get_Config (DB, "Schema_Version");
      DB_Schema_Version := (To_Integer (Field_By_Index (Get_Schema_Version, 1, ".")) * 10) +
                            To_Integer (Field_By_Index (Get_Schema_Version, 2, "."));
      -- False if DB_Schema_Version is >= Schema_Version => no need updating
      if (DB_Schema_Version < Schema_Version) then
         DB_Status := Open_Need_Update;
      end if;

      --  Database_Line'(record) mandatory with GCC 11,
      --  See http://www.ada-auth.org/cgi-bin/cvsweb.cgi/ai12s/ai12-0400-1.txt?rev=1.3
      Databases.Append (Database_Line'(Brand => DB_Brand,
                                       Status => DB_Status,
                                       URI => DB_URI,
                                       Name => DB_Name,
                                       Host => DB_Host,
                                       Port => DB_Port,
                                       User => DB_User,
                                       Password => DB_Password,
                                       File => DB_File,
                                       -- Store DB_Version for later writing of
                                       -- this current version in Schema_Update,
                                       -- which must be done at the very end of
                                       -- the update process to ensure the
                                       -- completion of the update
                                       Version => DB_Version,
                                       DBS => DB_DBS,
                                       DBM => DB_DBM
                                       ));
      --  Msg.Std (40 * "-");
      --  -- Index
      --  DB_Index : Databases_List.Extended_Index;
      --  DB_Index := Databases.Last_Index;
      --  Msg.Dbg ("DB_Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (DB_Index)));
      --  Msg.Dbg ("URI:      " & Databases(DB_Index).URI);
      --  -- Last_Element
      --  Msg.Dbg ("URI:      " & Databases.Last_Element.URI);
      --  Msg.Dbg ("DBM:      " & From_Latin_1 (Databases.Last_Element.DBM'Image));
      --  Msg.Dbg ("DBS:      " & From_Latin_1 (Databases.Last_Element.DBS'Image));
      --  Msg.Dbg ("URI:      " & Databases.Last_Element.URI);
      --  Msg.Dbg ("Name:     " & Databases.Last_Element.Name);
      --  Msg.Dbg ("Host:     " & Databases.Last_Element.Host);
      --  Msg.Dbg ("Port:     " & To_String (Databases.Last_Element.Port));
      --  Msg.Dbg ("User:     " & Databases.Last_Element.User);
      --  Msg.Dbg ("Password: " & Databases.Last_Element.Password);
      --  Msg.Dbg ("File:     " & Databases.Last_Element.File);
      --  Msg.Dbg ("Major:    " & To_String (Databases.Last_Element.Major));
      --  Msg.Dbg ("Minor:    " & To_String (Databases.Last_Element.Minor));
      --  Msg.Std (40 * "-");

   end Open_Load;

   ----------------------------------------------------------------------------
   function Properties (DB_Name : String) return Database_Line is
      use Databases_List; -- for operators
      DB_Index : Databases_List.Extended_Index := Databases_List.No_Index;
      DB_Record : Database_Line;
   begin
      for C in Databases.Iterate loop
         --Msg.Dbg ("Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (To_Index (C))));
         if  Databases(C).Name = DB_Name then
              DB_Record := Databases(C);
            exit;
         end if;
      end loop;
   return DB_Record;
   end Properties;

   ----------------------------------------------------------------------------
   function Read (DB : in out GSD.Connection'Class;
                  Table_Name : String;
                  Columns : String;
                  Condition : String := "") return String is
      Sql_Request : String := "SELECT " & Columns & " FROM " & Table_Name & " " & Condition;
      RS : GSD.Recordset'Class := DB.Query (Sql_Request);
      Counter_Columns : constant Natural:= Field_Count (Columns, ",");
      Sql_Result : String := "";
   begin
      if Table_Exists (DB, Table_Name) then
         --  Msg.Dbg ("Sql_Request: " & Sql_Request);
         --  Iterate result(s) line(s)
         if Sql.Table_Exists (DB, Table_Sys_Config) then
            --  Column existance test (TBD)
            if True then
               while RS.Next loop
                  --  Iterate choosen columns
                  for Index in 1..RS.Number_Of_Fields loop
                     --  Msg.Dbg ("Current_Column_Text: " & Column_Text (Local_Statement, Index));
                     Sql_Result := Sql_Result & RS.Field_Value (Index) & CD;
                  end loop;
                  if (Index (Sql_Result, CD) > 0) then
                     --  Delete last CD and add row delimiter
                     Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1) & RD;
                  end if;
               end loop;
            end if;
         end if;
         --  Delete last RD
         if Length (Sql_Result) >= 2 then -- to handle one digit answer with a trailing RD = 2 chars
            if Slice (Sql_Result, Length (Sql_Result), Length (Sql_Result)) = RD then
               Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1);
            end if;
         end if;
         --  Msg.Dbg ("Read: " & Sql_Result);
      else
         Msg.Err ("Sql.Read > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
      RS.Close;
      return Sql_Result;
   end Read;

   ----------------------------------------------------------------------------
   function Row_Count (DB : in out GSD.Connection'Class;
                       Table_Name : String;
                       Option : String := "*") return Integer is
      DB_Type : Database_Brand := Brand (DB);
      Result : Natural := 0;
   begin
      if Table_Exists (DB, Table_Name) then
         if DB_Type = MySQL then
            declare
               RS : GSD.Recordset'Class := DB.Query ("SELECT COUNT(" & Option & ") FROM " & Table_Name);
            begin
               while RS.Next loop
                  Result := To_Integer (RS.Field_Value (1));
               end loop;
               RS.Close;
            end;
         elsif DB_Type = SQLite then
            Result := DB.Execute_Update ("SELECT COUNT(" & Option & ") FROM " & Table_Name);
         end if;
      else
         Msg.Err ("Sql.Row_Count > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
      return Result;
   end Row_Count;

   ----------------------------------------------------------------------------
   procedure Schema_Load (Command : in Schema_Command := Null_Command;
                          Name : in String := "";
                          Attribute : in String := "";
                          Comment : in String := "") is
   begin
      Schema.Append (Schema_Line'(Command, Name, Attribute, Comment));
   end Schema_Load;

   ----------------------------------------------------------------------------
   procedure Schema_Update (DB : in out GSD.Connection'Class) is

      DBT : Sql.Database_Line_Type;
      --  DB_Brand : Database_Brand;
      --  DB_Status : Database_Status;
      --  DB_URI : String;
      --  DB_Name : String;
      --  DB_Host: String;
      --  DB_Port : Natural;
      --  DB_User : String;
      --  DB_Password : String;
      --  DB_File : String;
      DB_Version : String;

      Current_Table_Name, Current_Table_Constraint, Current_Table_Comment,
      Current_Column_Name, Current_Column_Type, Current_Column_Constraint,
      Current_Column_Comment, Current_Index_Name, Current_Index_Key,
      Current_Index_Constraint : String := "";

      Current_Table_Not_Exists : Boolean := False;
      Columns_Counter, Index_Counter, Constraint_Counter : Natural := 0;

      --  type Parsing_States is (Idle, Init, Table, Column, Index,
      --                          Table_Name, Table_Constraint,
      --                          Column_Name, Column_Constraint,
      --                          Index_Name, Constraint);
      --
      --  State : Parsing_States := Idle;
      --  State_Memory :  Parsing_States := Init;

      ---------------------------------
      procedure Add_Sys_Schema is

      begin
         if Table_Exists (DB, Table_Sys_Schema) then
            if Current_Table_Name /= Table_Sys_Schema then
               if Column_Exists (DB, Table_Sys_Schema, "Table_Name") and
                 Column_Exists (DB, Table_Sys_Schema, "Column_Name") and
                 Column_Exists (DB, Table_Sys_Schema, "Column_Type") and
                 Column_Exists (DB, Table_Sys_Schema, "Version") and
                 Column_Exists (DB, Table_Sys_Schema, "Comment") then

                  DB.Execute_Query ("INSERT INTO " & Table_Sys_Schema &
                                      " (Table_Name, Column_Name, Column_Type, Column_Constraint, Version, Comment)" &
                                      " VALUES ('" &
                                      Current_Table_Name & "', '" &
                                      Current_Column_Name & "', '" &
                                      Current_Column_Type & "', '" &
                                      Current_Column_Constraint & "', '" &
                                      "+" & DB_Version  & "', '" &
                                      Current_Column_Comment & "')");
               end if;
            end if;
         end if;
      end Add_Sys_Schema;

      ---------------------------------
      procedure Clear_Column is
      begin
         Current_Column_Name := "";
         Current_Column_Type := "";
         Current_Column_Comment := "";
         Current_Column_Constraint := "";
         Constraint_Counter := 0;
      end Clear_Column;

      ---------------------------------
      procedure Create_Column is
      begin
         -- Test if non empty column name to handle table break
         -- when previous table column already exists
         if not Empty (Current_Column_Name) then
            if Column_Exists (DB, Current_Table_Name, Current_Column_Name) then
               Msg.Dbg ("Existing Table: " & Current_Table_Name &
                          " - Existing Column: " & Current_Column_Name &
                          " " & Current_Column_Type &
                          " " & Current_Column_Constraint);
            else
               Msg.Std ("Table: " & Current_Table_Name &
                          " - Create Column: " & Current_Column_Name &
                          " " & Current_Column_Type &
                          " " & Current_Column_Constraint);
               DB.Execute_Query ("ALTER TABLE " & Current_Table_Name  &
                              " ADD COLUMN " & Current_Column_Name &
                              " " & Current_Column_Type &
                              " " & Current_Column_Constraint);
               --  Add item in data dictionnary
               Add_Sys_Schema;
            end if;
            Clear_Column;
         end if;
      end Create_Column;

      ---------------------------------
      procedure Clear_Index is
      begin
         Current_Index_Name := "";
         Current_Index_Key := "";
         Current_Index_Constraint := "";
      end Clear_Index;

      ---------------------------------
      procedure Create_Index is
      begin
         -- Test if non empty column name to handle table break
         -- when previous table index already exists
         if not Empty (Current_Index_Name) then
            if Index_Exists (DB, Current_Table_Name, Current_Index_Name) then
               Msg.Dbg ("Existing Table: " & Current_Table_Name &
                          " - Existing Index: " & Current_Index_Name &
                          " " & Current_Index_Key &
                          " " & Current_Index_Constraint);
            else
               Msg.Std ("Table: " & Current_Table_Name &
                          " - Creating Index: " & Current_Index_Name &
                          " " & Current_Index_Key &
                          " " & Current_Index_Constraint);

               DB.Execute_Query ("CREATE " & Current_Index_Constraint &
                              " INDEX " & Current_Index_Name  &
                              " ON " & Current_Table_Name &
                              " (" & Current_Index_Key & ");");
            end if;
            Clear_Index;
         end if;
      end Create_Index;

      ---------------------------------
      procedure Create_Table is
      begin
         if Current_Table_Not_Exists then
            Msg.Std ("Create Table: " & Current_Table_Name &
                       " - Create Column: " & Current_Column_Name &
                       " " & Current_Column_Type &
                       " " & Current_Column_Constraint &
                       " " & Current_Table_Constraint);

            Msg.Std ("CREATE TABLE " & Current_Table_Name &
                           " (" & Current_Column_Name &
                           " " & Current_Column_Type &
                           " " & Current_Column_Constraint &
                           " " & Current_Table_Constraint & ")");

            DB.Execute_Query ("CREATE TABLE " & Current_Table_Name &
                           " (" & Current_Column_Name &
                           " " & Current_Column_Type &
                           " " & Current_Column_Constraint &
                           " " & Current_Table_Constraint & ")");
            --  Add item in data dictionnary
            Add_Sys_Schema;

            Current_Table_Not_Exists := False;
            Current_Table_Constraint := "";
            Current_Table_Comment := "";
            Clear_Column;
         else
            Create_Column;
         end if;
      end Create_Table;

    begin

      -- Empty Sys_Table for later filling from scratch
      --  if Table_Exists (DB, "Sys_Schema") then
      --     DB.Execute_Query ("DELETE FROM Sys_Schema;");
      --     DB.Execute_Query ("VACUUM;");
      --  end if;

      --Display Schema list for tests
      --  for I of Schema loop
      --     Schema_Command_List.Put (I.Command);
      --     Tio.Put_Line (" - " & I.Name & " - " & I.Attribute);
      --  end loop;

      for I of Schema loop

         --Schema_Command_List.Put (I.Command);
         --Tio.Put_Line (" - " & I.Name & " - " & I.Attribute);

         if    I.Command = Null_Command then
            -- No processing
            null;
         elsif I.Command = Database_Name then
            DBT := Sql.Properties (I.Name);
            if DBT.Brand /= None then -- Ie DB name not found
               Msg.Std ("Database " & I.Name & DBT.Name & DBT.URI & " needs creation or update");
               --  DB_Brand := DBT.Brand;
               --  DB_Status := DBT.Status;
               --  DB_URI := DBT.URI;
               --  DB_Name := DBT.Name;
               --  DB_Host:= DBT.Host;
               --  DB_Port := DBT.Port;
               --  DB_User := DBT.User;
               --  DB_Password := DBT.Password;
               --  DB_File := DBT.File;
               DB_Version := DBT.Version;
            else
               Msg.Err ("Sql.Schema_Update > Database not found: " & I.Name);
            end if;

         elsif I.Command = Database_Pragma then
            -- No processing
            null;

         elsif I.Command = Table_Name then
            -- Wait first column reading if table has to be created
            if Columns_Counter = 1 then
               Create_Table;
            end if;
            -- Process eventually a remaining column
            Create_Column;
            -- Last table command must have been read to eventually create the last index
            Create_Index;

            Current_Table_Name := I.Name;
            Current_Table_Comment := I.Comment;
            Msg.Dbg ("Load Table_Name: " & Current_Table_Name & " " & Current_Table_Comment);

            Current_Table_Not_Exists := not Table_Exists (DB, I.Name);
            Msg.Dbg ("Table_Name exists: " & To_String (not Current_Table_Not_Exists));

            Columns_Counter := 0;

         elsif I.Command = Table_Constraint then

            Current_Table_Constraint := I.Attribute;
            Msg.Dbg ("Load Table_Constraint: " & Current_Table_Constraint);

         elsif I.Command = Column_Name then
            -- Wait first column reading if table has to be created
            if Columns_Counter = 1 then
               Create_Table;
            else
               Create_Column;
            end if;

            Current_Column_Name := I.Name;
            Current_Column_Type := I.Attribute;
            Current_Column_Comment := I.Comment;
            Msg.Dbg ("Load Column_Name: " & Current_Column_Name & " " & Current_Column_Type & " " & Current_Column_Comment) ;

            Columns_Counter := Columns_Counter + 1;

         elsif I.Command = Column_Constraint then

            Current_Column_Constraint := I.Attribute;
            Msg.Dbg ("Load Column_Constraint: " & Current_Column_Constraint);

            Constraint_Counter := Constraint_Counter + 1;

         elsif I.Command = Index_Name then
            -- Previous table command could be a column creation
            if Columns_Counter >= 2 then
               Create_Column;
            end if;
            -- Previous table command could be an index creation
            Create_Index;

            Current_Index_Name := I.Name;
            Current_Index_Key := I.Attribute;
            Msg.Dbg ("Load Index_Name: " & Current_Index_Name & " " & Current_Index_Key);

            Index_Counter := Index_Counter + 1;

         elsif I.Command = Index_Constraint then

            Current_Index_Constraint := I.Attribute;
            Msg.Dbg ("Load Index_Constraint: " & Current_Index_Constraint);

         end if;

      end loop;

      --  Deal with remaining work
      Create_Table;
      if Columns_Counter >= 2 then
         Create_Column;
      end if;
      Create_Index;

      -- Clear Schema container for the next possible database processing
      Schema.Clear;

      -- Update database schema version
      Set_Config (DB, "Schema_Version", DB_Version);

   end Schema_Update;

   ----------------------------------------------------------------------------
   function Search (DB : in out GSD.Connection'Class;
                    Table_Name : String;
                    Condition : String) return Boolean is
      Result : Boolean := False;
   begin
      if Table_Exists (DB, Table_Name) then
         declare
            RS : GSD.Recordset'Class := DB.Query ("SELECT * FROM " & Table_Name &
                                     " WHERE " & Condition & " LIMIT 1");
         begin
            Result := RS.Next;
            RS.Close;
         end;
      else
         Msg.Err ("Sql.Row_Count > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
      return Result;
   end Search;

   ----------------------------------------------------------------------------
   procedure Set_Config (DB : in out GSD.Connection'Class;
                         Parameter : String;
                         Value : String) is
   begin
      if not Sql.Table_Exists (DB, Table_Sys_Config) then
         DB.Execute_Query ("CREATE TABLE " & Table_Sys_Config &
         " (Parameter text DEFAULT NULL, Value text DEFAULT NULL, KEY Idx_Config_Parameter (Parameter (50)))");
      end if;
      if Sql.Column_Exists (DB, Table_Sys_Config, "Parameter") and Sql.Column_Exists (DB, Table_Sys_Config, "Value") then
         DB.Execute_Query ("REPLACE INTO " & Table_Sys_Config & " (Parameter, Value) VALUES ('" & Parameter & "', '" & Value & "')");
      end if;
   end Set_Config;

   ----------------------------------------------------------------------------
   function Table_Exists (DB : in out GSD.Connection'Class;
                          Table_Name : String) return Boolean is
      Tables : Gnoga.Types.Data_Array_Type;
      Result : Boolean := False;
   begin
      Tables := DB.List_Of_Tables;
      for I in 1 .. Natural (Tables.Length) loop
         if Tables.Element (I) = Table_Name then
            Result := True;
            exit;
         end if;
      end loop;
      return Result;
   end Table_Exists;

   ----------------------------------------------------------------------------
   function To_Money (DB_Integer : Integer) return Money is
   begin
      return Money (DB_Integer) / 100.00;
   end To_Money;

   ----------------------------------------------------------------------------
   procedure Update (DB : in out GSD.Connection'Class;
                     Table_Name : String;
                     Columns_Values : String;
                     Where_Condition : String) is
      Description_List : GSD.Field_Description_Array_Type;
      Description : GSD.Field_Description;
      Current_Column, Current_Value, Update_Columns_Values, Current_Type,
      Sql_Request : String := "";
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);
   begin
      if Table_Exists (DB, Table_Name) then
         Description_List := DB.Field_Descriptions (Table_Name);
         -- Msg.Dbg ("Counter_Columns: " & To_String (Natural (Description_List.Last_Index)));
         -- Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            --  Iterate through each column
            for I in Description_List.First_Index .. Description_List.Last_Index loop
               Description := Description_List.Element (I);
               Current_Column := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND);
               --  Msg.Std (I);
               --  Msg.Std (Description_List.Last_Index);
               --  Msg.Dbg ("Current_Column_Insert: " & Current_Column);
               --  Msg.Dbg ("Current_Column_Table: " & To_Upper (Description.Column_Name));
               -- If field name and column name match
               if To_Upper (Current_Column) = To_Upper (Description.Column_Name) then
                  -- Fill Name and Value, according to field type
                  Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
                  Current_Type := To_Upper (Slice (Description.Data_Type, 1, 3));
                  Update_Columns_Values := Update_Columns_Values & Current_Column & " = ";
                  --  Msg.Dbg  ("Field Type: " & Current_Type);
                  --  Apply, depending of type
                  if Current_Type = "INT" then -- MySQL INT but SQLite INTEGER
                     Update_Columns_Values := Update_Columns_Values & Current_Value & ",";
                  elsif Current_Type = "TEX" then
                     -- Single quotes outside string and, inside string, escape single quote with pair of single quotes
                     Update_Columns_Values := Update_Columns_Values & "'" & Replace_Pattern (Current_Value, "'", "''") & "',";
                  elsif Current_Type = "BLO" then
                     -- Single quotes outside string and, inside string, escape single quote with pair of single quotes
                     Update_Columns_Values := Update_Columns_Values & "'" & Replace_Pattern (Current_Value, "'", "''") & "',";
                  else
                     Msg.Err ("Sql.Insert > Field: " & Current_Column & " does not handle Type: " & Current_Type);
                  end if;
                  exit; -- No need to iterate further after match
               else
                  if I = Description_List.Last_Index then
                     Msg.Err ("Sql.Insert > Field: " & Current_Column & " does not exists in Table: " & Table_Name);
                  end if;
               end if;
            end loop;
         end loop;
         -- Msg.Dbg ("Update_Columns_Values: " & Update_Columns_Values);
         -- If at least one Field/Value pair has been processed
         if (Index (Update_Columns_Values, ",") > 0) then
            -- Trailing comma deletion
            Update_Columns_Values := Slice (Update_Columns_Values, 1, Length (Update_Columns_Values) - 1);
            Sql_Request := "UPDATE " & Table_Name & " SET " & Update_Columns_Values & " WHERE " & Where_Condition & ";";
            DB.Execute_Query (Sql_Request);
         end if;
      else
         Msg.Err ("Sql.Insert > Table does not exists: " & Table_Name);
         --raise Table_Dont_Exists with To_Latin_1 (Table_Name);
      end if;
   end Update;

--     --
--     function Error (Information : String ; Information_Extended : out String) return Integer is
--        Exception_Information : String := Information;
--        Exception_Information_Extended : String := "Extended information unavailable, see error code number";
--        Exception_Information_Number : Integer := 0;
--     begin
--
--        if not Empty (Exception_Information) then
--
--           Exception_Information := Field_By_Index (Exception_Information, 1, CR);
--           Exception_Information := Field_By_Index (Exception_Information, 2, "[");
--           Exception_Information := Stript_Chars (Exception_Information, " ]" & CRLF);
--
--           -- If Error_Code here
--           if Is_Numeric (Exception_Information) then
--              Exception_Information_Number := To_Integer (Exception_Information);
--              Exception_Information_Extended := Error_Display (Exception_Information_Number);
--              Information_Extended := Exception_Information_Extended;
--           end if;
--
--        else
--           --  Send raw information
--           Information_Extended := "No Error_Code present, send remaining raw information: " & Exception_Information;
--        end if;
--
--        return Exception_Information_Number;
--     end Error;
--
--      --
--     procedure Error (Exception_Hook : AE.Exception_Occurrence) is
--        Exception_Information : String := From_Latin_1 (AE.Exception_Information (Exception_Hook));
--        Exception_Information_Extended : String := "Extended information unavailable, see error code number";
--        Exception_Information_Number : Integer := 0;
--     begin
--
--        if not Empty (Exception_Information) then
--
--           Exception_Information := Field_By_Index (Exception_Information, 1, CR);
--           Exception_Information := Field_By_Index (Exception_Information, 2, "[");
--           Exception_Information := Stript_Chars (Exception_Information, " ]" & CRLF);
--
--           -- If Error_Code here
--           if Is_Numeric (Exception_Information) then
--              Exception_Information_Number := To_Integer (Exception_Information);
--              Exception_Information_Extended := Error_Display (Exception_Information_Number);
--           end if;
--
--        else
--           Exception_Information_Number := Status_No_Code;
--        end if;
--
--        Log.Err ("Genesix DB exception: " & Error_Display (Exception_Information_Number));
--
--     end Error;
--
--     function Error (Exception_Hook : AE.Exception_Occurrence) return Integer is
--        Exception_Information : String := From_Latin_1 (AE.Exception_Information (Exception_Hook));
--        Exception_Information_Number : Integer := 0;
--     begin
--
--        if not Empty (Exception_Information) then
--
--           Exception_Information := Field_By_Index (Exception_Information, 1, CR);
--           Exception_Information := Field_By_Index (Exception_Information, 2, "[");
--           Exception_Information := Stript_Chars (Exception_Information, " ]" & CRLF);
--
--           -- If Error_Code here
--           if Is_Numeric (Exception_Information) then
--              Exception_Information_Number := To_Integer (Exception_Information);
--           end if;
--        end if;
--
--        return Exception_Information_Number;
--     end Error;
--
--     --
--     function Error_Display (Error_Code : Integer) return String is
--        Error_String : String;
--     begin
--
--        if Error_Code = Status_Need_Update then
--           Error_String := "Database need update";
--        elsif Error_Code = Status_No_Code then
--           Error_String := "No Error_Code present";
--        elsif Error_Code = Info_Ok then
--           Error_String := "Operation successful";
--        elsif Error_Code = Info_Row then
--           Error_String := "Another row of output is available";
--        elsif Error_Code = Info_Done then
--           Error_String := "Operation has completed";
--        elsif Error_Code = Error_Generic then
--           Error_String := "Generic error code when no other error code can be used";
--        elsif Error_Code = Error_Internal then
--           Error_String := "Internal error";
--        elsif Error_Code = Error_Perm then
--           Error_String := "Permission error accessing a newly created database";
--        elsif Error_Code = Error_Abort then
--           Error_String := "Operation was aborted";
--        elsif Error_Code = Error_Busy then
--           Error_String := "Database is busy";
--        elsif Error_Code = Error_Locked then
--           Error_String := "Database is locked";
--        elsif Error_Code = Error_No_Mem then
--           Error_String := "No memory available";
--        elsif Error_Code = Error_Readonly then
--           Error_String := "Database is in read only mode";
--        elsif Error_Code = Error_Interrupt then
--           Error_String := "An operation was interrupted";
--        elsif Error_Code = Error_In_Out then
--           Error_String := "Read or write disk error";
--        elsif Error_Code = Error_Corrupt then
--           Error_String := "The database is corrupted";
--        elsif Error_Code = Error_Not_Found then
--           Error_String := "Multiple contexts error, see SQLite documentation";
--        elsif Error_Code = Error_Full then
--           Error_String := "The disk is full";
--        elsif Error_Code = Error_Cant_Open then
--           Error_String := "Can't open database or working file";
--        elsif Error_Code = Error_Protocol then
--           Error_String := "Locking protocol problem";
--        elsif Error_Code = Error_Schema then
--           Error_String := "Schema has changed during operation";
--        elsif Error_Code = Error_Too_Big then
--           Error_String := "String or blob too large";
--        elsif Error_Code = Error_Constraint then
--           Error_String := "A SQL constraint violation occurred";
--        elsif Error_Code = Error_Mismatch then
--           Error_String := "A type mismatch occurred";
--        elsif Error_Code = Error_Misuse then
--           Error_String := "Incorrect use of SQLite interface";
--        elsif Error_Code = Error_No_Lfs then
--           Error_String := "No Large File Support, database can't grow";
--        elsif Error_Code = Error_Authent then
--           Error_String := "Not authorized SQL prepared";
--        elsif Error_Code = Error_Range then
--           Error_String := "A parameter number argument in Bind or Column routines is out of range";
--        elsif Error_Code = Error_Not_A_DB then
--           Error_String := "Not a SQLite database";
--        else
--           Error_String := "Error_Code unknown: " & Trim_Left (To_String (Error_Code));
--        end if;
--
--        return Error_String;
--
--     end Error_Display;

------------------------------------------------------------------------------
end v22.Sql;
------------------------------------------------------------------------------
