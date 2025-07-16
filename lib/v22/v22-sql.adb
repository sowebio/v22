-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-sql.adb
--  @copyright See authors list below and README.md file
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

with GNAT.SHA512;

with UXStrings.Conversions; use UXStrings.Conversions;

with v22.Fls;
with v22.Msg;
with v22.Tio;

package body v22.Sql is

   -----------------------------------------------------------------------------
   --  API
   -----------------------------------------------------------------------------

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
         Msg.Info ("Closing " & From_Latin_1 (Databases(C).Brand'Image) & " database: " & Databases(C).Name);
      end loop;
   end Close;

   ----------------------------------------------------------------------------
   function Column_Exists (DB : in out GSD.Connection'Class; Table_Name : String; Column_Name : String) return Boolean is
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
          Msg.Error ("Sql.Column_Exists > Table does not exists: " & Table_Name);
      end if;
      return Result;
   end Column_Exists;

   ----------------------------------------------------------------------------
   procedure Delete (Database_Name : String; Table_Name : String; Where_Condition : String := "") is
      DBT : Sql.Database_Line_Type;
   begin
      DBT := Sql.Properties (Database_Name);
      if DBT.Brand = Sql.MySQL then
         Sql.Delete (DBT.DBM, Table_Name, Where_Condition);
      elsif DBT.Brand = Sql.SQLite then
         Sql.Delete (DBT.DBS, Table_Name, Where_Condition);
      else
         Msg.Error ("Sql.Delete > Properties not found for " & Database_Name);
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   procedure Delete (DB : in out GSD.Connection'Class; Table_Name : String; Where_Condition : String) is
      Query : String;
   begin
      Sql_Mutex.Lock;
      if Table_Exists (DB, Table_Name) then
         Query := "DELETE FROM " & Table_Name & " WHERE " & Where_Condition;
         Msg.Debug ("Delete_From: " & Query);
         DB.Execute_Query (Query);
      else
          Msg.Error ("Sql.Delete > Table does not exists: " & Table_Name);
      end if;
      Sql_Mutex.Unlock;

   exception
      when E : others =>
         Msg.Error_Latin_1 ("Sql.Delete > Error: " & Ada.Exceptions.Exception_Information(E));
         Sql_Mutex.Unlock;
   end Delete;

   ----------------------------------------------------------------------------
   function Escape_String (String_To_Process : String) return String is
      Current_Character : String;
      Result : String := "";
   begin
      for I in 1 .. String_To_Process.Length loop
         Current_Character := Slice (String_To_Process, I, I);
         if Current_Character = From_ASCII (ASCII.NUL) then    --  00d
            Result := Result & BK & "0";
         elsif Current_Character = From_ASCII (ASCII.BS) then  --  08d Backspace
            Result := Result & BK & "b";
         elsif Current_Character = From_ASCII (ASCII.HT) then  --  09d Tabulation
            Result := Result & BK & "t";
         elsif Current_Character = From_ASCII (ASCII.LF) then  --  10d Line feed
            Result := Result & BK & "n";
         elsif Current_Character = From_ASCII (ASCII.CR) then  --  13d Carriage return
            Result := Result & BK & "r";
         elsif Current_Character = DQ then                     --  22d Double Quote
            Result := Result & BK & DQ;
         elsif Current_Character = From_ASCII (ASCII.SUB) then --  26d ASCII 26 (Control+Z)
            Result := Result & BK & "Z";
         elsif Current_Character = SQ then                     --  27d Single quote
            Result := Result & SQ & SQ;                        --  '' instead of \' to comply with SQLite. MySQL accepts both.
         elsif Current_Character = "%" then                    --  37d
            Result := Result & BK & "%";
         elsif Current_Character = BK then                     --  92d Backslash
            Result := Result & BK & BK;
         --  elsif Current_Character = "_" then                    --  95d
         --     Result := Result & BK & "_";
         else
            Result := Result & String_To_Process(I);
         end if;
      end loop;
      return Result;
   end Escape_String;

   ----------------------------------------------------------------------------
   function Get_Config (DB : in out GSD.Connection'Class; Parameter : String) return String is
      RS : GSD.Recordset'Class := DB.Query ("SELECT Value FROM " & Table_Sys_Config & " WHERE Parameter='" & Parameter & "'");
      Result : String := "";
   begin
      if Database_With_Sys_Tables then
         if Sql.Table_Exists (DB, Table_Sys_Config) then
            if Sql.Column_Exists (DB, Table_Sys_Config, "Parameter") and
              Sql.Column_Exists (DB, Table_Sys_Config, "Value") then
               while RS.Next loop
                  Result := RS.Field_Value (1);
               end loop;
            end if;
         end if;
         RS.Close;
      end if;
      return Result;
   exception
      when Error : GSD.Query_Error =>
         -- To handle first launch when database not created yet
         RS.Close;
         return Result;
   end Get_Config;

   ----------------------------------------------------------------------------
   function Get_Config (Parameter : String) return String is
      DBT : Sql.Database_Line_Type;
      Result : String := "";
   begin
      if Database_With_Sys_Tables then
         DBT := Sql.Properties (Sql.Get_Database_Main);
         if DBT.Brand = Sql.MySQL then
            Result := Sql.Get_Config (DBT.DBM, Parameter);
         elsif DBT.Brand = Sql.SQLite then
            Result := Sql.Get_Config (DBT.DBS, Parameter);
         else
            Msg.Error ("Sql.Get_Config > Properties not found for " & Sql.Get_Database_Main);
         end if;
      end if;
      return Result;
   end Get_Config;

   ----------------------------------------------------------------------------
   function Get_Database_Brand (DB : in out GSD.Connection'Class) return Database_Brand is
      --  {GNOGA.SERVER.DATABASE.MYSQL.CONNECTION object}
      --  {GNOGA.SERVER.DATABASE.SQLITE.CONNECTION object}
      DB_Brand : String := Field_By_Index (From_Latin_1 (Ada.Tags.External_Tag (DB'Tag)), 4, ".");
      Result : Database_Brand := Unknown;
   begin
      if DB_Brand = "MYSQL" then
         Result := MySQL;
      elsif DB_Brand = "SQLITE" then
         Result := SQLite;
      end if;
      return Result;
   end Get_Database_Brand;

   ----------------------------------------------------------------------------
   function Get_Database_Main return String is
   begin
      return Database_Main;
   end Get_Database_Main;

   ----------------------------------------------------------------------------
   function Get_Version (DB : in out GSD.Connection'Class) return String is
      DB_Type : Database_Brand := Get_Database_Brand (DB);
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
   function Index_Exists (DB : in out GSD.Connection'Class; Table_Name : String; Index_Name : String) return Boolean is
      DB_Type : Database_Brand := Get_Database_Brand (DB);
      Input : String;
      Result : Boolean;
   begin
      if Table_Exists (DB, Table_Name) then
         if DB_Type = MySQL then
            Input := "SHOW INDEX FROM " & Table_Name & " WHERE Key_name = '" & Index_Name & "'";
            Msg.Debug (Input);
            Result := (DB.Execute_Update (Input) >= 1); -- Must be >= 1 to handle composite index
         elsif DB_Type = SQLite then
            -- When SQLite API fixed, replace the following lines by:
            -- Result := (DB.Execute_Update ("SELECT * FROM sqlite_master WHERE type = 'index'" &
            -- " and tbl_name = '" & Table_Name  & "' and name = '" & Index_Name & "'") >= 1);
            declare
               RS : GSD.Recordset'Class := DB.Query ("SELECT * FROM sqlite_master WHERE type = 'index'" &
                                   " and tbl_name = '" & Table_Name  & "' and name = '" & Index_Name & "'");
            begin
               Result := RS.Next;
               RS.Close;
            end;
         end if;
      else
          Msg.Error ("Sql.Index_Exists > Table does not exists: " & Table_Name);
      end if;
      return Result;
   end Index_Exists;

   ----------------------------------------------------------------------------
   procedure Insert (Database_Name : String; Table_Name : String; Columns_Values : String) is
      DBT : Sql.Database_Line_Type;
   begin
      DBT := Sql.Properties (Database_Name);
      if DBT.Brand = Sql.MySQL then
         Sql.Insert (DBT.DBM, Table_Name, Columns_Values);
      elsif DBT.Brand = Sql.SQLite then
         Sql.Insert (DBT.DBS, Table_Name, Columns_Values);
      else
         Msg.Error ("Sql.Insert > Properties not found for " & Database_Name);
      end if;
   end Insert;

   ----------------------------------------------------------------------------
   procedure Insert (DB : in out GSD.Connection'Class; Table_Name : String; Columns_Values : String) is
      Description_List : GSD.Field_Description_Array_Type;
      Description : GSD.Field_Description;
      Current_Column, Current_Value, Current_Type, Insert_Columns_Names,
      Insert_Columns_Values, Sql_Request : String := "";
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);
   begin
      Sql_Mutex.Lock;
      if Table_Exists (DB, Table_Name) then
         Description_List := DB.Field_Descriptions (Table_Name);
         --  Msg.Debug ("Counter_Columns: " & To_String (Natural (Description_List.Last_Index)));

         --  Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            --  Iterate through each column
            for I in Description_List.First_Index .. Description_List.Last_Index loop
               Description := Description_List.Element (I);
               Current_Column := Trim_Both (Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND));
               --  Msg.Debug (I);
               --  Msg.Debug (Description_List.Last_Index);
               --  Msg.Debug ("Current_Column (PRG name): " & To_Upper (Current_Column));
               --  Msg.Debug ("Current_Column (DB name): " & To_Upper (Description.Column_Name));

               --  If field name and column name match
               if To_Upper (Current_Column) = To_Upper (Description.Column_Name) then
                  --  Fill Name and Value, according to field type
                  Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
                  Current_Type := To_Upper (Slice (Description.Data_Type, 1, 3));
                  Insert_Columns_Names := Insert_Columns_Names & Current_Column & ",";
                  --  Msg.Debug ("Current_Value: " & Current_Value);
                  --  Msg.Debug ("Current_Type: " & Current_Type);
                  --  Msg.Debug ("Insert_Columns_Names: " & Insert_Columns_Names);

                  --  Apply, depending of the column type:
                  --  BLOB, TEXT, VARCHAR: Single quotes outside string and escaping characters inside string
                  --  BIGINT, DECIMAL, DOUBLE, FLOAT, INTEGER: Insert 0 if empty
                  if Current_Type = "BIG" then --  BIGINT
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "BLO" then --  BLOB
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "DEC" then --  DECIMAL
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "DOU" then --  DOUBLE
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "FLO" then --  FLOAT
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "INT" then --  MySQL INT but SQLite INTEGER
                     Insert_Columns_Values := Insert_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "TEX" then --  TEXT
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "VAR" then --  VARCHAR
                     Current_Value := Escape_String (Current_Value);
                     --  Truncate string adjusting it to maximum column length
                     if Current_Value.Length > GSD.Field_Size (Description) then
                        Current_Value := Slice (Current_Value, 1, GSD.Field_Size (Description));
                        Msg.Error ("Sql.Insert > String too long to fit in VARCHAR(" &
                                   To_String_Unsigned (GSD.Field_Size (Description)) &
                                   "). Truncate it: " & Slice (Current_Value, 1, GSD.Field_Size (Description)));
                     end if;
                     Insert_Columns_Values := Insert_Columns_Values & "'" & Current_Value & "',";
                  else
                      Msg.Error ("Sql.Insert > Field: " & Current_Column & " does not handle Type: " & Current_Type);
                  end if;
                  exit; --  No need to iterate further after match
               else
                  if I = Description_List.Last_Index and not Is_Empty (Current_Column) then
                      Msg.Error ("Sql.Insert > Field: '" & Current_Column & "' does not exists in Table: " & Table_Name);
                  end if;
               end if;
            end loop;
         end loop;
         --  If at least one Field/Value pair has been processed
         if (Index (Insert_Columns_Names, ",") > 0) and
            (Index (Insert_Columns_Values, ",") > 0) then
            --  Trailing comma deletion
            Insert_Columns_Names := Slice (Insert_Columns_Names, 1, Length (Insert_Columns_Names) - 1);
            Insert_Columns_Values := Slice (Insert_Columns_Values, 1, Length (Insert_Columns_Values) - 1);
            --  Msg.Debug ("Sql.Insert > Insert_Columns_Names: " & Insert_Columns_Names);
            --  Msg.Debug ("Sql.Insert > Insert_Columns_Values: " & Insert_Columns_Values);
            Sql_Request := "INSERT INTO " & Table_Name & " (" & Insert_Columns_Names & ") VALUES (" & Insert_Columns_Values & ");";
            Msg.Debug ("Sql.Insert > Insert_Into: " & Sql_Request);
            DB.Execute_Query (Sql_Request);
         end if;
      else
          Msg.Error ("Sql.Insert > Table does not exists: " & Table_Name);
      end if;
      Sql_Mutex.Unlock;

   exception
      when E : others =>
         Msg.Error_Latin_1 ("Sql.Insert > Error: " & Ada.Exceptions.Exception_Information(E));
         Sql_Mutex.Unlock;
   end Insert;

   ----------------------------------------------------------------------------
   function Last_RowID (DB : in out GSD.Connection'Class;
                        Table_Name : String) return Integer is
      DB_Type : Database_Brand := Get_Database_Brand (DB);
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
          Msg.Error ("Sql.Last_RowID > Table does not exists: " & Table_Name);
      end if;
      return Result;
   end Last_RowID;

   ----------------------------------------------------------------------------
   function DB_Index (DB_Name : String) return Databases_List.Extended_Index is
      use Databases_List; -- for operators
      DB_Index : Databases_List.Extended_Index := Databases_List.No_Index;
   begin
      for C in Databases.Iterate loop
         --  Msg.Debug ("Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (To_Index (C))));
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
                  Version : String := "";
                  Rank : Database_Rank := Main;
                  Sys_Tables : Database_Sys_Tables := Auto_Create) return Database_Status is
      DB_Brand : Database_Brand := MySQL;
      DB_Status : Database_Status := None;
      DB_URI : String := URI;
      DB_Name, DB_Host, DB_User, DB_Password, DB_File, DB_Temp : String;
      DB_Port : Natural := 0;
      DB_URI_Parameter, DB_URI_Parameter_Name : String := "";
   begin
      --  MySQL: db:db_name?host=host_name&port=port_number&user=user_name&password=user_password
      --  This parameters string conforms to RFC 3986
      DB_Name := Field_By_Index (Field_By_Index (DB_URI, 2, ":"), 1, "?");
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

            --  IL SERAIT BON DE TESTER ICI LA RÉSOLUTION DU NOM DE HOST mais
            --  ping i188c1 (non résolvable dans ro6.genesix.org) est traduit par ce dernier i188c1.genesix.org
            --  alors que genesix.org est la racine du nom de host ro6.genesix.org... Est-ce bien normal ?
            -- Règles ? Soit le nom est une IP et on pingue soit c'est un nom et on nslookup le nom
            --
            --  PING OK : ping -c 1 172.31.0.1
            --    PING 172.31.0.1 (172.31.0.1) 56(84) bytes of data.
            --    64 bytes from 172.31.0.1: icmp_seq=1 ttl=64 time=21.3 ms
            --
            --  --- 172.31.0.1 ping statistics ---
            --    1 packets transmitted, 1 received, 0% packet loss, time 0ms
            --      rtt min/avg/max/mdev = 21.260/21.260/21.260/0.000 ms
            --
            --  PING KO : ping -c 1 172.31.0.34
            --  PING 172.31.0.34 (172.31.0.34) 56(84) bytes of data.
            --
            --  --- 172.31.0.34 ping statistics ---
            --  1 packets transmitted, 0 received, 100% packet loss, time 0ms
            --
            --  NSLOOKUP KO  : sr@ro6  ~/Sowebio/informatique/dev/prv/hex01_cfg/prg >nslookup i188c1
            --  Server:<tab>192.168.0.254
            --  Address:<tab>192.168.0.254#53
            --
            --  ** server can't find i188c1: NXDOMAIN
            --
            --  NSLOOKUP OK  : sr@ro6  (1) ~/Sowebio/informatique/dev/prv/hex01_cfg/prg >nslookup i188c1.genesix.org
            --  Server:<tab>192.168.0.254
            --  Address:<tab>192.168.0.254#53
            --
            --  Non-authoritative answer:
            --  Name:<tab>i188c1.genesix.org
            --  Address: 57.128.10.255

            --  Msg.Debug ("DB_Host: " & DB_Host);
            --  Msg.Debug ("DB_Port: " & To_String_Unsigned (DB_Port));
            --  Msg.Debug ("DB_Name: " & DB_Name);
            --  Msg.Debug ("DB_User: " & DB_User);
            --  Msg.Debug ("DB_Password: " & DB_Password);
            DBM.Connect (Database => DB_Name,
                        Host => DB_Host,
                        Port => DB_Port,
                        User => DB_User,
                        Password => DB_Password);
            DB_Status := Open_Success;
         exception
            when Error : GSD.Connection_Error =>
               DB_Status := Open_Failed;
               Msg.Error ("v22.Sql.Open > MySQL database " & DB_Name & " failed" &
                           (if Is_Empty (DBM.Error_Message) then From_ASCII ("") else From_ASCII (" > ") & DBM.Error_Message));
               Msg.Error ("DB_Host: " & DB_Host);
               Msg.Error ("DB_Port: " & To_String_Unsigned (DB_Port));
               Msg.Error ("DB_Name: " & DB_Name);
               Msg.Error ("DB_User: " & DB_User);
               Msg.Error ("DB_Password: " & DB_Password);
         end;
      end if;
      if (DB_Status = Open_Success) then
         Open_Load (DBM, MySQL, DB_Status, DB_URI, DB_Name, DB_Host, DB_Port, DB_User, DB_Password, DB_File, Version, Rank, Sys_Tables);
      end if;
      return DB_Status;
   end Open;

   ----------------------------------------------------------------------------
   function Open (DBS : in out GSD.SQLite.Connection;
                  URI : String := "";
                  Version : String := "";
                  Rank : Database_Rank := Main;
                  Sys_Tables : Database_Sys_Tables := Auto_Create) return Database_Status is
      DB_Brand : Database_Brand := SQLite;
      DB_Status : Database_Status := None;
      DB_URI : String := URI;
      DB_Name, DB_Host, DB_User, DB_Password, DB_File, DB_Temp : String;
      DB_Port : Natural := 0;
   begin
      -- SQLite: file:data.db or file:data.db?mode=ro&cache=private
      -- SQLite has been set in RFC 3986 URI mode (see sqlite.c)
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
             Msg.Error ("v22.Sql.Open > SQlite Database " & DB_Name & " failed" &
                     (if Is_Empty (DBS.Error_Message) then From_ASCII(" ") else From_ASCII(" > ") & DBS.Error_Message));
      end;
      if (DB_Status = Open_Success) then
         Open_Load (DBS, SQLite, DB_Status, DB_URI, DB_Name, DB_Host, DB_Port, DB_User, DB_Password, DB_File, Version, Rank, Sys_Tables);
      end if;
      return DB_Status;
   end Open;

   ----------------------------------------------------------------------------
   task body Ping is
      --  Delay_Value : Duration := 3600.0;  -- Wait 1 hour between pings
      Delay_Value : Duration := 900.0;  -- Wait 15 minutes between pings
      function Image is new UXStrings.Conversions.Fixed_Point_Image (Duration);
   begin
      accept Start;
      Msg.Debug ("Ping on databases armed for " & Trim_Left (Field_By_Index (Image (Delay_Value), 1, ".")) & "s cycles");
      loop
         delay Delay_Value;
         Sql.Ping_Send;
      end loop;
   end Ping;

   ----------------------------------------------------------------------------
   function Properties (DB_Name : String) return Database_Line is
      use Databases_List; -- for operators
      DB_Index : Databases_List.Extended_Index := Databases_List.No_Index;
      DB_Record : Database_Line;
   begin
      for C in Databases.Iterate loop
         --  Msg.Debug ("Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (To_Index (C))));
         if  Databases(C).Name = DB_Name then
              DB_Record := Databases(C);
            exit;
         end if;
      end loop;
   return DB_Record;
   end Properties;

   ----------------------------------------------------------------------------
   function Read (Database_Name : String; Table_Name : String; Columns : String; Condition : String := "") return String is
      DBT : Sql.Database_Line_Type;
      Sql_Result : String := "";
   begin
      DBT := Sql.Properties (Database_Name);
      if DBT.Brand = Sql.MySQL then
         Sql_Result := Sql.Read (DBT.DBM, Table_Name, Columns, Condition);
      elsif DBT.Brand = Sql.SQLite then
         Sql_Result := Sql.Read (DBT.DBS, Table_Name, Columns, Condition);
      else
         Msg.Error ("Sql.Read > Properties not found for " & Database_Name);
      end if;
      return Sql_Result;
   end Read;

   ----------------------------------------------------------------------------
   function Read (DB : in out GSD.Connection'Class; Table_Name : String;
                  Columns : String; Condition : String := "") return String is
      Counter_Columns : constant Natural:= Field_Count (Columns, ",");
      Sql_Condition : String := Condition;
      Sql_Query, Sql_Result : String := "";
   begin
      if Table_Exists (DB, Table_Name) then
         if not Is_Empty (Sql_Condition) then
            --  No ORDER BY nor WHERE, add WHERE by default
            if Index (To_Upper (Sql_Condition), "ORDER") = 0 then
               if Index (To_Upper (Sql_Condition), "WHERE") = 0 then
                  Sql_Condition := "WHERE " & Sql_Condition;
               end if;
            end if;
         end if;
         Sql_Query := "SELECT " & Columns & " FROM " & Table_Name & " " & Sql_Condition;
         Msg.Debug ("Sql.Read > Query: " & Sql_Query);
         declare
            RS : GSD.Recordset'Class := DB.Query (Sql_Query);
         begin
            --  Iterate result(s) line(s)
            while RS.Next loop
               --  Iterate choosen columns
               for Index in 1..RS.Number_Of_Fields loop
                  --  Msg.Debug ("Current_Column_Text: " & Column_Text (Local_Statement, Index));
                  Sql_Result := Sql_Result & RS.Field_Value (Index) & CD;
               end loop;
               --  Delete last CD and add row delimiter
               if (Index (Sql_Result, CD) > 0) then
                  Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1) & RD;
               end if;
            end loop;
            RS.Close;
         end;
         --  Delete last RD
         if Length (Sql_Result) >= 2 then -- to handle one digit answer with a trailing RD = 2 chars
            if Slice (Sql_Result, Length (Sql_Result), Length (Sql_Result)) = RD then
               Sql_Result := Slice (Sql_Result, 1, Length (Sql_Result) - 1);
            end if;
         end if;
         Msg.Debug ("Sql.Read > Read result: " & Sql_Result);
      else
         Msg.Debug ("Sql.Read > Query: " & Sql_Query);
         Msg.Error ("Sql.Read > Table does not exists: " & Table_Name);
      end if;
      return Sql_Result;
   end Read;

   ----------------------------------------------------------------------------
   function Row_Count (DB : in out GSD.Connection'Class; Table_Name : String; Option : String := "*") return Integer is
      DB_Type : Database_Brand := Get_Database_Brand (DB);
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
          Msg.Error ("Sql.Row_Count > Table does not exists: " & Table_Name);
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

      ---------------------------------
      procedure Add_Sys_Schema is
         DB_Query : String := "INSERT INTO " & Table_Sys_Schema &
                              " (Table_Name, Column_Name, Column_Type, Column_Constraint, Version, Comment)" &
                              " VALUES ('" &
                              Current_Table_Name & "', '" &
                              Current_Column_Name & "', '" &
                              Current_Column_Type & "', '" &
                              Current_Column_Constraint & "', '" &
                              "+" & DB_Version  & "', '" &
                              Replace (Current_Column_Comment, "'", " ") & "')";
      begin
         --Msg.Debug ("Entry: " & DB_Query);
         if Table_Exists (DB, Table_Sys_Schema) then
            if Current_Table_Name /= Table_Sys_Schema then
               if Column_Exists (DB, Table_Sys_Schema, "Table_Name") and
                  Column_Exists (DB, Table_Sys_Schema, "Column_Name") and
                  Column_Exists (DB, Table_Sys_Schema, "Column_Type") and
                  Column_Exists (DB, Table_Sys_Schema, "Version") and
                  Column_Exists (DB, Table_Sys_Schema, "Comment") then
                  --Msg.Debug ("Execution: " & DB_Query);
                  DB.Execute_Query (DB_Query);
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
         --  Test if non empty column name to handle table break
         --  when previous table column already exists
         if not Is_Empty (Current_Column_Name) then
            if Column_Exists (DB, Current_Table_Name, Current_Column_Name) then
               Msg.Debug ("Existing Table: " & Current_Table_Name &
                          " - Existing Column: " & Current_Column_Name &
                          " " & Current_Column_Type &
                          " " & Current_Column_Constraint);
            else
               Msg.Info ("Table: " & Current_Table_Name &
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
         --  Test if non empty column name to handle table break
         --  when previous table index already exists
         if not Is_Empty (Current_Index_Name) then
            if Index_Exists (DB, Current_Table_Name, Current_Index_Name) then
               Msg.Debug ("Existing Table: " & Current_Table_Name &
                          " - Existing Index: " & Current_Index_Name &
                          " " & Current_Index_Key &
                          " " & Current_Index_Constraint);
            else
               Msg.Info ("Table: " & Current_Table_Name &
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
         DB_Query : String := "CREATE TABLE " & Current_Table_Name &
                             " (" & Current_Column_Name &
                             " " & Current_Column_Type &
                             " " & Current_Column_Constraint &
                             " " & Current_Table_Constraint & ")";
      begin
         if Current_Table_Not_Exists then
            Msg.Info ("Create Table: " & Current_Table_Name &
                       " - Create Column: " & Current_Column_Name &
                       " " & Current_Column_Type &
                       " " & Current_Column_Constraint &
                       " " & Current_Table_Constraint);

            Msg.Debug (DB_Query);
            DB.Execute_Query (DB_Query);
            Add_Sys_Schema; --  Add item in data dictionnary

            Current_Table_Not_Exists := False;
            Current_Table_Constraint := "";
            Current_Table_Comment := "";
            Clear_Column;
         else
            Create_Column;
         end if;
      end Create_Table;

    begin

      --  Empty Sys_Table for later filling from scratch
      --  if Table_Exists (DB, "Sys_Schema") then
      --     DB.Execute_Query ("DELETE FROM Sys_Schema;");
      --     DB.Execute_Query ("VACUUM;");
      --  end if;

      --  Display Schema list for tests
      --  for I of Schema loop
      --     Schema_Command_List.Put (I.Command);
      --     Tio.Put_Line (" - " & I.Name & " - " & I.Attribute);
      --  end loop;

      for I of Schema loop

         --  Schema_Command_List.Put (I.Command);
         --  Tio.Put_Line (" - " & I.Name & " - " & I.Attribute);

         if    I.Command = Null_Command then
            null; --  No processing

         elsif I.Command = Database_Name then
            DBT := Sql.Properties (I.Name);
            if DBT.Brand /= None then -- ie DB name not found
               Msg.Info ("Database " & I.Name & " needs creation or update");
               DB_Version := DBT.Version;
            else
                Msg.Error ("Sql.Schema_Update > Database not found: " & I.Name);
            end if;

         elsif I.Command = Database_Pragma then
            null; --  No processing

         elsif I.Command = Table_Name then
            --  Wait first column reading if table has to be created
            if Columns_Counter = 1 then
               Create_Table;
            end if;
            --  Process eventually a remaining column
            Create_Column;
            --  Last table command must have been read to eventually create the last index
            Create_Index;

            Current_Table_Name := I.Name;
            Current_Table_Comment := I.Comment;
            Msg.Debug ("Load Table_Name: " & Current_Table_Name & " " & Current_Table_Comment);

            Current_Table_Not_Exists := not Table_Exists (DB, I.Name);
            Msg.Debug ("Table_Name exists: " & To_String (not Current_Table_Not_Exists));

            Columns_Counter := 0;

         elsif I.Command = Table_Constraint then

            Current_Table_Constraint := I.Attribute;
            Msg.Debug ("Load Table_Constraint: " & Current_Table_Constraint);

         elsif I.Command = Column_Name then
            --  Wait first column reading if table has to be created
            if Columns_Counter = 1 then
               Create_Table;
            else
               Create_Column;
            end if;

            Current_Column_Name := I.Name;
            Current_Column_Type := I.Attribute;
            Current_Column_Comment := I.Comment;
            Msg.Debug ("Load Column_Name: " & Current_Column_Name & " " & Current_Column_Type & " " & Current_Column_Comment) ;

            Columns_Counter := Columns_Counter + 1;

         elsif I.Command = Column_Constraint then

            Current_Column_Constraint := I.Attribute;
            Msg.Debug ("Load Column_Constraint: " & Current_Column_Constraint);

            Constraint_Counter := Constraint_Counter + 1;

         elsif I.Command = Index_Name then
            --  Previous table command could be a column creation
            if Columns_Counter >= 2 then
               Create_Column;
            end if;
            --  Previous table command could be an index creation
            Create_Index;

            Current_Index_Name := I.Name;
            Current_Index_Key := I.Attribute;
            Msg.Debug ("Load Index_Name: " & Current_Index_Name & " " & Current_Index_Key);

            Index_Counter := Index_Counter + 1;

         elsif I.Command = Index_Constraint then

            Current_Index_Constraint := I.Attribute;
            Msg.Debug ("Load Index_Constraint: " & Current_Index_Constraint);

         end if;

      end loop;

      --  Deal with remaining work
      Create_Table;
      if Columns_Counter >= 2 then
         Create_Column;
      end if;
      Create_Index;

      --  Only main database has Sys_Users table
      if Table_Exists (DB, Table_Sys_Users) then
         --  If Sys_Users table has just been created, it still has no record,
         --  but Sys_Users must have, at least, one admin user registered
         if Sql.Row_Count (DB, "Sys_Users") = 0 then
            Msg.Info ("Initialize " & Sql.Get_Database_Main & " newly created");
            --  Initialize Sys_Users with a default user with administrator rights
            declare
               Query : constant String := "Login~admin" & CD &
                                          "First_Name~Number" & CD &
                                          "Last_Name~Six" & CD &
                                          "Password~" & From_Latin_1 (GNAT.SHA512.Digest ("password")) & CD &
                                          "Grants~" & GRANTS_ROLE_ADMINISTRATOR & SD & GRANTS_RIGHTS_FULL & CD &
                                          "Notes~Default administrator" & CD &
                                          "DTS_Creation~" & Prg.Date_Time_Stamp;
            begin
               Msg.Debug ("Query: " & Query);
               Sql.Insert (DB, "Sys_Users", Query);
            end;
         end if;
      end if;

      --  Clear Schema container for the next possible database processing
      Schema.Clear;

      --  Update database schema version
      Set_Config (DB, "Schema_Version", DB_Version);

   end Schema_Update;

   ----------------------------------------------------------------------------
   function Search (Database_Name : String; Table_Name : String; Condition : String) return Boolean is
      DBT : Sql.Database_Line_Type;
      Result : Boolean := False;
   begin
      DBT := Sql.Properties (Database_Name);
      if DBT.Brand = Sql.MySQL then
         Result := Sql.Search (DBT.DBM, Table_Name, Condition);
      elsif DBT.Brand = Sql.SQLite then
         Result := Sql.Search (DBT.DBS, Table_Name, Condition);
      else
         Msg.Error ("Sql.Search > Properties not found for " & Database_Name);
      end if;
      return Result;
   end Search;

   ----------------------------------------------------------------------------
   function Search (DB : in out GSD.Connection'Class; Table_Name : String; Condition : String) return Boolean is
      Result : Boolean := False;
   begin
      if Table_Exists (DB, Table_Name) then
         declare
            RS : GSD.Recordset'Class := DB.Query ("SELECT * FROM " & Table_Name & " WHERE " & Condition & " LIMIT 1");
         begin
            Result := RS.Next;
            RS.Close;
         end;
      else
          Msg.Error ("Sql.Row_Count > Table does not exists: " & Table_Name);
      end if;
      return Result;
   end Search;

   ----------------------------------------------------------------------------
   procedure Set_Config (DB : in out GSD.Connection'Class; Parameter : String; Value : String) is
      Where : String := "Parameter" & " = '" & Parameter & "'";
      Query : String := "Parameter" & ND & Parameter & CD & "Value" & ND & Value;
   begin
      if Database_With_Sys_Tables then
         --  No Mutex as they already exists inside Update and Insert
         if Table_Exists (DB, Table_Sys_Config) then
            if Column_Exists (DB, Table_Sys_Config, "Parameter") and Column_Exists (DB, Table_Sys_Config, "Value") then
               if Search (DB, Table_Sys_Config, Where) then
                  Update (DB, Table_Sys_Config, Query, Where);
               else
                  Insert (DB, Table_Sys_Config, Query);
               end if;
            end if;
         else
            Msg.Error ("Sql.Set_Config > Table does not exists: " & Table_Sys_Config);
         end if;
      end if;
   exception
      when E : others =>
         Msg.Error_Latin_1 ("Sql.Insert > Error: " & Ada.Exceptions.Exception_Information(E));
   end Set_Config;

   ----------------------------------------------------------------------------
   procedure Set_Database_Main (Database_Name : String) is
   begin
      Database_Main := Database_Name;
   end Set_Database_Main;

   ----------------------------------------------------------------------------
   function Table_Exists (DB : in out GSD.Connection'Class; Table_Name : String) return Boolean is
      Tables : Gnoga.Types.Data_Array_Type;
      Result : Boolean := False;
   begin
      Tables := DB.List_Of_Tables;
      --Msg.Debug ("Sql.Table_Exists > Table_Name: " & Table_Name);
      --Msg.Debug ("Sql.Table_Exists > Tables.Length: " & To_String_Unsigned (Natural (Tables.Length)));
      for I in 1 .. Natural (Tables.Length) loop
         --Msg.Debug ("Sql.Table_Exists > Tables.Element (I): " & Tables.Element (I));
         if Tables.Element (I) = Table_Name then
            Result := True;
            exit;
         end if;
      end loop;
      return Result;
   end Table_Exists;

   ----------------------------------------------------------------------------
   procedure Update (Database_Name : String; Table_Name : String; Columns_Values : String; Condition : String) is
      DBT : Sql.Database_Line_Type;
   begin
      DBT := Sql.Properties (Database_Name);
      if DBT.Brand = Sql.MySQL then
         Sql.Update (DBT.DBM, Table_Name, Columns_Values, Condition);
      elsif DBT.Brand = Sql.SQLite then
         Sql.Update (DBT.DBS, Table_Name, Columns_Values, Condition);
      else
         Msg.Error ("Sql.Update > Properties not found for " & Database_Name);
      end if;
   end Update;

   ----------------------------------------------------------------------------
   procedure Update (DB : in out GSD.Connection'Class; Table_Name : String; Columns_Values : String; Condition : String) is
      Description_List : GSD.Field_Description_Array_Type;
      Description : GSD.Field_Description;
      Current_Column, Current_Value, Update_Columns_Values, Current_Type : String := "";
      Sql_Condition : String := Condition;
      Sql_Request : String := "";
      Counter_Columns : constant Natural := Field_Count (Columns_Values, CD);
   begin
      Sql_Mutex.Lock;

      if Table_Exists (DB, Table_Name) then
         Description_List := DB.Field_Descriptions (Table_Name);
         --  Msg.Debug ("Counter_Columns: " & To_String (Natural (Description_List.Last_Index)));

         --  Check each field in parameter against the current table's column
         for Index in 1 .. Counter_Columns loop
            --  Iterate through each column
            for I in Description_List.First_Index .. Description_List.Last_Index loop
               Description := Description_List.Element (I);
               Current_Column := Trim_Both (Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 1, ND));
               --  Msg.Debug (I);
               --  Msg.Debug (Description_List.Last_Index);
               --  Msg.Debug ("Current_Column (PRG name): " & To_Upper (Current_Column));
               --  Msg.Debug ("Current_Column (DB name): " & To_Upper (Description.Column_Name));

               --  If field name and column name match
               if To_Upper (Current_Column) = To_Upper (Description.Column_Name) then
                  --  Fill Name and Value, according to field type
                  Current_Value := Field_By_Index (Field_By_Index (Columns_Values, Index, CD), 2, ND);
                  Current_Type := To_Upper (Slice (Description.Data_Type, 1, 3));
                  Update_Columns_Values := Update_Columns_Values & Current_Column & " = ";
                  --  Msg.Debug ("Current_Value: " & Current_Value);
                  --  Msg.Debug ("Current_Type: " & Current_Type);
                  --  Msg.Debug ("Update_Columns_Values: " & Update_Columns_Values);
                  --  Msg.Debug ("Field Type: " & Current_Type);

                  --  Apply, depending of type :
                  --  BLOB, TEXT, VARCHAR: Single quotes outside string and escaping characters inside string
                  --  BIGINT, DECIMAL, DOUBLE, FLOAT, INTEGER: Insert 0 if empty
                  if Current_Type = "BIG" then --  BIGINT
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "BLO" then --  BLOB
                     Update_Columns_Values := Update_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "DEC" then --  DECIMAL
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "DOU" then --  DOUBLE
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "FLO" then --  FLOAT
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "INT" then --  MySQL INT but SQLite INTEGER
                     Update_Columns_Values := Update_Columns_Values & (if Is_Empty (Current_Value) then From_ASCII ("0") else Current_Value) & ",";
                  elsif Current_Type = "TEX" then --  TEXT
                     Update_Columns_Values := Update_Columns_Values & "'" & Escape_String (Current_Value) & "',";
                  elsif Current_Type = "VAR" then --  VARCHAR
                     Current_Value := Escape_String (Current_Value);
                     --  Truncate string adjusting it to maximum column length
                     if Current_Value.Length > GSD.Field_Size (Description) then
                        Current_Value := Slice (Current_Value, 1, GSD.Field_Size (Description));
                        Msg.Error ("Sql.Insert > String too long to fit in VARCHAR(" &
                                   To_String_Unsigned (GSD.Field_Size (Description)) &
                                   "). Truncate it: " & Slice (Current_Value, 1, GSD.Field_Size (Description)));
                     end if;
                     Update_Columns_Values := Update_Columns_Values & "'" & Current_Value & "',";
                  else
                      Msg.Error ("Sql.Update > Field: " & Current_Column & " does not handle Type: " & Current_Type);
                  end if;
                  exit; --  No need to iterate further after match
               else
                  if I = Description_List.Last_Index then
                      Msg.Error ("Sql.Update > Field: " & Current_Column & " does not exists in Table: " & Table_Name);
                  end if;
               end if;
            end loop;
         end loop;
         --  If at least one Field/Value pair has been processed
         if (Index (Update_Columns_Values, ",") > 0) then
            -- Trailing comma deletion
            Update_Columns_Values := Slice (Update_Columns_Values, 1, Length (Update_Columns_Values) - 1);
            --  Msg.Debug ("Sql.Update > Insert_Columns_Values: " & Update_Columns_Values);
            if not Is_Empty (Sql_Condition) then
               if Index (To_Upper (Sql_Condition), "WHERE") = 0 then
                  Sql_Condition := "WHERE " & Sql_Condition;
               end if;
            end if;
            Sql_Request := "UPDATE " & Table_Name & " SET " & Update_Columns_Values & " " & Sql_Condition & ";";
            Msg.Debug ("Sql.Update > Update: " & Sql_Request);
            DB.Execute_Query (Sql_Request);
         end if;
      else
          Msg.Error ("Sql.Update > Table does not exists: " & Table_Name);
      end if;

      Sql_Mutex.Unlock;
   exception
      when E : others =>
         Msg.Error_Latin_1 ("Sql.Update > Error: " & Ada.Exceptions.Exception_Information(E));
         Sql_Mutex.Unlock;
   end Update;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

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
                        DB_Version : String;
                        Rank : Database_Rank := Main;
                        Sys_Tables : Database_Sys_Tables := Auto_Create) is
      Get_Schema_Version : String;
      DB_Schema_Version : Natural;
      DB_DBS : GSD.SQLite.Connection;
      DB_DBM : GSD.MySQL.Connection;

      --  DB_Major : constant Natural := To_Integer (Field_By_Index (DB_Version, 1, "."));
      --  DB_Minor : constant Natural := To_Integer (Field_By_Index (DB_Version, 2, "."));
      --  Schema_Version : Natural := (DB_Major * 10) + DB_Minor;

      DB_Version_Level : String := DB_Version;
      Schema_Version : Natural := (To_Integer (Field_By_Index (DB_Version, 1, ".")) * 10) +
                                   To_Integer (Field_By_Index (DB_Version, 2, "."));
   begin

      if Sys_Tables = No_Create then
         Database_With_Sys_Tables := False;
      end if;

      --  Preload Database definition
      Schema_Load (Database_Name, DB_Name);
      if DB_Brand = MySQL then
         DB_DBM := GSD.MySQL.Connection (DB);
         null;
      elsif DB_Brand = SQLite then
         DB_DBS := GSD.SQLite.Connection (DB);
         Schema_Load (Database_Pragma,"journal_mode","WAL");
         --  SQLite doc: As of SQLite version 3.6.19, the default setting for foreign key enforcement is OFF.
         Schema_Load (Database_Pragma,"foreign_keys","ON");
         --  Open DB and eventually apply pragmas
         for I of Schema loop
            if I.Command = Database_Name then
               --  Do nothing
               null;
            elsif I.Command = Database_Pragma then
               Msg.Debug ("Load Database_Pragma: " & I.Name & "=" & I.Attribute);
               DB.Execute_Query ("PRAGMA " & I.Name & "=" & I.Attribute);
            else
               exit;
            end if;
         end loop;
      end if;

      if Database_With_Sys_Tables then

         if Table_Exists (DB, Table_Sys_Config) then
            --  Load if exists current database schema version else Database_Version = 0
            Get_Schema_Version := Get_Config (DB, "Schema_Version");
         else
            Get_Schema_Version := "0.0";
            Schema_Version := (if Schema_Version = 0 then 1 else Schema_Version);
            DB_Version_Level := "0.1";
         end if;

         DB_Schema_Version := (To_Integer (Field_By_Index (Get_Schema_Version, 1, ".")) * 10) +
                               To_Integer (Field_By_Index (Get_Schema_Version, 2, "."));

         --  False if DB_Schema_Version is >= Schema_Version => no need updating
         if (DB_Schema_Version < Schema_Version) then
            DB_Status := Open_Need_Update;
            --  System tables creation only for main database
            if Rank = Main then

               -- Preload Sys_Config table definition
               Schema_Load (Table_Name, Table_Sys_Config, Comment => "System config table");
               Sql.Schema_Load (Sql.Column_Name, "Id", "INTEGER", "Primary key");
               Sql.Schema_Load (Table_Constraint, "Id", "PRIMARY KEY");
               Sql.Schema_Load (Sql.Column_Constraint, "Id", "AUTO_INCREMENT");

               Schema_Load (Column_Name, "Parameter", "VARCHAR(40)");
               -- SQLite: raised GNOGA.SERVER.DATABASE.QUERY_ERROR : ALTER TABLE Sys_Config ADD COLUMN
               --                                                      Parameter VARCHAR(40) UNIQUE => Cannot add a UNIQUE column
               -- Schema_Load (Column_Constraint, "Parameter", "UNIQUE", "Parameter name");
               Schema_Load (Column_Name, "Value", "TEXT", "Parameter value");
               Schema_Load (Index_Name, "Idx_Config_Parameter","Parameter");

               if Sys_Tables = Auto_Create then
                  -- Preload Sys_Schema table definition
                  Schema_Load (Table_Name, Table_Sys_Schema, Comment => "System schema table");
                  Sql.Schema_Load (Sql.Column_Name, "Id", "INTEGER", "Primary key");
                  Sql.Schema_Load (Table_Constraint, "Id", "PRIMARY KEY");
                  Sql.Schema_Load (Sql.Column_Constraint, "Id", "AUTO_INCREMENT");

                  Schema_Load (Column_Name, "Table_Name", "VARCHAR(40)");
                  Schema_Load (Column_Name, "Column_Name", "VARCHAR(40)");
                  Schema_Load (Column_Name, "Column_Type", "VARCHAR(20)");
                  Schema_Load (Column_Name, "Column_Constraint", "VARCHAR(20)");
                  Schema_Load (Column_Name, "Version", "VARCHAR(10)");
                  Schema_Load (Column_Name, "Comment", "TEXT");
                  Schema_Load (Index_Name, "Idx_Schema_Table_Name", "Table_Name, Column_Name");

                  --  Preload Sys_User table definition
                  Schema_Load (Sql.Table_Name, Table_Sys_Users, Comment => "System user table");
                  Sql.Schema_Load (Sql.Column_Name, "Id", "INTEGER", "Primary key - User number");
                  Sql.Schema_Load (Table_Constraint, "Id", "PRIMARY KEY");
                  Sql.Schema_Load (Sql.Column_Constraint, "Id", "AUTO_INCREMENT");

                  Schema_Load (Sql.Column_Name, "DTS_Creation", "VARCHAR(15)", "User creation date time stamp");
                  Schema_Load (Sql.Column_Name, "DTS_Update", "VARCHAR(15)", "User update date time stamp");

                  Schema_Load (Sql.Column_Name, "Login", "VARCHAR(40)", "User login");
                  --  SQLite: raised GNOGA.SERVER.DATABASE.QUERY_ERROR :
                  --  ALTER TABLE Sys_Config ADD COLUMN Parameter VARCHAR(40) UNIQUE => Cannot add a UNIQUE column
                  --  Schema_Load (Column_Constraint, "Login", "UNIQUE");
                  Schema_Load (Sql.Column_Name, "First_Name", "VARCHAR(40)", "User surname");
                  Schema_Load (Sql.Column_Name, "Last_Name", "VARCHAR(40)", "User name");
                  Schema_Load (Sql.Column_Name, "Phone", "VARCHAR(20)", "User phone");
                  Schema_Load (Sql.Column_Name, "Email", "VARCHAR(40)", "User email");
                  Schema_Load (Sql.Column_Name, "Password", "VARCHAR(128)", "User hashed password");

                  Schema_Load (Sql.Column_Name, "Password_Errors", "INTEGER", "Password errors counter");
                  Sql.Schema_Load (Sql.Column_Constraint, "Password_Errors", "DEFAULT 0");
                  Schema_Load (Sql.Column_Name, "Password_Validity", "INTEGER", "Password validity in seconds");
                  Sql.Schema_Load (Sql.Column_Constraint, "Password_Validity", "DEFAULT 0");

                  Schema_Load (Sql.Column_Name, "Grants", "VARCHAR(20)", "See v22.ads definitions");
                  Schema_Load (Sql.Column_Name, "Properties", "VARCHAR(40)", "Property_1:Value,Property_2:value...Property_N");
                  Schema_Load (Sql.Column_Name, "Language", "VARCHAR(5)", "Language from country code ISO 3166-1 alpha-2");
                  Schema_Load (Sql.Column_Name, "Time_Zone", "VARCHAR(10)", "Time zone TZ Database compliant see Wikipedia page");
                  Schema_Load (Sql.Column_Name, "Theme", "VARCHAR(20)", "Theme name");
                  Schema_Load (Sql.Column_Name, "Notes", "TEXT", "Notes");

                  Schema_Load (Sql.Column_Name, "Connection_Total", "INTEGER", "Duration of cumulated connections");
                  Sql.Schema_Load (Sql.Column_Constraint, "Connection_Total", "DEFAULT 0");
                  Schema_Load (Sql.Column_Name, "Connection_Counter", "INTEGER", "Total number of connections");
                  Sql.Schema_Load (Sql.Column_Constraint, "Connection_Counter", "DEFAULT 0");

                  Schema_Load (Sql.Column_Name, "Connection_Info", "VARCHAR(15)", "Current or last connection info (Datetime, IP, etc.");

                  Schema_Load (Sql.Index_Name, "Idx_User_Login", "Login");
                  --Schema_Load (Sql.Index_Constraint, "Idx_User_Login", "UNIQUE");
               end if;
            end if;
         end if;

      else
         DB_Status := Open_Success; --Open_Need_Update;
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
                                       --  Store DB_Version for later writing in Schema_Update,
                                       --  which must be done at the very end of the update
                                       --  process to ensure update completion
                                       Version => DB_Version_Level,
                                       DBS => DB_DBS,
                                       DBM => DB_DBM
                                       ));
      --  Msg.Info (40 * "-");
      --  -- Index
      --  DB_Index : Databases_List.Extended_Index;
      --  DB_Index := Databases.Last_Index;
      --  Msg.Debug ("DB_Index: " & From_Latin_1 (Databases_List.Extended_Index'Image (DB_Index)));
      --  Msg.Debug ("URI:      " & Databases(DB_Index).URI);
      --  -- Last_Element
      --  Msg.Debug ("URI:      " & Databases.Last_Element.URI);
      --  Msg.Debug ("DBM:      " & From_Latin_1 (Databases.Last_Element.DBM'Image));
      --  Msg.Debug ("DBS:      " & From_Latin_1 (Databases.Last_Element.DBS'Image));
      --  Msg.Debug ("URI:      " & Databases.Last_Element.URI);
      --  Msg.Debug ("Name:     " & Databases.Last_Element.Name);
      --  Msg.Debug ("Host:     " & Databases.Last_Element.Host);
      --  Msg.Debug ("Port:     " & To_String (Databases.Last_Element.Port));
      --  Msg.Debug ("User:     " & Databases.Last_Element.User);
      --  Msg.Debug ("Password: " & Databases.Last_Element.Password);
      --  Msg.Debug ("File:     " & Databases.Last_Element.File);
      --  Msg.Debug ("Major:    " & To_String (Databases.Last_Element.Major));
      --  Msg.Debug ("Minor:    " & To_String (Databases.Last_Element.Minor));
      --  Msg.Info (40 * "-");

   end Open_Load;

   ----------------------------------------------------------------------------
   procedure Ping_Send is
   begin
      Msg.Debug ("Ping on databases triggered");
      for C in Databases.Iterate loop
         if Databases(C).Brand = MySQL then
            Sql_Mutex.Lock;
            Databases(C).DBM.Execute_Query ("SELECT 1");
            Sql_Mutex.Unlock;
            Msg.Debug ("Ping on " & From_Latin_1 (Databases(C).Brand'Image) & " database: " & Databases(C).Name);
         elsif Databases(C).Brand = SQLite then
            --  Not applicable
            null;
         end if;
      end loop;

   exception
      when E : others =>
         Msg.Error_Latin_1 ("Sql.Insert > Error: " & Ada.Exceptions.Exception_Information(E));
         Sql_Mutex.Unlock;
   end Ping_Send;

------------------------------------------------------------------------------
end v22.Sql;
------------------------------------------------------------------------------
