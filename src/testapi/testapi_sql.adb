-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_sql.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   GPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - API test program
--
--  @description
--  Build application and documentation
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

package body TestApi_Sql is

   procedure Run is

      package GSD renames Gnoga.Server.Database;

      use v22.Sql; -- for Sql.Database_Brand operators

      DBS_1 : GSD.SQLite.Connection;
      DBS_2 : GSD.SQLite.Connection;
      DBT : Sql.Database_Line_Type;

      Result : Integer;
      --Result_Status : Sql.Database_Status;

      Money_Value : Money := 100.01;
      Bigint_Value : Long_Long_Integer;

   begin

      -------------------------------------------------------------------------
      Msg.Set_Task ("SQL T1");
      Msg.Title ("Money conversion to and from Long_Long_Integer");
      Msg.New_Line;

      Tio.Put_Line ("Money is delta 0.01 digits 10 - Money'Image (Money'Last): " & From_Latin_1 (Money'Image (Money'Last)));
      Msg.New_Line;

      -- Not relevant anymore
      --  Bigint_Value := From_Money_To_DB (Money_Value);
      --  Msg.Info (Bigint_Value);
      --  Msg.Info (To_Money (Bigint_Value));
      --  Msg.New_Line;
      --  Money_Value := -100.01;
      --  Bigint_Value := From_Money_To_DB (Money_Value);
      --  Msg.Info (Bigint_Value);
      --  Msg.Info (To_Money (Bigint_Value));
      --  Bigint_Value := -10001;
      --  Msg.Info (Bigint_Value);
      --  Msg.New_Line;
      Money_Value := 0.0;
      Msg.Info (Money_Value);
      Msg.Info ("1234567890");
      Msg.Info (From_Latin_1 (Money'Image (Money_Value)));
      Msg.Info (From_Latin_1 (Money'Image (-1.1)));
      Msg.New_Line;
      Msg.Info ("123: " & From_Money_To_DB ("123"));
      Msg.Info ("123.: " & From_Money_To_DB ("123."));
      Msg.Info ("123.1: " & From_Money_To_DB ("123.1"));
      Msg.Info ("123.12: " & From_Money_To_DB ("123.12"));
      Msg.Info ("123.123456: " & From_Money_To_DB ("123.123456"));
      Msg.New_Line;
      Msg.Info (".1: " & From_Money_To_DB (".1"));
      Msg.Info (".12: " & From_Money_To_DB (".12"));
      Msg.Info (".123456: " & From_Money_To_DB (".123456"));
      Msg.Info ("-0.123456: " & From_Money_To_DB ("-0.123456"));
      Msg.Info ("-.123456: " & From_Money_To_DB ("-.123456"));
      Msg.Info ("-.: " & From_Money_To_DB ("-."));
      Msg.Info ("-: " & From_Money_To_DB ("-"));
      Msg.New_Line;
      Msg.Info ("000: " & From_DB_To_Money_String ("000"));
      Msg.Info ("001: " & From_DB_To_Money_String ("001"));
      Msg.Info ("012: " & From_DB_To_Money_String ("012"));
      Msg.Info ("12312: " & From_DB_To_Money_String ("12312"));
      Msg.Info ("-001: " & From_DB_To_Money_String ("-001"));
      Msg.Info ("-012: " & From_DB_To_Money_String ("-012"));
      Msg.Info ("-12312: " & From_DB_To_Money_String ("-12312"));


      ----------------------------------------------------------------------------
      Msg.Set_Task ("SQL T2");
      Msg.Title ("Opening two SQLite databases");

      if Sql.Open (DBS_1, "file:v22_testapi_1.db", "1.0") = Open_Need_Update then

         Sql.Schema_Load (Sql.Table_Name,        "Tbl_Cluster", Comment => "Clusters table");
         Sql.Schema_Load (Sql.Column_Name,       "Number", "INTEGER", "Cluster number (1) 1...240");
         Sql.Schema_Load (Sql.Column_Constraint, "Number", "UNIQUE");
         Sql.Schema_Load (Sql.Table_Constraint,  "Number", "PRIMARY KEY");

         Sql.Schema_Load (Sql.Column_Name,       "Key_Name",            "TEXT",    "Cluster key name");
         Sql.Schema_Load (Sql.Column_Name,       "Key_Private",         "BLOB",    "Cluster private key");
         Sql.Schema_Load (Sql.Column_Name,       "Key_Public",          "BLOB",    "Cluster public key");
         Sql.Schema_Load (Sql.Column_Name,       "Supervisor_Instance", "INTEGER", "Supervisor instance");
         --Sql.Schema_Load (Sql.Column_Constraint, "Supervisor_Instance", "REFERENCES Tbl_Instance(Number)");
         Sql.Schema_Load (Sql.Column_Name,       "Comment",             "TEXT",    "Comment");
         --Sql.Schema_Load (Sql.Column_Name,       "Thingy1",              "TEXT",    "Thingy1 created at v1.1");
         --Sql.Schema_Load (Sql.Column_Name,       "Thingy2",              "TEXT",    "Thingy2 created at v1.1");

         Sql.Schema_Load (Sql.Index_Name,        "Idx_Cluster_Number", "Number");
         Sql.Schema_Load (Sql.Index_Constraint,  "Idx_Cluster_Number", "UNIQUE");

         Sql.Schema_Update (DBS_1);
      end if;

      Msg.New_Line;

      if Sql.Open (DBS_2, "file:v22_testapi_2.db", "1.0") = Open_Need_Update then
         Sql.Schema_Load (Sql.Table_Name,        "Tbl_Cluster", Comment => "Clusters table");
         Sql.Schema_Load (Sql.Column_Name,       "Number", "INTEGER", "Cluster number (1) 1...240");
         Sql.Schema_Load (Sql.Column_Constraint, "Number", "UNIQUE");
         Sql.Schema_Load (Sql.Table_Constraint,  "Number", "PRIMARY KEY");

         Sql.Schema_Load (Sql.Column_Name,       "Key_Name",            "TEXT",    "Cluster key name");
         Sql.Schema_Load (Sql.Column_Name,       "Key_Private",         "BLOB",    "Cluster private key");
         Sql.Schema_Load (Sql.Column_Name,       "Key_Public",          "BLOB",    "Cluster public key");
         Sql.Schema_Load (Sql.Column_Name,       "Supervisor_Instance", "INTEGER", "Supervisor instance");
         Sql.Schema_Load (Sql.Column_Name,       "Comment",             "TEXT",    "Comment");

         Sql.Schema_Load (Sql.Index_Name,        "Idx_Cluster_Number", "Number");
         Sql.Schema_Load (Sql.Index_Constraint,  "Idx_Cluster_Number", "UNIQUE");

         Sql.Schema_Update (DBS_2);
      end if;

      Msg.Info ("Get_Version - SQLite: " & Sql.Get_Version (DBS_1));

      DBT := Sql.Properties ("v22_testapi_1");
      if DBT.Brand /= None then
         Msg.Debug ("Get URI from Properties: " & DBT.URI);
      else
         Msg.Error ("DB_Properties not found for: v22_testapi");
      end if;

      Msg.Info ("Test using Execute_Query to insure good freeing of previous results");
      Msg.Info ("Theses 10 consecutive Execute_Query calls should not raise exception");
      for I in 1 .. 10 loop
         DBS_1.Execute_Query ("SELECT * FROM Tbl_Cluster");
      end loop;

      Msg.Info ("Test with empty recordsets");
      Msg.Info ("Theses consecutive calls should not raise exceptions");
      Msg.New_Line;
      DBS_1.Execute_Query ("BEGIN TRANSACTION");
      DBS_1.Execute_Query ("SAVEPOINT SV_Index_Exists");
      Result := DBS_1.Execute_Update ("SELECT * FROM Tbl_Cluster");
      DBS_1.Execute_Query ("COMMIT");
      Msg.Info ("Result for an existant index (must be 1): " & Image (Result));
      Msg.New_Line;

      Msg.Info ("Table_Exists - SQLite - Existing table: " & Image (Sql.Table_Exists (DBS_1, "Sys_Config")));
      Msg.Info ("Table_Exists - SQLite - Non existing table: " & Image (Sql.Table_Exists (DBS_1, "Sys_Config_non_existing")));
      Msg.New_Line;

      Msg.Info ("Index_Exists - SQLite - Existing index: " & Image (Sql.Index_Exists (DBS_1, "Sys_Config", "Idx_Config_Parameter")));
      Msg.Info ("Index_Exists - SQLite - Non existing index: " & Image (Sql.Index_Exists (DBS_1, "Sys_Config", "Idx_Config_Parameter_non_existing")));
      Msg.New_Line;

      Msg.Info ("Column_Exists - SQLite - Existing column: " & Image (Sql.Column_Exists (DBS_1, "Sys_Config", "Parameter")));
      Msg.Info ("Column_Exists - SQLite - Non existing column: " & Image (Sql.Column_Exists (DBS_1, "Sys_Config", "Parameter_non_existing")));
      Msg.New_Line;

      Msg.Info ("Insert - SQLite - Creating lines");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key^Key_Private~Private key");
      Msg.Info ("Insert - SQLite - One non existing field, check log: an error must appear");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key^Key_Private_non_existing~Private key^Key_Public~Public key");
      Msg.New_Line;

      Msg.Info ("Update - SQLite - Existing lines");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key (update)^Key_Private~Private key (update)");
      Msg.Info ("Update - SQLite - One non existing field, check log: an error must appear");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key (update)^Key_Private_non_existing~Private key (update)^Key_Public~Public key (update)");
      Msg.New_Line;

      Msg.Info ("Last_RowID - SQLite: " & Image (Sql.Last_RowID (DBS_1, "Sys_Config")));
      Msg.New_Line;

      Msg.Info ("Read - SQLite:");
      Msg.New_Line;
      Field_Display (Sql.Read (DBS_1, "Tbl_Cluster", "Key_Name, Key_Private",
                      "WHERE Key_Name='Name of the key'"), CD, RD, "Key_Name title, Key_Private title");
      Msg.New_Line;

      Msg.Info ("Row_Count - SQLite: " & Image (Sql.Row_Count (DBS_1, "Sys_Config")));
      Msg.New_Line;

      Msg.Info ("Search - SQLite - Existing: " & Image (Sql.Search (DBS_1, "Tbl_Cluster", "Key_Name='Name of the key'")));
      Msg.Info ("Search - SQLite - Non existing: " & Image (Sql.Search (DBS_1, "Tbl_Cluster", "Key_Name='Non existing'")));
      Msg.New_Line;

      Msg.Info ("Set_Config - SQLite - Write Test with 1: ");
      Sql.Set_Config (DBS_1, "Test", "1");
      Msg.Info ("Get_Config - SQLite - Read Test: " & Sql.Get_Config (DBS_1, "Test"));
      Msg.New_Line;

      --  Close all opened databases
      Sql.Close;
      Msg.New_Line;

   end Run;

-------------------------------------------------------------------------------
end TestApi_Sql;
-------------------------------------------------------------------------------
