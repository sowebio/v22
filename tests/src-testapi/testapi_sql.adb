-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      TestApi_Sql.adb
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
      Integer_Value : Integer;

   begin

      -------------------------------------------------------------------------
      Msg.Set_Task ("SQL T1");
      Msg.Title ("Money conversion to and from integer");
      Msg.Line;

      Tio.Put_Line ("Money is delta 0.01 digits 10 - Money'Image (Money'Last): " & From_Latin_1 (Money'Image (Money'Last)));
      Msg.Line;

      Integer_Value := From_Money (Money_Value);
      Msg.Std (Integer_Value);
      Msg.Std (To_Money (Integer_Value));

      Money_Value := -100.01;
      Integer_Value := From_Money (Money_Value);
      Msg.Std (Integer_Value);
      Msg.Std (To_Money (Integer_Value));
      Integer_Value := -10001;
      Msg.Std (Integer_Value);
      Msg.Line;

      ----------------------------------------------------------------------------
      Msg.Set_Task ("SQL T2");
      Msg.Title ("Opening two SQLite databases");
      --Msg.Line;

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

      Msg.Line;

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

      Msg.Std ("Get_Version - SQLite: " & Sql.Get_Version (DBS_1));

      DBT := Sql.Properties ("v22_testapi_1");
      if DBT.Brand /= None then
         Msg.Dbg ("Get URI from Properties: " & DBT.URI);
      else
         Msg.Err ("DB_Properties not found for: v22_testapi");
      end if;

      Msg.Std ("Test using Execute_Query to insure good freeing of previous results");
      Msg.Std ("Theses 10 consecutive Execute_Query calls should not raise exception");
      for I in 1 .. 10 loop
         DBS_1.Execute_Query ("SELECT * FROM Tbl_Cluster");
      end loop;

      Msg.Std ("Test with empty recordsets");
      Msg.Std ("Theses consecutive calls should not raise exceptions");
      Msg.Line;
      DBS_1.Execute_Query ("BEGIN TRANSACTION");
      DBS_1.Execute_Query ("SAVEPOINT SV_Index_Exists");
      Result := DBS_1.Execute_Update ("SELECT * FROM Tbl_Cluster");
      DBS_1.Execute_Query ("COMMIT");
      Msg.Std ("Result for an existant index (must be 1): " & Image (Result));
      Msg.Line;

      Msg.Std ("Table_Exists - SQLite - Existing table: " & Image (Sql.Table_Exists (DBS_1, "Sys_Config")));
      Msg.Std ("Table_Exists - SQLite - Non existing table: " & Image (Sql.Table_Exists (DBS_1, "Sys_Config_non_existing")));
      Msg.Line;

      Msg.Std ("Index_Exists - SQLite - Existing index: " & Image (Sql.Index_Exists (DBS_1, "Sys_Config", "Idx_Config_Parameter")));
      Msg.Std ("Index_Exists - SQLite - Non existing index: " & Image (Sql.Index_Exists (DBS_1, "Sys_Config", "Idx_Config_Parameter_non_existing")));
      Msg.Line;

      Msg.Std ("Column_Exists - SQLite - Existing column: " & Image (Sql.Column_Exists (DBS_1, "Sys_Config", "Parameter")));
      Msg.Std ("Column_Exists - SQLite - Non existing column: " & Image (Sql.Column_Exists (DBS_1, "Sys_Config", "Parameter_non_existing")));
      Msg.Line;

      Msg.Std ("Insert - SQLite - Creating lines");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key^Key_Private~Private key");
      Msg.Std ("Insert - SQLite - One non existing field, check log: an error must appear");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key^Key_Private_non_existing~Private key^Key_Public~Public key");
      Msg.Line;

      Msg.Std ("Update - SQLite - Existing lines");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key (update)^Key_Private~Private key (update)");
      Msg.Std ("Update - SQLite - One non existing field, check log: an error must appear");
      Sql.Insert (DBS_1, "Tbl_Cluster", "Key_Name~Name of the key (update)^Key_Private_non_existing~Private key (update)^Key_Public~Public key (update)");
      Msg.Line;

      Msg.Std ("Last_RowID - SQLite: " & Image (Sql.Last_RowID (DBS_1, "Sys_Config")));
      Msg.Line;

      Msg.Std ("Read - SQLite:");
      Msg.Line;
      Field_Display (Sql.Read (DBS_1, "Tbl_Cluster", "Key_Name, Key_Private", "WHERE Key_Name='Name of the key'"), CD, RD, "Key_Name title, Key_Private  title");
      Msg.Line;

      Msg.Std ("Row_Count - SQLite: " & Image (Sql.Row_Count (DBS_1, "Sys_Config")));
      Msg.Line;

      Msg.Std ("Search - SQLite - Existing: " & Image (Sql.Search (DBS_1, "Tbl_Cluster", "Key_Name='Name of the key'")));
      Msg.Std ("Search - SQLite - Non existing: " & Image (Sql.Search (DBS_1, "Tbl_Cluster", "Key_Name='Non existing'")));
      Msg.Line;

      Msg.Std ("Set_Config - SQLite - Write Test with 1: ");
      Sql.Set_Config (DBS_1, "Test", "1");
      Msg.Std ("Get_Config - SQLite - Read Test: " & Sql.Get_Config (DBS_1, "Test"));
      Msg.Line;

      --  Close all opened databases
      Sql.Close;
      Msg.Line;

   end Run;

-------------------------------------------------------------------------------
end TestApi_Sql;
-------------------------------------------------------------------------------
