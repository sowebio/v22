-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-sql.ads
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
--  Dmitry Kazakov - dk - http://www.dmitry-kazakov.de (low level SQLite binding in sql directory)
--  Stéphane Rivière - sr - sriviere@soweb.io (high level SQLite binding, some low level binding hacks)
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

-- Schema
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Vectors;
with Ada.Text_IO;

with Interfaces.C.Strings;

with Gnoga.Types;
with Gnoga.Server.Database.MySQL; use Gnoga.Server.Database.MySQL;
with Gnoga.Server.Database.SQLite;

with v22.Uxs; use v22.Uxs;

package v22.Sql is

   package ICS renames Interfaces.C.Strings;
   package GSD renames Gnoga.Server.Database;

   type Database_Brand is (None, MySQL, SQLite, Unknown);

   type Database_Status is (None,
                            Open_Failed,
                            Open_Failed_Parameter_Invalid,
                            Open_Success,
                            Open_Need_Update);

   type Schema_Command is (Null_Command,
                           Database_Name, Database_Pragma,
                           Table_Name, Table_Constraint,
                           Column_Name, Column_Constraint,
                           Index_Name, Index_Constraint);

   package Schema_Command_List is new Ada.Text_IO.Enumeration_IO (Enum => Schema_Command);

   type Database_Line is record
      Brand : Database_Brand := None;
      Status : Database_Status := None;
      URI : String;
      Name : String;
      Host : String;
      Port : Natural;
      User : String;
      Password : String;
      File : String;
      Version : String;
      DBM : GSD.MySQL.Connection;
      DBS : GSD.SQLite.Connection;
   end record;

   package Databases_List is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Database_Line);
   subtype Database_Line_Type is Database_Line; -- Mandatory by the compiler

   type Schema_Line is record
      Command : Schema_Command;
      Name : String;
      Attribute : String;
      Comment : String;
   end record;

   package Schema_Lines_List is new Ada.Containers.Vectors (Index_Type => Natural, Element_Type => Schema_Line);

------------------------------------------------------------------------------

   function Brand (DB : in out GSD.Connection'Class) return Database_Brand;
   --  Returns the Database_Type of DB

   procedure Close (DB : in out GSD.Connection'Class);
   procedure Close;
   -- Close a database or all data database without passing database handle.

   function Column_Exists (DB : in out GSD.Connection'Class;
                           Table_Name : String;
                           Column_Name : String) return Boolean;
   --  Return true if Column_Name exists in Table_Name.
   --  Return False if Column_Name or Table_Name does not exist.

   procedure Delete (DB : in out GSD.Connection'Class;
                     Table_Name : String;
                     Where_Condition : String);
   --  Delete a row in Table_Name specifying a Where_Condition.

   function From_Money (DB_Money : Money) return Integer;
   --  Convert type Money to Integer for accurate storage.

   function Get_Config (DB : in out GSD.Connection'Class;
                        Parameter : String) return String;
   --  Get configuration Value from Parameter stored in Sys_Config table.
   --  The Sys_Config table must be already created. Returns an empty string
   --  if the Sys_Config table or parameter does not exist.

   function Get_Version (DB : in out GSD.Connection'Class) return String;
   --  Returns MySQL or SQLite database version.
   --  MySQL: v10.3.39-MariaDB-0+deb10u1 (output sample).
   --  SQLite: v3.43.0 (output sample).

   function Index_Exists (DB : in out GSD.Connection'Class;
                          Table_Name : String;
                          Index_Name : String) return Boolean;
   --  Return True if Index_Name exists for Table_Name.
   --  Return False if Index_Name or Table_Name does not exist.
   --  Names are case insensitive for MySQL and case sensitive for SQLite.

   procedure Insert (DB : in out GSD.Connection'Class;
                     Table_Name : String;
                     Columns_Values : String);
   --  Create a row in Table_Name with Columns_Values.
   --  The special character ^ (or constant CD as Column delimiter) is used to
   --  separate column/value pairs and the special character ~ (or constant ND
   --  as Name/value delimiter) is used to distinguish the name of a column
   --  from its value. A non existent Table or Column don't raise exception
   --  but an error is logged.

   function Last_RowID (DB : in out GSD.Connection'Class;
                        Table_Name : String) return Integer;
   -- Returns last existing RowID in Table_Name.

   function Open (DBM : in out GSD.MySQL.Connection;
                  URI : String := "";
                  Version : String := "") return Database_Status;
   function Open (DBS : in out GSD.SQLite.Connection;
                  URI : String := "";
                  Version : String := "") return Database_Status;
   --  Open a database.
   --  URI conforms to RFC 3986 URI. See examples below:
   --  MySQL:  db:db_name?host=192.168.0.243&port=3306&user=user_name&password=user_password
   --  SQLite: file:data.db or file:data.db?mode=ro&cache=private
   --          see https://www.sqlite.org/c3ref/open.html for more information.
   --  DB_Version is major.minor DB schema version
   --  Returns Database_Status see above

   function Properties (DB_Name : String) return Database_Line;
   -- Returns database properties record.

   function Read (DB : in out GSD.Connection'Class;
                  Table_Name : String;
                  Columns : String;
                  Condition : String := "") return String;
   --  Returns an extraction from Table_Name with comma delimited Columns and
   --  standard SQL Condition (like WHERE, ORDER BY, LIMIT).
   --  The extraction is formatted with standard v22 CD constant as Column
   --  delimiter and RD constant as Row delimiter.

   --  function Row_Count (DB : in out GSD.MySQL.Connection;
   --                      Table_Name : String;
   --                      Option : String := "*") return Integer;
   function Row_Count (DB : in out GSD.Connection'Class;
                       Table_Name : String;
                       Option : String := "*") return Integer;
   --  Returns counted rows in Table_Name with Options All and Distinct.


   procedure Schema_Load (Command : in Schema_Command := Null_Command;
                             Name : in String := "";
                        Attribute : in String := "";
                          Comment : in String := "");
   -- Load a schema line. Commands will be executed with Schema_Update in code
   -- source order

   procedure Schema_Update (DB : in out GSD.Connection'Class);
   --  Create, read, update and delete operations on database schema after
   --  loading schema by Schema_Load

   function Search (DB : in out GSD.Connection'Class;
                    Table_Name : String;
                    Condition : String) return Boolean;
   --  Return True if Condition verified.

   procedure Set_Config (DB : in out GSD.Connection'Class;
                         Parameter : String;
                         Value : String);
   --  Store configuration Parameter and Value to Sys_Config table.
   --  The new Value will replaced the eventually existing one.
   --  The Sys_Config table will be created if needed.

   function Table_Exists (DB : in out GSD.Connection'Class;
                          Table_Name : String) return Boolean;
   --  Return true if Table_Name exists.

   function To_Money (DB_Integer : Integer) return Money;
   --  Convert Integer storage to Money type for accurate computing.

   procedure Update (DB : in out GSD.Connection'Class;
                     Table_Name : String;
                     Columns_Values : String;
                     Where_Condition : String);
   --  Update one or more row in Table_Name with Columns_Values specifying a
   --  Where_Condition.
   --  The special character ^ (or constant CD as Column delimiter) is used to
   --  separate column/value pairs and the special character ~ (or constant ND
   --  as Name/value delimiter) is used to distinguish the name of a column
   --  from its value.
   --  A non existent Table or Column don't raise exception but an error is
   --  logged.





   ----------------------------------------------------------------------------
   --  Table_Dont_Exists : exception;
   --
   --  subtype Statement is SQLite.Statement;
   --  subtype Datatype  is SQLite.Datatype;
   --  subtype Integer_64 is Interfaces.Integer_64;
   --
   --  --  Command           Name          Attribute
   --  --  -------------------------------------------------
   --  --  Database_Name     Gnx
   --  --  Database_Pragma   journal_mode  WAL
   --  --  Database_Pragma   synchronous   FULL
   --  --  Database_Pragma   foreign_keys  ON
   --  --  Database_Schema   prog_version  x.y
   --  --  Table_Name        Cluster
   --  --  Column_Name       Number        INTEGER
   --  --  Column_Constraint Number        UNIQUE
   --  --  Column_Name       Default       INTEGER
   --  --  Column_Name       Domain        TEXT
   --  --  Column_Name       Email         TEXT
   --  --  Column_Name       Manager       INTEGER
   --  --  Column_Name       API_EP        TEXT
   --  --  Column_Name       API_AK        TEXT
   --  --  Column_Name       API_AS        TEXT
   --  --  Column_Name       API_CK        TEXT
   --  --  Table_Constraint  Number        PRIMARY KEY
   --  --  Table_Options
   --  --  Index_Name        Idx           Number
   --  --  Index_Constraint  Idx           UNIQUE



   --  --  https://www.sqlite.org/rescode.html
   --  --  Gap in numbering means unused code
   --  Info_Ok            : constant Integer := 0;
   --  Info_Row           : constant Integer := 100;
   --  Info_Done          : constant Integer := 101;
   --
   --  Error_Generic      : constant Integer := 1;
   --  Error_Internal     : constant Integer := 2;
   --  Error_Perm         : constant Integer := 3;
   --  Error_Abort        : constant Integer := 4;
   --  Error_Busy         : constant Integer := 5;
   --  Error_Locked       : constant Integer := 6;
   --  Error_No_Mem       : constant Integer := 7;
   --  Error_Readonly     : constant Integer := 8;
   --  Error_Interrupt    : constant Integer := 9;
   --  Error_In_Out       : constant Integer := 10;
   --  Error_Corrupt      : constant Integer := 11;
   --  Error_Not_Found    : constant Integer := 12;
   --  Error_Full         : constant Integer := 13;
   --  Error_Cant_Open    : constant Integer := 14;
   --  Error_Protocol     : constant Integer := 15;
   --  Error_Schema       : constant Integer := 17;
   --  Error_Too_Big      : constant Integer := 18;
   --  Error_Constraint   : constant Integer := 19;
   --  Error_Mismatch     : constant Integer := 20;
   --  Error_Misuse       : constant Integer := 21;
   --  Error_No_Lfs       : constant Integer := 22;
   --  Error_Authent      : constant Integer := 23;
   --  Error_Range        : constant Integer := 25;
   --  Error_Not_A_DB     : constant Integer := 26;

   --  function Error (Information : String ; Information_Extended : out String) return Integer;
   --  --  SQLite error processing
   --
   --  procedure Error (Exception_Hook : Ada.Exceptions.Exception_Occurrence);
   --  --  SQLite error processing
   --
   --  function Error_Display (Error_Code : Integer) return String;
   --  --  SQLite status, info and error display

------------------------------------------------------------------------------
private

   Table_Sys_Config : String := "Sys_Config";
   Table_Sys_Schema : String := "Sys_Schema";

   Databases : Databases_List.Vector;
   Schema : Schema_Lines_List.Vector;

   function Sqlite3_Libversion return ICS.chars_ptr;
   pragma Import (C, Sqlite3_Libversion, "sqlite3_libversion");

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
                        DB_Version : String);

------------------------------------------------------------------------------
end v22.Sql;
------------------------------------------------------------------------------
