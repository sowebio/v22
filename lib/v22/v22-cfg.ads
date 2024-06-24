-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-cfg.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - config file manager
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with v22.Prg;
with v22.Tio;
with v22.Uxs; use v22.Uxs;

package v22.Cfg is

   pragma Elaborate_Body;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Close;
   --  Close Cfg file. For sanity only as each setting is instantly flushed to
   --  disk.

   procedure Comment (Text : String);
   --  Insert a comment to the next line.

   procedure Delete (Section : String; Parameter : String);
   --  Delete parameter in section. If no other parameter in this section,
   --  delete section too. Avoid reserved chars [ ] = # inside parameters.
   --  If reserved chars are passed, the procedure does nothing.

   function Get (Section : String; Parameter : String) return String;
   --  Return parameter in section or empty string if not found. Avoid reserved
   --  chars [ ] = # inside parameters.

   function Get_Name return String;
   --  Returns full qualified (with path) application configuration file name.

   procedure New_Line;
   --  Insert a blank line to the next line.

   function Open (Cfg_File_Read_In : String := "") return Boolean;
   --  Open and load if exist a configuration file. Create blank if non
   --  existent. Default configuration file name is full qualified program name
   --  followed by .cfg extension and created in the program start directory.
   --  This default file name may be changed by Set_Name procedure.

   procedure Set (Section : String; Parameter : String; Value : String; Comment : String := "");
   --  Create or replace an existing parameter in a section. If this latter
   --  does not exist, also creating it. New setting is persistent even program
   --  quits unexpectedly after. Avoid reserved chars [ ] = # inside
   --  parameters. If reserved chars are passed, the procedure does nothing. A
   --  optional trailing comment can also be added.

   procedure Set_Name (Cfg_File_Read_In : String);
   --  Set application configuration file name, relative or full qualified.

-------------------------------------------------------------------------------
private

   Cfg_File_Read : String := Prg.Start_Dir & "/" & Prg.Name & ".cfg";
   Cfg_File_Write : String := Prg.Start_Dir & "/" & Prg.Name & ".tmp";

   Cfg_Open_Section : constant String := "[";
   Cfg_Close_Section : constant String := "]";
   Cfg_Assignment : constant String := "=";
   Cfg_Comment : constant String := "#";
   Cfg_Command_Delete : constant String := Cfg_Open_Section & Cfg_Open_Section & "D";
   Cfg_Command_Add : constant String := Cfg_Open_Section & Cfg_Open_Section & "P";

   --  Memory consumption test of an array of String
   --  Table_Max  ram
   --  (elements) (bytes)
   --  500        1088
   --  250        488

   Table_Max : Natural := 250;
   type Table_Lines is array (1 .. Table_Max) of String;
   Cfg_Table : Table_Lines;

   Cfg_Last : Natural := 0;
   Cfg_Section : Natural := 0;
   Cfg_Parameter : Natural := 0;

   Handle_Read : Tio.File;
   Handle_Write : Tio.File;

   --  Service functions
   function Cfg_Read return Boolean;
   procedure Cfg_Search (Section : String; Parameter : String);
   function Cfg_Write (Section : String := ""; Parameter : String := ""; Value : String := ""; Trailing_Comment : String := "")
                       return Boolean;
   function Check_Parameters (Section : String; Parameter : String; Value : String) return Boolean;
   function Table_Write (Line : String) return Boolean;
   --  procedure Table_Write (Line : String);

-------------------------------------------------------------------------------
end v22.Cfg;
-------------------------------------------------------------------------------
