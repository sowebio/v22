-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-utf.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Text I/O package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Text_IO;



with UXStrings.Text_IO;
with UXStrings.Text_IO.Text_Streams;
with UXStrings.Conversions;

with Interfaces;

with v22.Uxs; use v22.Uxs;

package v22.Uxf is

   package UXS  renames UXStrings;
   package UTI  renames UXStrings.Text_IO;
   package UTS  renames UXStrings.Text_IO.Text_Streams;

   ----------------------------------------------------------------------------
   --  API - Terminal
   ----------------------------------------------------------------------------


   ----------------------------------------------------------------------------
   --  API - Text File
   ----------------------------------------------------------------------------

   subtype File is UTI.File_Type;

   procedure Append (Handle : in out File; Name : String);
   --  Append on an existing file.

   procedure Close (Handle : in out File) renames UTI.Close;
   --  Close a file.

   procedure Create (Handle : in out File; Name : String);
   --  Create a file.

   function End_Of_File (Handle : in out File) return Boolean renames UTI.End_Of_File;
   --  Test if enf of file reached.

   function End_Of_Line (Handle : in out File) return Boolean renames UTI.End_Of_Line;
   --  Test if end of line reached.

   procedure Flush (Handle : in File) renames UTI.Flush;
   --  Flush file buffer to disk.

   function Get_Line (Handle : in out File) return String;
   procedure Get_Line (Handle : in out File; V : in out String);
   --  Read a line and then move the file pointer to the next line.

   function Is_Open (Handle : File) return Boolean renames UTI.Is_Open;
   --  Test if a file is open.

   procedure New_Line (Handle : in File; Spacing : UTI.Positive_Count := 1) renames UTI.New_Line;
   --  Add a new line to a file.

   procedure Open_Read (Handle : in out File; Name : String; Scheme : Encoding_Scheme := UTF_8);
   --  Open a file in read mode. Maximum file length is one petabyte (1024 TB).

   procedure Put (Handle : File; S : String) renames UTI.Put;
   --  Write to a file.

   procedure Put_Line (Handle : File; S : String) renames UTI.Put_Line;
   --  Write a file and then add a new line

   procedure Reset (Handle : in out File) renames UTI.Reset;
   --  Reset the file pointer to the start of the file

-------------------------------------------------------------------------------
private

   Ansi_State : Boolean := True;

-------------------------------------------------------------------------------
end v22.Uxf;
-------------------------------------------------------------------------------
