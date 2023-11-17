-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-tio.ads
--  @copyright See authors list below and v22.copyrights file
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
with Interfaces;

with v22.Uxs; use v22.Uxs;

package v22.Tio is

   package ATI renames Ada.Text_IO;

   ----------------------------------------------------------------------------
   --  API - Terminal
   ----------------------------------------------------------------------------

   Max_Row  : constant Natural := 29;
   Max_Column : constant Natural := 79;

   subtype Row is Natural range 0 .. Max_Row;
   subtype Column  is Natural range 0 .. Max_Column;
   subtype Integer_64 is Interfaces.Integer_64;

   procedure Put (B : Boolean);
   procedure Put (B : On_Off);
   procedure Put (C : Character) renames ATI.Put;
   procedure Put (V : String);
   procedure Put (I : Integer);
   procedure Put (I : Long_Integer);
   procedure Put (I : Integer_64);
   procedure Put (M : Money);
   --  Print to the console.

   procedure Put_Line (B : Boolean);
   procedure Put_Line (B : On_Off);
   procedure Put_Line (C : Character);
   procedure Put_Line (V : String);
   procedure Put_Line (I : Integer);
   procedure Put_Line (I : Long_Integer);
   procedure Put_Line (I : Integer_64);
   procedure Put_Line (M : Money);
   --  Print to the console then add a new line.

   procedure Animated_Delay (Delay_Seconds : Positive);
   -- Animated delay in seconds with markers each 5 and 10 seconds.
   -- ....!....|....!....|....!./ < animated wheel with /-\|/-| characters
   -- .1s !5s  |10s

   function Ansi return Boolean;
   -- Get ANSI state

   procedure Ansi_Off;
   -- Set ANSI off

   procedure Ansi_On;
   -- Set ANSI on

   function Confirm_Twice (User_Prompt_1 : String ; User_Prompt_2 : String) return Boolean;
   -- Double check by user before action. Returns True if user has validate.

   procedure New_Line (Spacing : ATI.Positive_Count := 1) renames ATI.New_Line;
   --  Add a new line to the console

   procedure Get_Immediate (C : out Character) renames Ada.Text_IO.Get_Immediate;
   --  Get a character validated by [Enter].

   function Get_Password return String;
   --  Returns a password blind typed

   procedure Pause;
   --  Displays "Press any key to continue or [Ctrl-C] to abort..." waiting for
   --  user input.

   procedure Beep;
   --  Send a beep.

   procedure Clear_Screen;
   --  Clear the screen.

   procedure Cursor_Move (X : Row; Y : Column);
   --  Move the cursor at the specified X,Y coordinates.

   procedure Cursor_Line_Forward (X : Row);
   --  Move the cursor forward X rows.

   procedure Cursor_Line_Backward (X : Row);
   --  Move the cursor backward X rows.

   procedure Cursor_Line_Erase;
   --  Erase the current line from the current cursor position to the end of
   --  the line.

   procedure Cursor_Save;
   --  Save the current cursor position.

   procedure Cursor_Restore;
   --  Restore the previous saved cursor position.

   procedure Set_Cursor (Switch : On_Off);
   --  Display or hide cursor.

   ----------------------------------------------------------------------------
   --  API - Text File
   ----------------------------------------------------------------------------

   subtype File is ATI.File_Type;

   procedure Append (Handle : in out File; Name : String);
   --  Append on an existing file.

   procedure Close (Handle : in out File) renames ATI.Close;
   --  Close a file.

   procedure Create (Handle : in out File; Name : String);
   --  Create a file.

   function End_Of_File (Handle : File) return Boolean renames ATI.End_Of_File;
   --  Test if enf of file reached.

   function End_Of_Line (Handle : File) return Boolean renames ATI.End_Of_Line;
   --  Test if end of line reached.

      procedure Flush (Handle : File) renames ATI.Flush;
   --  Flush file buffer to disk.

   procedure Get_Line (Handle : File; V : out String);
   --  Read a line then move the file pointer to the next line.

   function Is_Open (Handle : File) return Boolean renames ATI.Is_Open;
   --  Test if a file is open.

   procedure New_Line (Handle : File; Spacing : ATI.Positive_Count := 1) renames ATI.New_Line;
   --  Add a new line to a file.

   procedure Open_Conf (Handle : in out File; Name : String ; Wipe_Before_Process : Boolean := False ; Permissions : String := "");
   --  Special Open procedure for config files. Creates or Append if needed.
   --  Ensure that the complete directory tree structure exists before
   --  creating file. Creating this directory tree if needed.
   --  Allways make backup before Append. If Wipe_Before_Process is True, the
   --  file Name is backuped before beeing deleted

   procedure Open_Read (Handle : in out File; Name : String);
   --  Open a file in read mode.

   procedure Put (Handle  : File; C : Character) renames ATI.Put;
   procedure Put (Handle  : File; S : Standard.String) renames ATI.Put;
   procedure Put (Handle  : File; V : String);
   --  Write to a file.

   procedure Put_Line (Handle  : File; C : Character);
   procedure Put_Line (Handle  : File; S : Standard.String) renames ATI.Put_Line;
   procedure Put_Line (Handle  : File; V : String);
   --  Write a file and then add a new line

   function Read_File (File_Name : String) return String;
   --  Read a text file File_To_Read and returning a String buffer. LF
   --  (line feed) are preserved.

   procedure Reset (Handle : in out File) renames ATI.Reset;
   --  Reset the file pointer to the start of the file

   procedure Write_File (File_Name : String ; Content : String ; Permissions : String := "");
   --  Write a text file File_To_Write with Content. LF in content are
   --  preserved and used as line feed. Read Open_Conf documentation for
   --  implementation details.

-------------------------------------------------------------------------------
private

   Ansi_State : Boolean := True;

-------------------------------------------------------------------------------
end v22.Tio;
-------------------------------------------------------------------------------
