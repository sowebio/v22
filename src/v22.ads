-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework
--
--  @description
--
--  @authors
--  Théodore Gigault - tg - developpement@soweb.io
--  Arthur Le Floch - alf - developpement@soweb.io
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Interfaces;
with Ada.Exceptions;
with UXStrings;

package v22 is

   use UXStrings;
   subtype String is UXString;
   Null_String : constant String := Null_UXString;

   type Money is delta 0.01 digits 14 range -999_999_999_999.99 .. 999_999_999_999.99;
   -- Type money 1_000_000_000_000.00

   type Unsigned_Integer8 is mod 256;
   --subtype Integer_64 is Interfaces.Integer_64;

   --  Redirection constants
   STD_OUT_REDIRECT  : constant String := " 1>/dev/null";
   ERR_OUT_REDIRECT  : constant String := " 2>/dev/null";
   STD_ERR_OUT_REDIRECT : constant String := " 2>/dev/null 1>/dev/null";

   -- Flag files
   ACCESS_OK : constant String := "access_ok_dont_delete_this_file";
   INSTALL_OK : constant String := "install_ok_dont_delete_this_file";

   --  String constants
   HT   : constant String := From_ASCII (ASCII.HT);  -- 09d 09h Tab
   LF   : constant String := From_ASCII (ASCII.LF);  -- 10d 0Ah Line Feed
   CR   : constant String := From_ASCII (ASCII.CR);  -- 13d 0Dh Carriage return
   ESC  : constant String := From_ASCII (ASCII.ESC); -- 27d 1Bh Escape
   DQ   : constant String := From_ASCII ('"');       -- 34d 22h Double quote
   CRLF : constant String := CR & LF;

   ND   : constant String := "~"; -- 126d 7Eh Name/value delimiter
   CD   : constant String := "^"; --  94d 5Eh Column delimiter
   RD   : constant String := "\"; --  92d 5Ch Row delimiter
   VD   : constant String := ","; --  44d 2Ch Virgule (comma) delimiter
   DD   : constant String := ","; --  46d 2Eh Dot delimiter
   SD   : constant String := ":"; --  58d 3Ah Colon delimiter
   SP   : constant String := " "; --  32d 20h Space

   -- ANSI colors (ISO 6429 standard)
   CONSOLE_COLOR_GREEN  : constant String := ESC & "[1;32m";
   CONSOLE_COLOR_RED    : constant String := ESC & "[1;31m";
   CONSOLE_COLOR_YELLOW : constant String := ESC & "[1;33m";
   CONSOLE_COLOR_RESET  : constant String := ESC & "[0m";

   function Get_Version return String;
   --  Returns the Library name and formatted version like:
   --  “v22 v.minor.major”.

   function Get_Build return String;
   --  Returns the formatted build date stamp like:
   --  “build YYYY-mm-dd hh:mm:ss”.

   function Get_Log_Dir return String;
   --  Returns the log directory

   function Get_Tmp_Dir return String;
   --  Returns the temporary files directory

   procedure Raise_Exception;
   --  Raise an exception for reporting test and <program_Name.err> file
   --  creation. In addition to the usual trace, a v20 exception give some
   --  extra information like : exception time, program uptime, program &
   --  library names & versions, start & home directories and Ada and all
   --  languages memory allocation, current & maximum (peak) values.

   procedure Exception_Handling
   (Exception_Hook : Ada.Exceptions.Exception_Occurrence);
   --  Process exceptions.

-------------------------------------------------------------------------------
private

   Name : constant String := "v22";
   --  Library's name

   Version_Major : constant Natural := 0;
   --  Library major version number

   Version_Minor : constant Natural := 1;
   --  Library minor version number

   --  135 cols width is the max full screen standard console on a rather old,
   --  but so good, Dell UltraSharp 1907Fp 1280x1024 4:3 monitor
   --  92 cols width is the max length useable in the "Listing 7" paragraph
   --  style of AIDE Manual with B612 font.
   --  79 is the standard width.

   Line_Max_Length : constant Natural := 255;
   Title_Max_Length : constant Natural := 85;
   --  Maximum line length for exceptions (.err) and log reports (.log)

   Log_Dir : constant String := "/var/log/";
   --  Log directory

   Tmp_Dir : constant String := "/tmp/";
   --  Temporary directory

   Errorlevel : Natural := 0;

------------------------------------------------------------------------------
end v22;
------------------------------------------------------------------------------
