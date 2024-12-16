-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Interfaces;
with Ada.Exceptions;

with UXStrings; use UXStrings;

package v22 is

   ----------------------------------------------------------------------------
   --  PUBLIC TYPES
   ----------------------------------------------------------------------------

   subtype String is UXString;
   Null_String : constant String := Null_UXString;

   type Money is delta 0.01 digits 14 range -999_999_999_999.99 .. 999_999_999_999.99;
   --  Type money (14 digits including decimals)

   type On_Off is (On, Off);
   -- To replace True/False in setters functions

   ----------------------------------------------------------------------------
   --  PUBLIC CONSTANTS
   ----------------------------------------------------------------------------

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
   SQ   : constant String := "'";                    -- 39d 27h Simple quote
   BK   : constant String := "\";                    -- 92d 5Ch Backslash
   CRLF : constant String := CR & LF;

   SP   : constant String := " "; --  32d 20h Space
   VD   : constant String := ","; --  44d 2Ch Virgule (comma) delimiter
   DD   : constant String := "."; --  46d 2Eh Dot delimiter
   SD   : constant String := ":"; --  58d 3Ah Column delimiter
   SC   : constant String := ";"; --  59d 3Bh Semicolon
   AR   : constant String := "@"; --  64d 40h Email delimiter
   RD   : constant String := "\"; --  92d 5Ch Row delimiter
   CD   : constant String := "^"; --  94d 5Eh Column delimiter
   ND   : constant String := "~"; -- 126d 7Eh Name/value delimiter

   -- Signs
   SIGN_MINUS : constant String := "-";
   SIGN_PLUS : constant String := "&#x2004"; -- 2x 2009, 2007, 2004

   --  Password constants
   PASSWORD_VALIDITY : constant Positive := 10 * 60;
   PASSWORD_MINIMUM_LENGTH : constant Positive := 8;

   --  Grants role constants
   GRANTS_ROLE_NONE : constant String := "N";
   GRANTS_ROLE_ADMINISTRATOR : constant String := "A";

   GRANTS_ROLE_NONE_DISPLAY : constant String := "Sans";
   GRANTS_ROLE_ADMINISTRATOR_DISPLAY : constant String := "Administrateur";

   --  Grants rights constants
   GRANTS_RIGHTS_NONE : constant String := "N";
   GRANTS_RIGHTS_CREATE : constant String := "C";
   GRANTS_RIGHTS_READ : constant String := "R";
   GRANTS_RIGHTS_UPDATE : constant String := "U";
   GRANTS_RIGHTS_DELETE : constant String := "D";
   GRANTS_RIGHTS_PRINT : constant String := "P";
   GRANTS_RIGHTS_EXPORT : constant String := "E";
   GRANTS_RIGHTS_FULL : constant String := GRANTS_RIGHTS_CREATE &
                                           GRANTS_RIGHTS_READ &
                                           GRANTS_RIGHTS_UPDATE &
                                           GRANTS_RIGHTS_DELETE &
                                           GRANTS_RIGHTS_PRINT &
                                           GRANTS_RIGHTS_EXPORT;
   GRANTS_RIGHTS_EMAIL : constant String := "@";
   GRANTS_RIGHTS_SMS : constant String := "S";

   --  Log severity levels
   SEVERITY_NONE : String := "N";
   SEVERITY_INFO : String := "I";
   SEVERITY_WARNING : String := "W";
   SEVERITY_ERROR : String := "E";

   SEVERITY_INFO_DISPLAY : String := "Information";
   SEVERITY_WARNING_DISPLAY : String := "Avertissement";
   SEVERITY_ERROR_DISPLAY : String := "Erreur";

   --  DB modes
   DB_NONE : constant String := "N";
   DB_CREATE : constant String := "C";
   DB_READ : constant String := "R";
   DB_UPDATE : constant String := "U";
   DB_DELETE : constant String := "D";
   DB_SEARCH : constant String := "S";
   DB_FILTER : constant String := "F";
   DB_EXPORT : constant String := "E";

   --  DB tags
   DB_NO_DISPLAY : constant String := "1"; --  Special Id value used to not display a row in list

   --  DB List tags
   HC   : constant String := "#"; --  33d 23h Hide column in list
   IC   : constant String := "!"; --  35d 21h Indexed column in list

   --  Common characters enumerations
   LETTERS_LOWER : constant String := "abcdefghijklmnopqrstuvwxyz";
   LETTERS_UPPER : constant String := To_Upper (LETTERS_LOWER);
   DIGITS_DECIMAL : constant String := "0123456789";
   DIGITS_HEXADECIMAL : constant String := DIGITS_DECIMAL & "ABCDEF";
   SPECIAL_CHARACTERS : constant String := "-_";

   --  ANSI colors (ISO 6429 standard)
   CONSOLE_COLOR_GREEN  : constant String := ESC & "[1;32m";
   CONSOLE_COLOR_RED    : constant String := ESC & "[1;31m";
   CONSOLE_COLOR_YELLOW : constant String := ESC & "[1;33m";
   CONSOLE_COLOR_RESET  : constant String := ESC & "[0m";

   --  v22 exit codes
   EXIT_CODE_SUCCESS : constant Natural := 0;
   EXIT_CODE_AFTER_HELP : constant Natural := 1;
   EXIT_CODE_INVALID_PARAMETER : constant Natural := 2;
   EXIT_CODE_EXCEPTION_CTRL_C : constant Natural := 8;
   EXIT_CODE_EXCEPTION_UNEXPECTED : constant Natural := 9;

   function Get_Build return String;
   --  Returns the formatted build date stamp like:
   --  “build YYYY-mm-dd hh:mm:ss”.

   procedure Exception_Handling (Exception_Hook : Ada.Exceptions.Exception_Occurrence);
   --  Process unexpected exceptions.

   procedure Exception_Ctrl_C_Handling;
   --  Process Ctrl-C exceptions.

   procedure Finalize (Handling_Type : String := "");
   --  Finalize application, closing log file, SQL connections and restore cursor state.
   --  A handling_Type parameter is provide for Ctrl-C handling or exceptions.

   function Get_Compiler_Version return String;
   --  This function returns the version in the form "GNAT v.vvx (yyyyddmm)" where
   --  v.vv is the main version number (e.g. 3.16), x is the version
   --  designator (e.g. a1 in 3.16a1), and yyyyddmm is the date in ISO form.

   --  An example of the returned value would be "3.16w (20021029)". The
   --  version is actually that of the binder used to bind the program,
   --  which will be the same as the compiler version if a consistent
   --  set of tools is used to build the program.

   --  this unit is only useable if the main program is written in Ada.

   function Get_Log_Dir return String;
   --  Returns the log directory

   function Get_Tmp_Dir return String;
   --  Returns the temporary files directory

   function Get_Version return String;
   --  Returns the Library name and formatted version like:
   --  “v22 v.minor.major”.

   procedure Raise_Exception;
   --  Raise an exception for reporting test and <program_Name.err> file
   --  creation. In addition to the usual trace, a v20 exception give some
   --  extra information like : exception time, program uptime, program &
   --  library names & versions, start & home directories and Ada and all
   --  languages memory allocation, current & maximum (peak) values.

   ----------------------------------------------------------------------------
   protected type Msg_Mutex_T is
      entry Lock;
      procedure Unlock;
   end Msg_Mutex_T;
   Msg_Mutex : Msg_Mutex_T;

   protected type Sql_Mutex_T is
      entry Lock;
      procedure Unlock;
   end Sql_Mutex_T;
   Sql_Mutex : Sql_Mutex_T;

-------------------------------------------------------------------------------
private

   Msg_Mutex_Owned : Boolean := False;
   Sql_Mutex_Owned : Boolean := False;

   Name : constant String := "v22";
   --  Library's name

   Version_Major : constant Natural := 0;
   --  Library major version number

   Version_Minor : constant Natural := 6;
   --  Library minor version number

   --  135 cols width is the max full screen standard console on a rather old,
   --  but so good, Dell UltraSharp 1907Fp 1280x1024 4:3 monitor
   --  92 cols width is the max length useable in the "Listing 7" paragraph
   --  style of AIDE Manual with B612 font.
   --  79 is the standard width.
   --  Definitely 512 to display erroneous long SQL queries ;>
   --  Definitely 1024 to display long buffers ;>

   Line_Max_Length : constant Natural := 1024;
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
