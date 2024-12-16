-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-prg.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Program package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Calendar; use Ada.Calendar; -- Use for operation on Ada.Calendar.Time
with Ada.Command_Line;
with Ada.Directories;

with v22.Uxs; use v22.Uxs;

package v22.Prg is

   package AC renames Ada.Calendar;

   type Time_UTC_Local is (UTC, Local);

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   function Check_Password (Password : String) return Boolean;
   --  Check password strength. A password must have at least contains uppercase, lowercase, digit
   --  and one special char among "-" and "_".

   Command : constant String := From_Latin_1 (Ada.Command_Line.Command_Name);
   --  Constant storing program command (Arg 0).

   function Current_Time_Seconds return Natural;
   --  Returns a duration as seconds since ISO date 197001010. Conforms to
   --  Unix time standard. Checked with console command "date +%s". Valid
   --  algorithm until 2070.

   function Date return String;
   --  Returns current date as YYYY-MM-DD.

   function Date_Not_Reached (DTS : String) return Boolean;
   --  Returns true if YYYY-MM-DD Date_ISO is still to come.

   function Date_Time_Stamp_Reached (DTS : String ; Zone : Time_UTC_Local := Local) return Boolean;
   --  Returns true if YYYYMMDD:HHMMSS Date_Time_Stamp has passed.

   function Date_Time_Stamp (Zone : Time_UTC_Local := Local) return String;
   --  Returns current datetime stamp as YYYYMMDD-HHMMSS.

   function Date_Time_Stamp_From_ISO (DTS : String) return String;
   --  Returns “YYYYMMDD-HHMMSS” from a date time stamp as “YYYY-MM-DD HH:MM:SS”.

   function Date_Time_Milli_Stamp return String;
   --  Returns current datetime stamp with trailing milliseconds as: YYYYMMDD-HHMMSS.NNN.
   --  Used in v22.Msg logs

   function Date_Time_Nano_Stamp return String;
   --  Returns current datetime stamp with trailing nanoseconds as: YYYYMMDD-HHMMSS.NNNNNNNNN.
   --  Useful for precise timings or unique identifier building with date indication.

   function Date_Time_Stamp_To_Date (DTS : String) return String;
   --  Returns YYYY-MM-DD from a datetime stamp as YYYYMMDD-HHMMSS.

   function Date_Time_Stamp_To_ISO (DTS : String) return String;
   --  Returns “YYYY-MM-DD HH:MM:SS” from a date time stamp as “YYYYMMDD-HHMMSS”.

   function Date_Time_Stamp_From_Local (DTS : String) return String;
   --  Returns a date time stamp “YYYYMMDD-HHMMSS” from a local “DD?MM?YYYY HH?MM?SS” (? is any character).

   function Date_Time_Stamp_To_Local (DTS : String; Sep : String := "/") return String;
   --  Returns “DD/MM/YYYY HH:MM:SS” from a date time stamp as “YYYYMMDD-HHMMSS” with a customizable date separator.

   function Date_To_ISO (DTS : String) return String;
   --  Returns “YYYY-MM-DD” from a string date as “DD?MM?YYYY” (? is any character).

   function Duration_Stamp (Time : Ada.Calendar.Time) return String;
   --  Returns a duration as HHhMMmSSs since Time.

   function Duration_Stamp_Seconds (Time : Ada.Calendar.Time) return Natural;
   --  Returns a duration as seconds since Time.

   function Duration_Stamp_Time (Time_Seconds : Integer) return String;
   --  Returns a formatted HHhMMmSSs String from Time_Seconds

   function Generate_Password (Characters_Space : String := LETTERS_LOWER & LETTERS_UPPER & DIGITS_DECIMAL & SPECIAL_CHARACTERS;
                               Password_Length : Positive := 14) return String;
   --  Password generation with variable characters space and length. Generates passwords like: x2U5hhIKX7IJt6.
   --  Default space is [a-z] + [A-Z] + [0-9] + '_' + '-', default length 14. Search space size > 1,26 x 10^25

   function Get_Version return String;
   --  Returns formatted program version :
   --  “<space>v.minor.major”.

   function Get_Version_Major return Natural;
   --  Returns major program version

   function Get_Version_Minor return Natural;
   --  Returns minor program version

   function Is_User_Not_Root return Boolean;
   --  Returns True if program user's not root.

   function Name return String;
   --  Program name.

   function Path return String;
   --  Program path.

   procedure Set_Handler_Ctrl_C (Switch : On_Off);
   --  Activate Ctrl-C interrupt handler. If Switch is On, Ctrl-C is activated and,
   --  when pressed, application is properly finalize. If Switch is Off, Ctrl-C is
   --  inhibited and application continue.

   function Get_Handler_Ctrl_C return On_Off;
   --  Get Ctrl-C handling status. If returns On, Ctrl-C is activated and,
   --  when pressed, application is properly finalize. If returns Off, Ctrl-C is
   --  inhibited and application continue.

   procedure Set_Exit_Status (Code : Natural);
   --  Set errorlevel return code. Each call is cumulative. Four calls with
   --  1, 2, 4 and 8 set 15 ie msb-00001111-lsb. Can be used everywhere in
   --  the program without special call at its end. Convention : 1 = no or bad
   --  command, 128 = runtime exception (8th bit).

   procedure Set_Version (Major : Natural; Minor : Natural);
   --  Set program version.

   Start_Dir : constant String := From_Latin_1 (Ada.Directories.Current_Directory);
   --  Constant storing current directory at start.

   Start_Time : constant AC.Time := AC.Clock;
   --  Constant storing Time at program start

   --  task Tracking is
   --     entry Start;
   --  end Tracking;
   --  Connection_Tracking.Start launch task to periodically track user connection.
   --  It currently manages connection times, but will be extended to other uses.

-------------------------------------------------------------------------------
private

   function Time_Format (Input_To_Format : Integer) return String;

   Version_Major : Natural := 0;
   Version_Minor : Natural := 0;

   Exit_Status : Natural := 0;
   Ctrl_C_Status : On_Off := On;

   --  procedure Tracking_Users;
   --  --  Trascking users connections times.

-------------------------------------------------------------------------------
end v22.Prg;
-------------------------------------------------------------------------------
