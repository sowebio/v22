-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-prg.adb
--  @copyright See authors list below and v22.copyrights file
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

with Ada.Calendar.Formatting;
with Ada.Strings.Fixed;

with GNAT.Calendar.Time_IO;
with GNAT.Ctrl_C;

with v22.Msg;
with v22.Sys;

package body v22.Prg is

   package ACF renames Ada.Calendar.Formatting;
   package GCT renames GNAT.Calendar.Time_IO;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Current_Time_Seconds return Natural is
   begin
      return Integer (AC.Clock - ACF.Time_Of (1970, 1, 1, 0.0));
   end Current_Time_Seconds;

   ----------------------------------------------------------------------------
   function Duration_Stamp (Time : Ada.Calendar.Time) return String is
      Day_Secs : Natural;
   begin
      Day_Secs := (if AC.Seconds (AC.Clock) < AC.Seconds (Time) then 86400 else 0) +
                      Integer (AC.Seconds (AC.Clock) - AC.Seconds (Time));
      --  Unlimited hours counter for long uptimes
      return To_String  ((Day_Secs / 3600)) & "h" &
             Time_Format ((Day_Secs / 60) mod 60) & "m" &
             Time_Format  (Day_Secs mod 60) & "s";
   end Duration_Stamp;

   ----------------------------------------------------------------------------
   function Duration_Stamp_Seconds (Time : Ada.Calendar.Time) return Natural is
   begin
      return (if AC.Seconds (AC.Clock) < AC.Seconds (Time) then 86400 else 0) +
                      Integer (AC.Seconds (AC.Clock) - AC.Seconds (Time));
   end Duration_Stamp_Seconds;

   ----------------------------------------------------------------------------
   function Duration_Stamp_Time (Time_Seconds : Integer) return String is
   begin
      --  Unlimited hours counter for long uptimes
      return To_String  ((Time_Seconds / 3600)) & "h" &
             Time_Format ((Time_Seconds / 60) mod 60) & "m" &
             Time_Format  (Time_Seconds mod 60) & "s";
   end Duration_Stamp_Time;

   ----------------------------------------------------------------------------
   function Generate_Password return String is
      --  Sowebio password generation with 64 chars:
      --    ([A-Z] + [a-z] + [0-9] + '_' + '-')
      --  Search space size:
      --    > 1,26 x 10^25
      --  Space exploration time:
      --    40000 centuries @ 100 billion tests per second.
      --  Command line with standard tools:
      --    < /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-14};echo;
      --  Generates 14 chars long passwords like:
      --    x2U5hhIKX7IJt6
      SE_Result : Integer := 0;
      SE_Output : String := "";
   begin
      Sys.Shell_Execute ("< /dev/urandom tr -dc _A-Z-a-z-0-9 | head -c${1:-14};echo;", SE_Result, SE_Output);
      if (SE_Result /= 0) then
          Msg.Error ("v22.Prg.Generate_Password > Password generation command failed");
      end if;
      return SE_Output;
   end Generate_Password;

   --  function Get_Version return String is
   --  begin
   --     return (Name & " v" &
   --             Ada.Strings.Fixed.Trim (Natural'Image (Version_Major),
   --             Ada.Strings.Left) & "." &
   --             Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor),
   --         Ada.Strings.Left));
   --  end Get_Version;

   ----------------------------------------------------------------------------
   function Get_Handler_Ctrl_C return On_Off is
   begin

   return Ctrl_C_Status;
   end Get_Handler_Ctrl_C;

   ----------------------------------------------------------------------------
   function Get_Version return String is
   begin
      return (Name & " v" &
              From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Major), Ada.Strings.Left)) & "." &
              From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor), Ada.Strings.Left)));
   end Get_Version;

   ----------------------------------------------------------------------------
   function Get_Version_Major return Natural is
   begin
      return Version_Major;
   end Get_Version_Major;

   ----------------------------------------------------------------------------
   function Get_Version_Minor return Natural is
   begin
      return Version_Minor;
   end Get_Version_Minor;

   ----------------------------------------------------------------------------
   function Is_User_Not_Root return Boolean is
   begin
      --  Determining if the current user is root is not so simple. Indeed, USER
      --  and USERNAME can contain a "root" user who is not named "root" (juste
      --  declare another user with a UID of 0). Two solutions are possible:
      --  - Use the HOME environment variable, which allows to determine if the
      --    user's home is "root".
      --  - Use the command "whoami" which will return "root", even if the user
      --  "root" has another name.
      return (not (To_Latin_1 (Sys.Get_Env ("HOME")) = "/root"));

    --  return (not (Sys.Get_Env ("HOME") = "/root"));
    --  +===========================GNAT BUG DETECTED==============================+
    --  | 11.2.0 (x86_64-pc-linux-gnu) Storage_Error stack overflow or erroneous memory access|
    --  | Error detected at v22-prg.adb:123:41                                     |
    --  | Please submit a bug report; see https://gcc.gnu.org/bugs/ .              |
    --  | Use a subject line meaningful to you and us to track the bug.            |
    --  | Include the entire contents of this bug box in the report.               |
    --  | Include the exact command that you entered.                              |
    --  | Also include sources listed below.                                       |
    --  +==========================================================================+

   end Is_User_Not_Root;

   ----------------------------------------------------------------------------
   function Name return String is
      Result : String := Tail_After_Match (Command, "/");
   begin
      if (Length (Result) = 0) then
         Result := Command;
      end if;
     return Result;
   end Name;

   ----------------------------------------------------------------------------
   function Path return String is
      Result_Code : Natural := 0;
      Result_String : String := "";
   begin
      Sys.Shell_Execute ("pwd", Result_Code, Result_String);
      -- pwd at root returns /
      -- pwn elsewhere returns a path without a trailing slash
      if (Slice (Result_String, Length (Result_String), Length (Result_String)) /= "/") then
         Result_String := Result_String & "/";
      end if;
      return Result_String;
   end Path;

   ----------------------------------------------------------------------------
   procedure Set_Handler_Ctrl_C (Switch : On_Off) is
   begin
      GNAT.Ctrl_C.Install_Handler (Handler => Exception_Ctrl_C_Handling'Access);
      Ctrl_C_Status := Switch;
   end Set_Handler_Ctrl_C;

   ----------------------------------------------------------------------------
   procedure Set_Exit_Status (Code : Natural) is
   begin
      Exit_Status := Exit_Status + Code;
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Exit_Status (Exit_Status));
   end Set_Exit_Status;

   ----------------------------------------------------------------------------
   procedure Set_Version (Major : Natural; Minor : Natural) is
   begin
      Version_Major := Major;
      Version_Minor := Minor;
   end Set_Version;

   ----------------------------------------------------------------------------
   function Time_Format (Input_To_Format : Integer) return String is
      Two_Digits_Output : String;
   begin
      Two_Digits_Output := To_String (Input_To_Format);
      if Input_To_Format < 10 then
         Two_Digits_Output := "0" & Two_Digits_Output;
      end if;
      return Two_Digits_Output;
   end Time_Format;

   ----------------------------------------------------------------------------
   function Time_Stamp return String is
   begin
      return From_ASCII (GCT.Image (AC.Clock, "%Y%m%d-%H%M%S"));
   end Time_Stamp;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

-------------------------------------------------------------------------------
end v22.Prg;
-------------------------------------------------------------------------------
