-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-prg.adb
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

with Ada.Calendar.Formatting;
with Ada.Numerics.Discrete_Random;
with Ada.Strings.Fixed;

with GNAT.Calendar.Time_IO; --  Where the GNU datetime formatting (%) are defined
with GNAT.Ctrl_C;

--with UXStrings.Conversions; use UXStrings.Conversions;

with v22.Fls;
with v22.Msg;
with v22.Sys;
with v22.Tio;

package body v22.Prg is

   package ACF renames Ada.Calendar.Formatting;
   package GCT renames GNAT.Calendar.Time_IO;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Check_Password (Password : String) return Boolean is
      Password_Length : Natural := Length (Password);
      Test_Character : String;
      Length_Ok, Lower_Ok, Upper_Ok, Numeric_Ok, Special_Ok : Boolean := False;
   begin
      if Password_Length >= PASSWORD_MINIMUM_LENGTH then
         Length_Ok := True;
         for I in 1 .. Password_Length loop
            Test_Character := Slice (Password, I, I);
            Msg.Debug (Test_Character);
            if Index (LETTERS_LOWER, Test_Character) > 0 then
               Msg.Debug ("Prg.Check_Password > Password has lower case character");
               Lower_Ok := True;
            elsif Index (LETTERS_UPPER, Test_Character) > 0 then
               Msg.Debug ("Prg.Check_Password > Password has upper case character");
               Upper_Ok := True;
            elsif Index (DIGITS_DECIMAL, Test_Character) > 0 then
               Msg.Debug ("Prg.Check_Password > Password has numeric character");
               Numeric_Ok := True;
            elsif Index (SPECIAL_CHARACTERS, Test_Character) > 0 then
               Msg.Debug ("Prg.Check_Password > Password has special character");
               Special_Ok := True;
            end if;
         end loop;
      else
         Msg.Debug ("Prg.Check_Password > Password " & Password & " too short");
      end if;
      return Length_Ok and Lower_Ok and Upper_Ok and Numeric_Ok and Special_Ok;
   end Check_Password;

   ----------------------------------------------------------------------------
   function Current_Time_Seconds return Natural is
   begin
      return Integer (AC.Clock - ACF.Time_Of (1970, 1, 1, 0.0));
   end Current_Time_Seconds;

   ----------------------------------------------------------------------------
   function Date return String is
   begin
      return Date_Time_Stamp_To_Date (Date_Time_Stamp);
   end Date;

   ----------------------------------------------------------------------------
   function Date_Not_Reached (DTS : String) return Boolean is
      Result : Integer := 0;
   begin
      --  0  0 00 01 11 11 11
      --  1  4 67 90 23 56 89
      --  YYYY-MM-DD HH:MM:SS

      --  Basic format checks
      if DTS.Length >= 10 and Slice (DTS, 5, 5) = "-" and Slice (DTS, 8, 8) = "-" then
         Result := Integer (ACF.Time_Of (Year_Number (To_Integer (Slice (DTS, 1, 4))),
                                        Month_Number (To_Integer (Slice (DTS, 6, 7))),
                                          Day_Number (To_Integer (Slice (DTS, 9, 10))), 0.0) - AC.Clock);
      else
         Msg.Error ("Date_Not_Reached > Input string not compliant: " & DTS);
      end if;
      return (Result > 0);
   end Date_Not_Reached;

   ----------------------------------------------------------------------------
   function Date_Time_Stamp return String is
   begin
      return From_ASCII (GCT.Image (AC.Clock, "%Y%m%d-%H%M%S"));
   end Date_Time_Stamp;

   ----------------------------------------------------------------------------
   function Date_Time_Stamp_From_ISO (DTS : String) return String is
      Result : String := "";
   begin
      --  0  0 00 01 11 11 11
      --  1  4 67 90 23 56 89
      --  YYYY-MM-DD HH:MM:SS
      if DTS.Length >= 19 then
         Result := Slice (DTS, 1, 4)   & Slice (DTS, 6, 7)   & Slice (DTS, 9, 10) & "-" &
                   Slice (DTS, 12, 13) & Slice (DTS, 15, 16) & Slice (DTS, 18, 19);
      else
         Msg.Error ("Date_Time_Stamp_From_ISO > Input string too short: " & DTS);
      end if;
      return Result;
   end Date_Time_Stamp_From_ISO;

   ----------------------------------------------------------------------------
   function Date_Time_Milli_Stamp return String is
   begin
      return From_ASCII (GCT.Image (AC.Clock, "%Y%m%d-%H%M%S.%i"));
   end Date_Time_Milli_Stamp;

   ----------------------------------------------------------------------------
   function Date_Time_Nano_Stamp return String is
   begin
      return From_ASCII (GCT.Image (AC.Clock, "%Y%m%d-%H%M%S.%o"));
   end Date_Time_Nano_Stamp;

   ----------------------------------------------------------------------------
   function Date_Time_Stamp_To_Date (DTS : String) return String is
      Result : String := "";
   begin
      --  0  00000 111111
      --  1  45678 012345
      --  YYYYMMDD-HHMMSS
      if DTS.Length >= 8 then
         Result := Slice (DTS, 1, 4) & "-" & Slice (DTS, 5, 6) & "-" & Slice (DTS, 7, 8);
      else
         Msg.Error ("Date_Time_Stamp_To_Date > Input string too short: " & DTS);
      end if;
      return Result;
   end Date_Time_Stamp_To_Date;

   ----------------------------------------------------------------------------
   function Date_Time_Stamp_To_ISO (DTS : String) return String is
      Result : String := "";
   begin
      --  0  00000 111111
      --  1  45678 012345
      --  YYYYMMDD-HHMMSS
      if DTS.Length >= 15 then
         Result := Slice (DTS, 1, 4)   & "-" & Slice (DTS, 5, 6)   & "-" & Slice (DTS, 7, 8) & " " &
                   Slice (DTS, 10, 11) & ":" & Slice (DTS, 12, 13) & ":" & Slice (DTS, 14, 15);
      else
         Msg.Error ("Date_Time_Stamp_To_ISO > Input string too short: " & DTS);
      end if;
      return Result;
   end Date_Time_Stamp_To_ISO;

   ----------------------------------------------------------------------------
   function Date_Time_Stamp_From_Local (DTS : String) return String is
      Result : String := "";
   begin
      --  00 00 0  1 11 11 11
      --  12 45 7  0 23 56 89
      --  DD?MM?YYYY HH?MM?SS
      if DTS.Length >= 19 then
         Result := Slice (DTS, 7, 10) & Slice (DTS, 4, 5) & Slice (DTS, 1, 2) & "-" &
                   Slice (DTS, 12, 13) & Slice (DTS, 15, 16) & Slice (DTS, 18, 19);
      else
         Msg.Error ("Date_Time_Stamp_From_ISO > Input string too short: " & DTS);
      end if;
      --YYYYMMDD-HHMMSS
      return Result;
   end Date_Time_Stamp_From_Local;

   ----------------------------------------------------------------------------
   function Date_Time_Stamp_To_Local (DTS : String; Sep : String := "/") return String is
      Result : String := "";
   begin
      --  0  00000 111111
      --  1  45678 012345
      --  YYYYMMDD-HHMMSS
      if DTS.Length >= 15 then
         Result := Slice (DTS, 7, 8)   & Sep & Slice (DTS, 5, 6)   & Sep & Slice (DTS, 1, 4) & " " &
                   Slice (DTS, 10, 11) & ":" & Slice (DTS, 12, 13) & ":" & Slice (DTS, 14, 15);
      else
         Msg.Error ("Date_Time_Stamp_To_Local > Input string too short: " & DTS);
      end if;
      return Result;
   end Date_Time_Stamp_To_Local;

   ----------------------------------------------------------------------------
   function Date_To_ISO (DTS : String) return String is
      Result : String := "";
   begin
      --  00 00 0001
      --  12 45 7890
      --  DD MM YYYY
      if DTS.Length >= 10 then
         Result := Slice (DTS, 7, 10) & "-" & Slice (DTS, 4, 5)  & "-" & Slice (DTS, 1, 2);
      else
         Msg.Error ("Date_To_ISO > Input string too short: " & DTS);
      end if;
      return Result;
   end Date_To_ISO;

   ----------------------------------------------------------------------------
   function Duration_Stamp (Time : Ada.Calendar.Time) return String is
      Day_Secs : Natural;
   begin
      Day_Secs := (if AC.Seconds (AC.Clock) < AC.Seconds (Time) then 86400 else 0) +
                      Integer (AC.Seconds (AC.Clock) - AC.Seconds (Time));
      --  Unlimited hours counter for long uptimes
      return To_String_Unsigned ((Day_Secs / 3600)) & "h" &
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
      return To_String_Unsigned ((Time_Seconds / 3600)) & "h" &
             Time_Format ((Time_Seconds / 60) mod 60) & "m" &
             Time_Format  (Time_Seconds mod 60) & "s";
   end Duration_Stamp_Time;

   ----------------------------------------------------------------------------
   function Generate_Password (Characters_Space : String := LETTERS_LOWER & LETTERS_UPPER & DIGITS_DECIMAL & SPECIAL_CHARACTERS;
                               Password_Length : Positive := 14) return String is
      --  Password generation with variable characters space and length. Generates passwords like: x2U5hhIKX7IJt6.
      --    Default space is [a-z] + [A-Z] + [0-9] + '_' + '-'
      --    Default length is 14
      --  Search space size:
      --    > 1,26 x 10^25
      --  Space exploration time:
      --    40000 centuries @ 100 billion tests per second.
      subtype Characters_Space_Range is Integer range 45 .. 122; -- ASCII range
      package Character_Random is new Ada.Numerics.Discrete_Random (Characters_Space_Range);
      Current : Character_Random.Generator;
      Current_Length : Integer := 0;
      Sample : String;
      Password : String := "";
   begin
      Character_Random.Reset (Current);
      loop
         Sample := From_ASCII (Character'Val (Character_Random.Random (Current)));
         if Index (Characters_Space, Sample) > 0 then
            Password := Password & Sample;
            Current_Length := Current_Length + 1;
            exit when Length (Password) = Password_Length;
         end if;
      end loop;
      return Password;
   end Generate_Password;

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
   --  task body Tracking is
   --     Delay_Value : Duration := 60.0;  -- Wait 1 minute between trackings
   --     function Image is new UXStrings.Conversions.Fixed_Point_Image (Duration);
   --  begin
   --     accept Start;
   --     Msg.Info ("Tracking armed for " & Trim_Left (Field_By_Index (Image (Delay_Value), 1, ".")) & "s cycles");
   --     loop
   --        delay Delay_Value;
   --        Prg.Tracking_Users;
   --     end loop;
   --  end Tracking;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Time_Format (Input_To_Format : Integer) return String is
      Two_Digits_Output : String;
   begin
      Two_Digits_Output := To_String_Unsigned (Input_To_Format);
      if Input_To_Format < 10 then
         Two_Digits_Output := "0" & Two_Digits_Output;
      end if;
      return Two_Digits_Output;
   end Time_Format;

   ----------------------------------------------------------------------------
   --  procedure Tracking_Users is
   --  begin
   --     Msg.Debug ("User tracking");
   --
   --     --  for C in Databases.Iterate loop
   --     --     if Databases(C).Brand = MySQL then
   --     --        Databases(C).DBM.Execute_Query ("SELECT 1");
   --     --        Msg.Info ("Ping on " & From_Latin_1 (Databases(C).Brand'Image) & " database: " & Databases(C).Name);
   --     --     elsif Databases(C).Brand = SQLite then
   --     --        --  Not applicable
   --     --        null;
   --     --     end if;
   --     --  end loop;
   --
   --  end Tracking_Users;

-------------------------------------------------------------------------------
end v22.Prg;
-------------------------------------------------------------------------------
