
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
--  Gautier de Montmollin - gdm - krikos@bluewin.ch
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Interfaces;

package v22.Uxs is

   package ACH renames Ada.Characters.Handling;

   subtype Unsigned8 is Interfaces.Unsigned_8;

   function Char_Count (String_To_Process : String ; Char_Set_Pattern : String) return Integer;
   --  Count each char in String_To_Process relative to Char_Set_Pattern.

   function Empty (Source : String) return Boolean;
   --  Return True if String is empty.

   function Ends_With (Item : String; Pattern : ASCII_Character) return Boolean;
   function Ends_With (Item : String; Pattern : String) return Boolean;
   --  Check if String Item ends with another String or String Pattern.

   --  Fields processing
   --  /!\ Use only Field_Delimiter characters between 0dec and 127dec, due to
   --  /!\ some keyboard available characters encoding with 2 chars. Some
   --  /!\ recommended keyboards Field_Delimiter characters are listed in
   --  /!\ v20.ads but also in the v20 documentation

   function Field_By_Index (String_Input : String ; Index_Field : Integer ; Field_Delimiter : String) return String;
   --  Return a field indexed by Index_Field and delimited by Field_Delimiter
   --  from String_To_Process.

   function Field_By_Name (String_Input : String ; Field_To_Search : String ; Field_Delimiter : String) return String;
   --  Return a field from a search string and delimited by Field_Delimiter.

   function Field_Count (String_To_Process : String ; Field_Delimiter : String) return Integer;
   --  Count fields in String_To_Process and return fields number.

   procedure Field_Display (String_To_Process : String; Column_Delimiter : String; Row_Delimiter : String; Custom_Header : String := "");
   --  Formatted display of a string fields structured in rows and columns

   function Field_Included (String_To_Process : String ; Items_List  : String ; Field_Delimiter : String) return Boolean;
   --  Returns True if all Items_List are included in String_To_Process list, which is delimited by Field_Delimiter.

   function Field_Search (String_To_Process : String ; Field_To_Search : String ; Field_Delimiter : String) return Boolean;
   --  Search Field_To_Search in String_To_Process and return True if found.

   function Is_Numeric (Item : in String) return Boolean;
   --  Return True if Item string contains only numeric

   function Replace_Char (String_To_Process : String ; Char_In : ASCII_Character ; Char_Out : ASCII_Character) return String;
   --  Replace all Char_In by Char_Out in String_To_Process

   function Replace_Pattern (String_To_Process : String ; Pattern_In : String ; Pattern_Out : String) return String;
   -- Replace Pattern_In by Pattern_Out in String_To_Process. Returns a
   -- String with Pattern_In replaced by Pattern_Out

   function Starts_With (Item : String; Pattern : ASCII_Character) return Boolean;
   function Starts_With (Item : String; Pattern : String) return Boolean;
   --  Check if String Item starts with another String or String Pattern.

   function Stript_Chars (String_To_Process : String ; Char_List : String) return String;
   --  Stript each char in String_To_Process relative to Char_List

   function Tail_After_Match (Source : String; Pattern : ASCII_Character) return String;
   function Tail_After_Match (Source : String; Pattern : String) return String;
   --  Extract a String from Source starting from Pattern+1 position to the
   --  end. Returns a String
   --
   --  Examples : Tail_After_Match (+"/etc/genesix/gnx-startup",
   --    "/")) returns "gnx-startup"
   --    "ix")) returns "/gnx-startup"
   --    "gene")) returns "six/gnx-startup"
   --    "etc/genesix/gnx-startu")) returns "p"
   --    "/etc/genesix/gnx-startu")) returns "p"
   --    "/etc/genesix/gnx-startup")) returns empty string
   --    "/etc/genesix/gnx-startupp")) returns empty string

   function To_Hex (Byte : Interfaces.Unsigned_8) return String;
   --  Convert a Byte to a String hexadecimal output.

   function To_Hex (String_To_Convert : String) return String;
   -- Convert a String to a String hexadecimal formatted output.

   function To_Hex_From_Val (Input : String) return String;
   --  Convert an ASCII String value ranging 0..127 to a String hexadecimal
   --  output. For example, with input is "61" dec, result is "3D" hex.

   function To_Integer (V : String) return Integer;
   --  Convert a String or String to an Integer.

   function To_String (B : Boolean) return String;
   function To_String (I : Integer) return String;
   function To_String (I : Long_Integer) return String;
   function To_String (C : ASCII_Character) return String;
   --  Convert a Boolean, a Integer, a Long Integer or a Char into String type.

   function To_Val (String_To_Convert : String) return String;
   -- Convert a String to String ASCII decimal values formatted output.

   function Trim_Both  (Source : String) return String;
   --  Returns an all trimmed spaces String of String Source.

   function Trim_Left (Source : String) return String;
   --  Returns a trimmed leading spaces String of String Source.

   function Trim_Right (Source : String) return String;
   --  Returns a trimmed trailing spaces String of String Source.

   function Trim_Slashes (Source : String) return String;
   --  Returns an all trimmed slahes String of String Source.
   --
   --  Examples : Trim_Slashes (
   --    "/") returns ""
   --    "i") returns "i"
   --    "/i") returns "i"
   --    "//////i/////") returns "i"

   --  function To_Lower (Item : Character) return Character renames ACH.To_Lower;
   --  --  Convert a Character or a String to lower case.
   --
   --  function To_Upper (Item : Character) return Character renames ACH.To_Upper;
   --  --  Convert a Character or a String to upper case.

------------------------------------------------------------------------------
end v22.Uxs;
------------------------------------------------------------------------------
