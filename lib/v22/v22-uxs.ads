
-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22.ads
--  @copyright See authors list below and vREADME.md file
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

with Interfaces; use Interfaces;

with Ada.Characters.Handling;
with Ada.Strings.Unbounded;

package v22.Uxs is

   package ACH renames Ada.Characters.Handling;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   function Char_Count (String_To_Process : String ; Char_Set_Pattern : String) return Integer;
   --  Count each char in String_To_Process relative to Char_Set_Pattern.

   function Ends_With (Item : String; Pattern : ASCII_Character) return Boolean;
   --  Check if String Item ends with another String or String Pattern.

   function Field_By_Index (String_Input : String ; Index_Field : Integer ; Field_Delimiter : String) return String;
   --  Return a field indexed by Index_Field and delimited by Field_Delimiter
   --  from String_To_Process.

   function Field_By_Name (String_Input : String ; Field_To_Search : String ; Field_Delimiter : String) return String;
   --  Return a field from a search string and delimited by Field_Delimiter.

   function Field_Count (String_To_Process : String ; Field_Delimiter : String) return Integer;
   --  Count fields in String_To_Process and return fields number.

   procedure Field_Display (String_To_Process : String; Column_Delimiter : String; Row_Delimiter : String; Custom_Header : String := "");
   --  Formatted display of a string fields structured in rows and columns

   function Field_Get_Data (Datas : String ; Field_Name : String) return String;
   --  Return space trimmed datas from Field_Name identifier in Datas or an empty string if  Field_Name not found.

   function Field_Included (String_To_Process : String ; Items_List  : String ; Field_Delimiter : String) return Boolean;
   --  Returns True if all Items_List are included in String_To_Process list, which is delimited by Field_Delimiter.

   function Field_Search (String_To_Process : String ; Field_To_Search : String ; Field_Delimiter : String) return Boolean;
   --  Search Field_To_Search in String_To_Process and return True if found.

   function From_DB_To_Date_String (DB_Value : String; Separator : String := "/") return String;
   --  Converts a ISO 8601 YYYY-MM-DD string to DD/MM/YYYY string with optional separator replacement.

   function From_DB_To_Money (DB_Value : String) return Money;
   --  Converts a String (as an image of type Bigint in database) into a Money type.

   function From_DB_To_Money_String (DB_Value : String) return String;
   --  Converts a string (as an image of  type Bigint in database) into a string (as an image of type Money).
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.
   --
   --  Examples:
   --   000   =>    0.00
   --   001   =>    0.01
   --   012   =>    0.12
   --   12312 =>  123.12
   --  -001   =>   -0.01
   --  -012   =>   -0.12
   --  -12312 => -123.12

   function From_DB_To_Money_String_With_Padding_Sign (DB_Value : String) return String;
   --  Converts a string (as an image of  type Bigint in database) into a string (as an image of type Money).
   --  With positive and invisible padding sign to keep vertical alignment.
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.

   --  function From_Money (DB_Money : Money) return Long_Long_Integer;
   --  function From_Money (DB_Money : Money) return String;
   --  --  Converts Money to Bigint database storage.
   --  --  Converts Money to String.
   --  --  Bigint (Long_Long_Integer) is used for accurate storage in database.

   function From_Money_To_DB (DB_Value : Money) return String;
   function From_Money_To_DB (DB_Value : String) return String;
   --  Converts a String (as an image of type Money) into a String compatible with storage as a Bigint in database.
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.
   --
   --  Examples:
   --  123        =>  12300
   --  123.       =>  12300
   --  123.1      =>  12301
   --  123.12     =>  12312
   --  123.123456 =>  12312
   --     .1      =>  001
   --     .12     =>  012
   --     .123456 =>  012
   --   -0.123456 => -012
   --    -.123456 => -012
   --    -.       =>  000
   --    -        =>  000

   function Is_Numeric (Item : in String; Signs : String := "") return Boolean;
   --  Returns True if Item string is numeric (i.e. contains only digits with or without leading signs like space, plus ou minus).

   function Padding_Left (String_To_Process : String ; Padding_Character : String ; Result_Length : Positive) return String;
   --  Left padding untill Result_Length a String_To_Process with a Padding_Character.

   function Replace_Char (String_To_Process : String ; Char_In : ASCII_Character ; Char_Out : ASCII_Character) return String;
   --  Replace all Char_In by Char_Out in String_To_Process

   function Starts_With (Item : String; Pattern : ASCII_Character) return Boolean;
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

   function To_Hex (Byte : Unsigned_8) return String;
   --  Convert a Byte to a String hexadecimal output.

   function To_Hex (String_In : String) return String;
   -- Convert a String to a String hexadecimal formatted output.

   function To_Hex_From_Val (Input : String) return String;
   --  Convert an ASCII String value ranging 0..127 to a String hexadecimal
   --  output. For example, with input is "61" dec, result is "3D" hex.

   function To_Hex_Control_Codes (String_To_Convert : String) return String;
   --  Convert any ASCII character value ranging 0..32 to a hexadecimal output but leave others characters unchanged.

   function To_Integer (V : String) return Integer;
   --  Convert a String to an Integer.
   --  Leading and trailing spaces are trimmed before conversion.
   --  Returns 0 if String is empty or contains a least one non numeric character.

   function To_Long_Long_Integer (V : String) return Long_Long_Integer;
   --  Convert a String to a Long_Long_Integer.
   --  Leading and trailing spaces are trimmed before conversion.
   --  Returns 0 if String is empty or contains a least one non numeric character.

   function To_Long_Integer_From_Hex (Hex_Val : String) return Long_Integer;
   --  Convert a hexadecimal string to a Long_Integer. Leading and trailing spaces are trimmed
   --  before conversion. Returns 0 if String is empty or contains invalid character.

   function To_Money (DB_Value : Long_Long_Integer) return Money;
   function To_Money (DB_Value : String) return Money;
   --  Convert Bigint database storage (Long_Long_Integer) or String to Money type.
   --  Bigint (Long_Long_Integer) is used for accurate storage in database.

   function To_String (B : Boolean) return String;
   function To_String (B : On_Off) return String;
   function To_String (I : Integer) return String;
   function To_String (I : Long_Integer) return String;
   function To_String (I : Long_Long_Integer) return String;
   function To_String (F : Float) return String;
   function To_String (M : Money) return String;
   function To_String (C : ASCII_Character) return String;
   --  Convert a Boolean, an On_Off type, an Integer, a Long Integer,
   --  a Long Long Integer, a Money type or a Char into String type.

   function To_String_From_Hex (String_To_Convert : String) return String;
   --  Convert a hexadecimal string to an ASCII String. Leading and trailing spaces are trimmed
   --  before conversion. Returns 0 if String is empty or contains invalid character.

   function To_String_Unsigned (I : Integer) return String;
   function To_String_Unsigned (I : Unsigned_8) return String;
   function To_String_Unsigned (I : Unsigned_16) return String;
   function To_String_Unsigned (I : Long_Integer) return String;
   --  Convert an Integer into String type removing the sign, i.e
   --  ' ' space for plus and '-' for minus

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

------------------------------------------------------------------------------
end v22.Uxs;
------------------------------------------------------------------------------
