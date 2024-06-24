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
--  Gautier de Montmollin - gdm - krikos@bluewin.ch
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Strings.Fixed;

with UXStrings;

with v22.Msg;
with v22.Tio;

package body v22.Uxs is

   use Interfaces;

   package AS renames Ada.Strings;
   package ASF renames Ada.Strings.Fixed;

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function Char_Count (String_To_Process : String ; Char_Set_Pattern : String) return Integer is
      Result_Count : Integer := 0;
   begin
      if (String_To_Process.Length > 0) and (Char_Set_Pattern.Length > 0) then
         for I in 1 .. String_To_Process.Length loop
            for J in 1 .. Char_Set_Pattern.Length loop
               if Slice (String_To_Process, I, I) = Slice (Char_Set_Pattern, J, J) then
                  Result_Count := Result_Count + 1;
                  exit;
               end if;
            end loop;
         end loop;
      end if;
      return Result_Count;
   end Char_Count;

   ---------------------------------------------------------------------------
   function Ends_With (Item : String; Pattern : ASCII_Character) return Boolean is
   begin
      return 1 <= Item.Length and then From_Unicode (Element (Item, Item.Length)) = From_ASCII (Pattern);
   end Ends_With;

   ---------------------------------------------------------------------------
   function Field_By_Index (String_Input : String ; Index_Field : Integer ; Field_Delimiter : String) return String is
      Index_Count, Index_Next : Integer := 1;
      Result_String : String := "";
      String_To_Process : String := String_Input & Field_Delimiter;
   begin
      --  Slow algo - gnx db info = 12s
      --  Index_Count : Integer := 1;
      --  Result_String, Result_Char: String := "";
      --  String_To_Process : constant String := String_Input & Field_Delimiter;
      --  String_To_Process_Length : constant Natural := Length (String_To_Process);
      -- begin
      --  for I in 1 .. String_To_Process_Length loop
      --     Result_Char := Slice (String_To_Process, I, I);
      --     if Result_Char = Field_Delimiter then
      --        if Index_Count = Index_Field then
      --           exit;
      --        else
      --           Index_Count := Index_Count + 1;
      --           Result_String := "";
      --        end if;
      --     else
      --        Result_String := Result_String & Result_Char;
      --     end if;
      --  end loop;
      --  return Trim_Both (Result_String);

      --  Fast algo (100 x fastest) - gnx db info = 0,12s
      loop
         Index_Next := Index (String_To_Process, Field_Delimiter);
         if Index_Count = Index_Field then
            Result_String := Slice (String_To_Process, 1, Index_Next - 1);
            exit;
         else
            Index_Count := Index_Count + 1;
            String_To_Process := Slice (String_To_Process, Index_Next + Field_Delimiter.Length, String_To_Process.Length);
            if String_To_Process.Length <= Field_Delimiter.Length then -- Last field reached
               Result_String := "";
               exit;
            end if;
         end if;
      end loop;
      return Result_String;
   end Field_By_Index;

   ---------------------------------------------------------------------------
   function Field_By_Name (String_Input : String ; Field_To_Search : String ; Field_Delimiter : String) return String is
      Index_Count : Integer := 1;
      Result_String, Result_Char: String := "";
      String_To_Process : constant String := String_Input & Field_Delimiter;
   begin
      if ((not Is_Empty (String_Input)) and
          (not Is_Empty (Field_To_Search)) and
          (not Is_Empty (Field_Delimiter))
         ) then
         for I in 1 .. String_To_Process.Length loop
            Result_Char := Slice (String_To_Process, I, I);
            if Result_Char = Field_Delimiter then
               if (Index (Result_String, Field_To_Search) > 0) then
                  exit;
               else
                  Index_Count := Index_Count + 1;
                  Result_String := "";
               end if;
            else
               Result_String := Result_String & Result_Char;
            end if;
         end loop;
      end if;
      return Trim_Both (Result_String);
   end Field_By_Name ;

   ---------------------------------------------------------------------------
   function Field_Count (String_To_Process : String ; Field_Delimiter : String) return Integer is
   begin
      if Is_Empty (String_To_Process) then
         return 0;
      else
         return Char_Count (String_To_Process, Field_Delimiter) + 1;
      end if;
   end Field_Count;

   ----------------------------------------------------------------------------
   procedure Field_Display (String_To_Process : String; Column_Delimiter: String ; Row_Delimiter: String; Custom_Header : String := "") is
      Columns : constant Natural := Field_Count (Field_By_Index (String_To_Process, 1, Row_Delimiter), Column_Delimiter);
      Rows : constant Natural := Field_Count (String_To_Process, Row_Delimiter);
      Header : constant String := Custom_Header;
      Max_Width : Natural := 0;
      Current_Column : String := "";
      type T_Display is array (Integer range <>, Integer range <>) of String;
      Display : T_Display (0..Rows+1, 0..Columns+1);
   begin
      --Msg.Debug ("Columns " & To_String (Columns));
      --Msg.Debug ("Rows " & To_String (Rows));
      --  Format
      if Columns > 0 then
         for Index_Columns in 1..Columns loop
            -- Get max header width
            if Header.Length > 0 then
               Max_Width := Length (Field_By_Index (Header, Index_Columns, ","));
            end if;
            -- Get max column width
            for Index_Rows in 1..Rows loop
               Current_Column := Field_By_Index (Field_By_Index (String_To_Process, Index_Rows, Row_Delimiter), Index_Columns, Column_Delimiter);
               if Length (Current_Column) > Max_Width then
                  Max_Width := Current_Column.Length;
               end if;
            end loop;
            --Msg.Debug ("Max_Width for column " & Current_Column & ": " & To_String (Max_Width));
            --  Format header
            if Header.Length > 0 then
               Current_Column := Field_By_Index (Header, Index_Columns, ",");
               Display (0, Index_Columns) := Current_Column & (Max_Width - Current_Column.Length) * " " ;
            end if;
            -- Format column
            for Index_Rows in 1..Rows loop
               Current_Column := Field_By_Index (Field_By_Index (String_To_Process, Index_Rows, Row_Delimiter), Index_Columns, Column_Delimiter);
               Display (Index_Rows, Index_Columns) := Current_Column & (Max_Width - Current_Column.Length) * " " ;
            end loop;

         end loop;
         -- Display header
         if Header.Length > 0 then
            Max_Width := 0;
            for Index_Columns in 1..Columns loop
               Current_Column := Display (0, Index_Columns) & "  ";
               Max_Width := Max_Width + Current_Column.Length;
               Tio.Put (Current_Column);
            end loop;
            Tio.New_Line;
            Tio.Put_Line ( (Max_Width - 2) * "-");
         end if;
         -- Display rows
         for Index_Rows in 1..Rows loop
            for Index_Columns in 1..Columns loop
               Tio.Put (Display (Index_Rows, Index_Columns) & "  ");
            end loop;
            Tio.New_Line;
         end loop;
      end if;
   end Field_Display;

   ----------------------------------------------------------------------------
   function Field_Get_Data (Datas : String ; Field_Name : String) return String is
      Ascii_Code, Start, Datas_End : Natural;
      Field_Data : String := "";
   begin
      Start := Index (Datas, Field_Name);
      if Start > 0 then
         Start := Start + Field_Name.Length;
         Datas_End := Datas.Length;
         while Start <= Datas_End loop
            if From_Unicode (Datas(Start)) = CR then
               exit;
            else
               Field_Data := Field_Data & Slice (Datas, Start, Start);
            end if;
            Start := Start + 1;
         end loop;
      end if;
      return Trim_Both (Field_Data);
   end Field_Get_Data;

   ----------------------------------------------------------------------------
   function Field_Included (String_To_Process : String ; Items_List  : String ; Field_Delimiter : String) return Boolean is
      Items_Count : constant Natural := Field_Count (Items_List, Field_Delimiter);
      Items_Found : Natural := 0;
      Current_Item_Element : String;
   begin
      if (Items_Count > 0) then
         for Index_Item in 1..Items_Count loop
            Current_Item_Element := Field_By_Index (Items_List, Index_Item, Field_Delimiter);
            if Field_Search (String_To_Process, Current_Item_Element, Field_Delimiter) then
               Items_Found := Items_Found + 1;
            end if;
         end loop;
      end if;
      return (Items_Found = Items_Count);
   end Field_Included;

   ----------------------------------------------------------------------------
   function Field_Search (String_To_Process : String ; Field_To_Search : String ; Field_Delimiter : String) return Boolean is
      Result : Boolean := False;
   begin
      -- "a" should not be found in "span" so search "a & delimiter"
      if (Index (String_To_Process & Field_Delimiter, Field_To_Search & Field_Delimiter) > 0) then
         Result := True;
      end if;
      return Result;
   end Field_Search;

   ----------------------------------------------------------------------------
   function From_DB_To_Date_String (DB_Value : String; Separator : String := "/") return String is
      Result : String := DB_Value;
   begin
      --  1    6  9
      --  YYYY-MM-DD
      --  An empty DB_Value should not be an error and could signified (on purpose) an infinite expiration date.
      if DB_Value.Length >= 10 then
         Result := Slice (DB_Value, 9, 10) & Separator & Slice (DB_Value, 6, 7) & Separator & Slice (DB_Value, 1, 4);
      elsif not Is_Empty (DB_Value) then
         Msg.Debug ("Gui.From_DB_To_Date_String > DB_Value not empty but too short to be processed by 'D' modifier.");
      end if;
      return Result;
   end From_DB_To_Date_String;

   ----------------------------------------------------------------------------
   function From_DB_To_Money (DB_Value : String) return Money is
   begin
      return Money (To_Integer (DB_Value)) / 100.00;
   end From_DB_To_Money;

   ----------------------------------------------------------------------------
   function From_DB_To_Money_String (DB_Value : String) return String is
   begin
      --return From_Money (Money (To_Integer (DB_Value)) / 100.00);
      return Trim_Left (From_Latin_1 (Money'Image (Money (To_Integer (DB_Value)) / 100.00)));
   end From_DB_To_Money_String;

   ----------------------------------------------------------------------------
   function From_DB_To_Money_String_With_Padding_Sign (DB_Value : String) return String is
      Result : String := From_Latin_1 (Money'Image (Money (To_Integer (DB_Value)) / 100.00));
   begin
      return (if (Index (Result, SIGN_MINUS) = 0) then SIGN_PLUS & Result else Result);
   end From_DB_To_Money_String_With_Padding_Sign;

   --  ----------------------------------------------------------------------------
   --  function From_Money (DB_Money : Money) return Long_Long_Integer is
   --  begin
   --     return Long_Long_Integer (DB_Money * 100);
   --  end From_Money;
   --
   --  function From_Money (DB_Money : Money) return String is
   --  begin
   --     return Trim_Left (From_Latin_1 (Money'Image (DB_Money)));
   --  end From_Money;

   ----------------------------------------------------------------------------
   function From_Money_To_DB (DB_Value : Money) return String is
   begin
      return From_Money_To_DB (Trim_Left (From_Latin_1 (Money'Image (DB_Value))));
   end From_Money_To_DB;

   function From_Money_To_DB (DB_Value : String) return String is
      Integer_Part, Decimal_Part : String;
   begin
      Integer_Part := Field_By_Index (DB_Value, 1, DD);
      Decimal_Part := Field_By_Index (DB_Value, 2, DD);
      if Is_Empty (Integer_Part) or (not Is_Numeric (Integer_Part, "-")) then
         Integer_Part := "0"; -- Add digit to malformed integer part
      end if;
      if Integer_Part.Length = 1 and Slice (Integer_Part, 1,1) = "-" then
         Integer_Part := Integer_Part & "0"; -- Add digit to malformed signed integer part
      end if;
      if Is_Empty (Integer_Part) or (not Is_Numeric (Decimal_Part)) then
         Decimal_Part := "00"; -- Add digits to incomplete decimal part
      end if;
      if Decimal_Part.Length > 2 then
         Decimal_Part := Slice (Decimal_Part, 1, 2); -- Trucate to two decimal places
      else
         Decimal_Part := ((2 - Decimal_Part.Length) * "0") & Decimal_Part; -- Add a possible leading zero
      end if;
      if Integer_Part & Decimal_Part = "-000" then
         Integer_Part := Slice (Integer_Part, 2, 2); -- Trucate negative sign
      end if;
      return Integer_Part & Decimal_Part;
   end From_Money_To_DB;

   ---------------------------------------------------------------------------
   --  https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Ada
   --  function Is_Numeric (Item : in String) return Boolean is
   --     Dummy : Float;
   --  begin
   --     Dummy := Float'Value (To_ASCII (Item));
   --     return True;
   --  exception
   --     when others =>
   --        return False;
   --  end Is_Numeric;

   function Is_Numeric (Item : in String; Signs : String := "") return Boolean is
      Digits_To_Test : String := DIGITS_DECIMAL;
      Numeric_Ok : Boolean := True;
   begin
      if Is_Empty (Item) then
         Numeric_Ok := False;
      else
         for I in 1 .. Item.Length loop
            if Index (DIGITS_DECIMAL & (if I = 1 then Signs else "") , Slice (Item, I, I)) = 0 then
               Numeric_Ok := False;
               exit;
            end if;
         end loop;
      end if;
      return Numeric_Ok;
   end Is_Numeric;

   ----------------------------------------------------------------------------
   function Padding_Left (String_To_Process : String ; Padding_Character : String ; Result_Length : Positive) return String is
   begin
      if Result_Length > String_To_Process.Length then
         return ((Result_Length - String_To_Process.Length) * Padding_Character) & String_To_Process;
      else
         return String_To_Process;
      end if;
   end Padding_Left;

   ---------------------------------------------------------------------------
   function Replace_Char (String_To_Process : String ; Char_In : ASCII_Character ; Char_Out : ASCII_Character) return String is
      Result_String : String := "";
      Result_Char : ASCII_Character;
   begin
      for I in 1 .. String_To_Process.Length loop
         Result_Char := Get_ASCII (From_Unicode (Element (String_To_Process, I)),1);
         if (Result_Char = Char_In) then
            Result_Char := Char_Out;
         end if;
         Result_String := Result_String & From_ASCII (Result_Char);
      end loop;
      return Result_String;
   end Replace_Char;

   ---------------------------------------------------------------------------
   function Starts_With (Item : String; Pattern : ASCII_Character) return Boolean is
   begin
      return 1 <= Length (Item) and then From_Unicode (Element (Item, 1)) = From_ASCII (Pattern);
   end Starts_With;

   ---------------------------------------------------------------------------
   function Stript_Chars (String_To_Process : String ; Char_List : String) return String is
      Result_String, Result_Char : String := "";
   begin
      for I in 1 .. String_To_Process.Length loop
         Result_Char := Slice (String_To_Process, I, I);
         for J in 1 .. Char_List.Length loop
            if Result_Char = Slice (Char_List, J, J) then
               Result_Char := "";
               exit;
            end if;
         end loop;
         Result_String := Result_String & Result_Char;
      end loop;
      return Result_String;
   end Stript_Chars;

   ---------------------------------------------------------------------------
   function Tail_After_Match (Source : String; Pattern : String) return String is
      Result : String := "";
   begin
      if (Source.Length > 0) and (Pattern.Length > 0) then
         if Index (Source, Pattern) > 0 then
            for I in reverse 1 .. Source.Length loop
               if I <= Source.Length - Pattern.Length then
                  if Slice (Source, I, I + Pattern.Length - 1) = Pattern then
                     Result := Slice (Source, I + Pattern.Length, Source.Length);
                     exit;
                  end if;
               end if;
            end loop;
         else
            Result := Source;
         end if;
      end if;
      return Result;
   end Tail_After_Match;

   function Tail_After_Match (Source : String; Pattern : ASCII_Character) return String is
   begin
      return Tail_After_Match (Source, From_ASCII (Pattern));
   end Tail_After_Match;

   ----------------------------------------------------------------------------
   function To_Hex (Byte : Unsigned_8) return String is
      Hex_Chars : constant array (Unsigned_8 range 0 .. 15)
        of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      Half_Byte_1 : constant Unsigned_8 := Byte mod 16;
      Half_Byte_2 : constant Unsigned_8 := Byte / 16;
   begin
      return From_Latin_1 (Hex_Chars (Half_Byte_2)) & From_Latin_1 (Hex_Chars (Half_Byte_1));
   end To_Hex;

   function To_Hex (String_In : String) return String is
      String_To_Convert : Standard.String := To_Latin_1 (String_In);
      String_Converted : String := "";
   begin
      for I in 1 .. String_To_Convert'Last loop
         String_Converted := String_Converted & To_Hex (Character'Pos (String_To_Convert(I))) & " ";
      end loop;
      if String_Converted.Length >= 3 then --  Strip last trailing space
         String_Converted := Slice (String_Converted, 1, Length (String_Converted) - 1);
      end if;
      return String_Converted;
   end To_Hex;

   ---------------------------------------------------------------------------
   function To_Hex_From_Val (Input : String) return String is
      Input_Value : constant Integer := To_Integer (Input);
   begin
      if (Input_Value >= 0) and (Input_Value <= 127) then
         return To_Hex (To_String (Input_Value));
         --return To_Hex (To_String (From_ASCII (Character'Val (Input_Value)));
      else
         return "";
      end if;
   end To_Hex_From_Val;

   ---------------------------------------------------------------------------
   function To_Hex_Control_Codes (String_To_Convert : String) return String is
      Current_Char : String;
      String_Converted : String := "";
      Ascii_Code : Natural;
   begin
      for I in 1 .. String_To_Convert.Length loop
         Ascii_Code := Wide_Wide_Character'Pos (String_To_Convert(I));
         if Ascii_Code > 32 then
            String_Converted := String_Converted & Slice (String_To_Convert, I, I);
         else
            String_Converted := String_Converted & "<" & To_Hex (Slice (String_To_Convert, I, I)) & ">";
         end if;
      end loop;
      return String_Converted;
   end To_Hex_Control_Codes;

   ---------------------------------------------------------------------------
   function To_String_From_Hex (String_To_Convert : String) return String is
      String_Converted : String := "";
      Cursor : Natural := 1;
      Digit_Value_H, Digit_Value_L : Long_Integer;
   begin
      while Cursor < String_To_Convert.Length loop
         Digit_Value_H := Long_Integer (Index (DIGITS_HEXADECIMAL, Slice (String_To_Convert, Cursor, Cursor)) - 1);
         Digit_Value_L := Long_Integer (Index (DIGITS_HEXADECIMAL, Slice (String_To_Convert, Cursor+1, Cursor+1)) - 1);
         --  Invalid digit
         if (Digit_Value_H < 0) or (Digit_Value_L < 0) then
            String_Converted := "";
            exit;
         end if;
         String_Converted := String_Converted & To_String (Character'Val ((16 * Digit_Value_H) + Digit_Value_L));
         Cursor := Cursor + 2;
      end loop;

      return String_Converted;
   end To_String_From_Hex;

   ---------------------------------------------------------------------------
   function To_Long_Integer_From_Hex (Hex_Val : String) return Long_Integer is
      Hex_Value : String := Trim_Both (Hex_Val);
      Result : Long_Integer := 0;
      Digit_Value : Long_Integer;
      Radix_Offset : Natural := 0;
      Digits_List : constant String := "0123456789ABCDEF";
   begin
      for I in reverse 1..Hex_Value.Length loop
         Digit_Value := Long_Integer (Index (Digits_List, Slice (Hex_Value, I, I)) - 1);
         --  Invalid digit
         if Digit_Value < 0 then
            Result := 0;
            exit;
         end if;
         Result := Result + (16 ** Radix_Offset * Digit_Value);
         Radix_Offset := Radix_Offset + 1;
      end loop;
      return Result;
   end To_Long_Integer_From_Hex;

   ---------------------------------------------------------------------------
   function To_Integer (V : String) return Integer is
      Result : Integer := 0;
      String_To_Process : String := Trim_Both (V);
   begin
      if String_To_Process.Length > 0 then
         if Is_Numeric (String_To_Process, "-") then
            Result := Integer'Value (To_ASCII (String_To_Process));
         end if;
      end if;
      return Result;
   end To_Integer;

   ---------------------------------------------------------------------------
   function To_Long_Long_Integer (V : String) return Long_Long_Integer is
      Result : Long_Long_Integer := 0;
      String_To_Process : String := Trim_Both (V);
   begin
      if String_To_Process.Length > 0 then
         if Is_Numeric (String_To_Process, "-") then
            Result := Long_Long_Integer'Value (To_ASCII (String_To_Process));
         end if;
      end if;
      return Result;
   end To_Long_Long_Integer;

   ----------------------------------------------------------------------------
   function To_Money (DB_Value : Long_Long_Integer) return Money is
   begin
      return Money (DB_Value) / 100.00;
   end To_Money;

   function To_Money (DB_Value : String) return Money is
   begin
      return Money (To_Integer (DB_Value)) / 100.00;
   end To_Money;

   ---------------------------------------------------------------------------
   function To_String (B : Boolean) return String is
   begin
      return From_Latin_1 ((if (B) then "True" else "False"));
   end To_String;

   function To_String (B : On_Off) return String is
   begin
      return From_Latin_1 (On_Off'Image (B));
   end To_String;

   function To_String (I : Integer) return String is
   begin
      return From_UTF_8 (I'Image);
   end To_String;

   function To_String (I : Long_Integer) return String is
   begin
      return Trim_Left (From_UTF_8 (Long_Integer'Image (I))); -- Suppress the space left for positive sign
   end To_String;

   function To_String (I : Long_Long_Integer) return String is
   begin
      return Trim_Left (From_UTF_8 (Long_Long_Integer'Image (I))); -- Suppress the space left for positive sign
   end To_String;

   function To_String (F : Float) return String is
   begin
      return Trim_Left (From_UTF_8 (Float'Image (F))); -- Suppress the space left for positive sign
   end To_String;

   function To_String (M : Money) return String is
   begin
      return From_DB_To_Money_String (From_Money_To_DB (M));
      --return Trim_Left (From_Latin_1 (Money'Image (M))); -- Suppress the space left for positive sign
   end To_String;

   function To_String (C : ASCII_Character) return String is
   begin
      return From_Latin_1 ((1 => C));
   end To_String;

   function To_String_Unsigned (I : Integer) return String is
   begin
      return From_UTF_8 (I'Image).Delete (1, 1); -- Removes the sign (' ' for plus, '-' for minus)
   end To_String_Unsigned;

   function To_String_Unsigned (I : Unsigned_8) return String is
   begin
      return From_UTF_8 (I'Image).Delete (1, 1); -- Removes the sign (' ' for plus, '-' for minus)
   end To_String_Unsigned;

   function To_String_Unsigned (I : Unsigned_16) return String is
   begin
      return From_UTF_8 (I'Image).Delete (1, 1); -- Removes the sign (' ' for plus, '-' for minus)
   end To_String_Unsigned;

   function To_String_Unsigned (I : Long_Integer) return String is
   begin
      return From_UTF_8 (I'Image).Delete (1, 1); -- Removes the sign (' ' for plus, '-' for minus)
   end To_String_Unsigned;

   ---------------------------------------------------------------------------
   function To_Val (String_To_Convert : String) return String is
      String_Converted : String := "";
      Ascii_Code : Natural;
   begin
      if String_To_Convert.Length > 0 then
         for I in 1 .. String_To_Convert.Length loop
            Ascii_Code := Wide_Wide_Character'Pos (String_To_Convert(I));
            String_Converted := String_Converted & To_String (Ascii_Code) & " ";
         end loop;
      end if;
      return String_Converted;
   end To_Val;

   ---------------------------------------------------------------------------
   function Trim_Both  (Source : String) return String is
   begin
      return Trim (Source, AS.Both);
   end Trim_Both;

   ---------------------------------------------------------------------------
   function Trim_Left  (Source : String) return String is
   begin
      return Trim (Source, AS.Left);
   end Trim_Left;

   ---------------------------------------------------------------------------
   function Trim_Right (Source : String) return String is
   begin
      return Trim (Source, AS.Right);
   end Trim_Right;

   ---------------------------------------------------------------------------
   function Trim_Slashes (Source : String) return String is
      Src : String := Source;
   begin
      --  Specific case
      if Src = "/" then
         Src := "";
      end if;
      --  Suppress leading slash(es)
      while Index (Src, "/", AS.Backward) = 1 and Src.Length > 1 loop
         Src := Slice (Src, 2, Src.Length);
      end loop;
      --  Suppress trailing slash(es)
      while Index (Src, "/", AS.Backward) = Src.Length and Src.Length > 1 loop
         Src := Slice (Src, 1, Src.Length - 1);
      end loop;
      return Src;
   end Trim_Slashes;

-------------------------------------------------------------------------------
end v22.Uxs;
-------------------------------------------------------------------------------
