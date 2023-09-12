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

with Ada.Strings.Fixed;

with UXStrings;

with v22.Msg;
with v22.Tio;

package body v22.Uxs is

   use Interfaces;

   package AS renames Ada.Strings;
   package ASF renames Ada.Strings.Fixed;

   ----------------------------------------------------------------------------
   function Char_Count (String_To_Process : String ; Char_Set_Pattern : String) return Integer is
      Result_Count : Integer := 0;
      String_To_Process_Length : constant Natural := Length (String_To_Process);
      Char_Set_Pattern_Length : constant Natural := Length (Char_Set_Pattern);
   begin
      if (String_To_Process_Length > 0) and (Char_Set_Pattern_Length > 0) then
         for I in 1 .. String_To_Process_Length loop
            for J in 1 .. Char_Set_Pattern_Length loop
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
   function Empty (Source : String) return Boolean is
   begin
      return (Length (Source) = 0);
   end Empty;

   ---------------------------------------------------------------------------
   function Ends_With (Item : String; Pattern : ASCII_Character) return Boolean is
   begin
      return 1 <= Length (Item) and
      then From_Unicode (Element (Item, Length (Item))) =  From_ASCII (Pattern);
   end Ends_With;

   function Ends_With (Item : String; Pattern : String) return Boolean is
   begin
      return Length (Pattern) <= Length (Item) and then Tail (Item, Length (Pattern)) = Pattern;
   end Ends_With;

   ---------------------------------------------------------------------------
   function Field_By_Index (String_Input : String ; Index_Field : Integer ; Field_Delimiter : String) return String is
      Index_Count, Index_Next : Integer := 1;
      Result_String : String := "";
      String_To_Process : String := String_Input & Field_Delimiter;
      Length_Delimiter : Natural := Length (Field_Delimiter);
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
      while True loop
         Index_Next := Index (String_To_Process, Field_Delimiter);
         if Index_Count = Index_Field then
            Result_String := Slice (String_To_Process, 1, Index_Next - 1);
            exit;
         else
            Index_Count := Index_Count + 1;
            -- String_To_Process := Slice (String_To_Process, Index_Next + 1, Length (String_To_Process));
            String_To_Process := Slice (String_To_Process, Index_Next + Length_Delimiter, Length (String_To_Process));
            --if Length (String_To_Process) <= 1 then -- Last field reached
            if Length (String_To_Process) <= Length_Delimiter then -- Last field reached
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
      String_To_Process_Length : constant Natural := Length (String_To_Process);
   begin
      if ((not Empty (String_Input)) and
          (not Empty (Field_To_Search)) and
          (not Empty (Field_Delimiter))
         ) then
         for I in 1 .. String_To_Process_Length loop
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
      if Empty (String_To_Process) then
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

      --Log.Dbg ("Columns " & To_String (Columns));
      --Log.Dbg ("Rows " & To_String (Rows));

      -- Format
      if Columns > 0 then
         for Index_Columns in 1..Columns loop

            -- Get max header width
            if Length (Header) > 0 then
               Max_Width := Length (Field_By_Index (Header, Index_Columns, ","));
            end if;

            -- Get max column width
            for Index_Rows in 1..Rows loop
               Current_Column := Field_By_Index (Field_By_Index (String_To_Process, Index_Rows, Row_Delimiter), Index_Columns, Column_Delimiter);
               if Length (Current_Column) > Max_Width then
                  Max_Width := Length (Current_Column);
               end if;
            end loop;

            --Log.Dbg ("Max_Width for column " & Current_Column & ": " & To_String (Max_Width));

            -- Format header
            if Length (Header) > 0 then
               Current_Column := Field_By_Index (Header, Index_Columns, ",");
               Display (0, Index_Columns) := Current_Column & (Max_Width - Length (Current_Column)) * " " ;
            end if;

            -- Format column
            for Index_Rows in 1..Rows loop
               Current_Column := Field_By_Index (Field_By_Index (String_To_Process, Index_Rows, Row_Delimiter), Index_Columns, Column_Delimiter);
               Display (Index_Rows, Index_Columns) := Current_Column & (Max_Width - Length (Current_Column)) * " " ;
            end loop;

         end loop;

         -- Display header
         if Length (Header) > 0 then
            Max_Width := 0;
            for Index_Columns in 1..Columns loop
               Current_Column := Display (0, Index_Columns) & "  ";
               Max_Width := Max_Width + Length (Current_Column);
               Tio.Put (Current_Column);
            end loop;
            Tio.Line;
            Tio.Put_Line ( (Max_Width - 2) * "-");
         end if;

         -- Display rows
         for Index_Rows in 1..Rows loop
            for Index_Columns in 1..Columns loop
               Tio.Put (Display (Index_Rows, Index_Columns) & "  ");
            end loop;
            Tio.Line;
         end loop;
      end if;

   end Field_Display;

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

   ---------------------------------------------------------------------------
   --  https://rosettacode.org/wiki/Determine_if_a_string_is_numeric#Ada
   function Is_Numeric (Item : in String) return Boolean is
      Dummy : Float;
   begin
      Dummy := Float'Value (To_ASCII (Item));
      return True;
   exception
      when others =>
         return False;
   end Is_Numeric;

   ---------------------------------------------------------------------------
   function Replace_Char (String_To_Process : String ; Char_In : ASCII_Character ; Char_Out : ASCII_Character) return String is
      Result_String : String := "";
      Result_Char : ASCII_Character;
      String_To_Process_Length : constant Natural := Length (String_To_Process);
   begin
      for I in 1 .. String_To_Process_Length loop
         --Result_Char := To_ASCII (From_Unicode (Element (String_To_Process, I)) );
         Result_Char := Get_ASCII (From_Unicode (Element (String_To_Process, I)),1);
         if (Result_Char = Char_In) then
            Result_Char := Char_Out;
         end if;
         Result_String := Result_String & From_ASCII (Result_Char);
      end loop;
      return Result_String;
   end Replace_Char;

   ---------------------------------------------------------------------------
   function Replace_Pattern (String_To_Process : String ;
              Pattern_In : String ; Pattern_Out : String) return String is
      Result_String : String := "";
      Result_Char : String;
      Pattern_In_Length : constant Natural := Length (Pattern_In);
      Pattern_Buffer : String := "";
      String_To_Process_Length : constant Natural := Length (String_To_Process);
   begin
      for I in 1 .. String_To_Process_Length loop
         Result_Char := Slice (String_To_Process, I, I);
         Pattern_Buffer := Pattern_Buffer & Result_Char;
         -- Sliding window
         if (Length (Pattern_Buffer) > Pattern_In_Length) then
            Pattern_Buffer := Slice (Pattern_Buffer, 2, Pattern_In_Length + 1);
         end if;
         if (Pattern_Buffer = Pattern_In) then
            Result_String := Slice (Result_String, 1, Length (Result_String) - Pattern_In_Length + 1) & Pattern_Out;
            Pattern_Buffer := "";
         else
            Result_String := Result_String & Result_Char;
         end if;
      end loop;
      return Result_String;
   end Replace_Pattern;

   ---------------------------------------------------------------------------
   function Starts_With (Item : String; Pattern : ASCII_Character) return Boolean is
   begin
      return 1 <= Length (Item) and then From_Unicode (Element (Item, 1)) = From_ASCII (Pattern);
   end Starts_With;

   function Starts_With (Item : String; Pattern : String) return Boolean is
   begin
      return Length (Pattern) <= Length (Item) and then Head (Item, Length (Pattern)) = Pattern;
   end Starts_With;

   ---------------------------------------------------------------------------
   function Stript_Chars (String_To_Process : String ; Char_List : String) return String is
      Result_String, Result_Char : String := "";
      String_To_Process_Length : constant Natural := Length (String_To_Process);
      Char_Set_Pattern_Length : constant Natural := Length (Char_List);
   begin
      for I in 1 .. String_To_Process_Length loop
         Result_Char := Slice (String_To_Process, I, I);
         for J in 1 .. Char_Set_Pattern_Length loop
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
      Source_Length : constant Natural := Length (Source);
      Pattern_Length : constant Natural := Length (Pattern);
   begin
      if (Source_Length > 0) and (Pattern_Length > 0) then
         if Index (Source, Pattern) > 0 then
            for I in reverse 1 .. Source_Length loop
               if I <= Source_Length - Pattern_Length then
                  if Slice (Source, I, I + Pattern_Length - 1) = Pattern then
                     Result := Slice (Source, I + Pattern_Length, Source_Length);
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
   function To_Hex (Byte : Unsigned8) return String is
      Hex_Chars : constant array (Unsigned8 range 0 .. 15)
        of Character := ('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D', 'E', 'F');
      Half_Byte_1 : constant Unsigned8 := Byte mod 16;
      Half_Byte_2 : constant Unsigned8 := Byte / 16;
   begin
      return From_Latin_1 (Hex_Chars (Half_Byte_2)) & From_Latin_1 (Hex_Chars (Half_Byte_1));
   end To_Hex;

   function To_Hex (String_To_Convert : String) return String is
      String_Extracted : String := " ";
      String_Converted : String := "";
   begin
      for I in 1 .. Length (String_To_Convert) loop
         String_Extracted := Slice (Source => String_To_Convert, Low => I, High => I);
         --Append (String_Converted, To_Hex ( Character'Pos ( String_Extracted (1))) & " ");
         Append (String_Converted, To_Hex ( To_String (To_Integer ( String_Extracted ))) & " ");

      end loop;
      --  Strip last trailing space
      if Length (String_Converted) >= 3 then
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
   function To_Integer (V : String) return Integer is
      Result : Integer := 0;
   begin
      if Length (V) > 0 then
         if Is_Numeric (V) then
            Result := Integer'Value (To_ASCII (V));
         end if;
      end if;
      return Result;
   end To_Integer;

   ---------------------------------------------------------------------------
   function To_String (B : Boolean) return String is
   begin
      if B then
         return "True";
      else
         return "False";
      end if;
   end To_String;

   function To_String (I : Integer) return String is
   begin
      --return From_UTF_8 (I'Image).Delete (1, 1); Keep the sign
      return From_UTF_8 (I'Image);
   end To_String;

   function To_String (I : Long_Integer) return String is
   begin
      --return From_UTF_8 (Long_Integer'Image (I)).Delete (1, 1); Keep the sign
      return From_UTF_8 (Long_Integer'Image (I)).Delete (1, 1);
   end To_String;

  function To_String (C : ASCII_Character) return String is
   begin
      return From_Latin_1 ((1 => C));
   end To_String;

   ---------------------------------------------------------------------------
   function To_Val (String_To_Convert : String) return String is
      S_Out : String := "";
   begin
      if Length (String_To_Convert) > 0 then
         for I in 1 .. Length (String_To_Convert) loop
            S_Out := S_Out & To_String ( To_Integer (From_Unicode (Element (String_To_Convert, I))) ) & " ";
         end loop;
      end if;
      return S_Out;
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
      while Index (Src, "/", AS.Backward) = 1 and Length (Src) > 1 loop
         Src := Slice (Src, 2, Length (Src));
      end loop;
      --  Suppress trailing slash(es)
      while Index (Src, "/", AS.Backward) = Length (Src) and Length (Src) > 1 loop
         Src := Slice (Src, 1, Length (Src) - 1);
      end loop;
      return Src;
   end Trim_Slashes;

-------------------------------------------------------------------------------
end v22.Uxs;
-------------------------------------------------------------------------------
