with Ada.Directories;
with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

with GNATCOLL.Random;

package body Libre_Frame is

   function Has_Prefix (Source, Pattern : String) return Boolean
   is (Pattern'Length <= Source'Length and then Pattern = Source (Source'First .. Source'First + Pattern'Length - 1));

   function Has_Suffix (Source, Pattern : String) return Boolean
   is (Pattern'Length <= Source'Length and then Pattern = Source (Source'Last - Pattern'Length + 1 .. Source'Last));

   function Replace (Source, Pattern, By : String) return String is
      Index : constant Natural := Strings.Fixed.Index (Source, Pattern);
   begin
      if Index = 0 then
         return Source;
      end if;
      return Strings.Fixed.Replace_Slice (Source, Index, Index + Pattern'Length - 1, By);
   end Replace;

   function Read_File (Name : String) return String is
   begin
      declare
         use Streams.Stream_IO;

         Data : String (1 .. Natural (Directories.Size (Name)));
         File : File_Type;
      begin
         Open (File, In_File, Name);
         String'Read (Stream (File), Data);
         Close (File);
         return Data;
      end;
   exception
      when Error : others =>
         raise Read_File_Error with Exceptions.Exception_Message (Error);
   end Read_File; -- TODO: test End_Error (and other exceptions), checks if I need to close the file (and if closing the file can reraise exceptions)

   procedure Reverse_Array (Target : in out List) is
      I   : Index := Target'First;
      J   : Index := Target'Last;
      Tmp : Item;
   begin
      while I < J loop
         Tmp := Target (I);
         Target (I) := Target (J);
         Target (J) := Tmp;
         I := I + 1;
         J := J - 1;
      end loop;
   end Reverse_Array;

   package body Vectors_Conversion is
      function To_Array (Source : Vectors.Vector) return List is
      begin
         return Result : List (Source.First_Index .. Source.Last_Index) do
            for I in Result'Range loop
               Result (I) := Source (I);
            end loop;
         end return;
      end To_Array;

      function To_Vector (Source : List) return Vectors.Vector is
         Result : Vectors.Vector;
      begin
         Result.Reserve_Capacity (Source'Length);
         for Item of Source loop
            Result.Append (Item);
         end loop;
         return Result;
      end To_Vector;
   end Vectors_Conversion;

   function To_Bytes (Source : Streams.Stream_Element_Array) return Bytes is
      subtype Result is Bytes (0 .. Source'Length - 1);
      function Convert is new Unchecked_Conversion (Streams.Stream_Element_Array, Result);
   begin
      return Convert (Source);
   end To_Bytes;

   function To_Bytes (Source : String) return Bytes is
      subtype Result is Bytes (0 .. Source'Length - 1);
      function Convert is new Unchecked_Conversion (String, Result);
   begin
      return Convert (Source);
   end To_Bytes;

   function To_Stream_Element_Array (Source : Bytes) return Streams.Stream_Element_Array is
      use type Streams.Stream_Element_Offset;
      subtype Result is Streams.Stream_Element_Array (0 .. Source'Length - 1);
      function Convert is new Unchecked_Conversion (Bytes, Result);
   begin
      return Convert (Source);
   end To_Stream_Element_Array;

   function To_String (Source : Bytes) return String is
      subtype Result is String (1 .. Source'Length);
      function Convert is new Unchecked_Conversion (Bytes, Result);
   begin
      return Convert (Source);
   end To_String;

   function Hexadecimal (Source : Bytes) return String is
      -- TODO: optimize
      package Stream_Element_IO is new Text_IO.Modular_IO (Byte);
      Hex : String (1 .. 6);
   begin
      return Result : String (1 .. Source'Length * 2) do
         for I in Source'Range loop
            Stream_Element_IO.Put (To => Hex, Item => Source (I), Base => 16);
            if Source (I) < 16 then
               Hex (4) := '0';
            end if;
            Result (Positive (I - Source'First + 1) * 2 - 1 .. Positive (I - Source'First + 1) * 2) := Hex (4 .. 5);
         end loop;
      end return;
   end Hexadecimal;

   procedure Put_Random_Bytes (Target : out Bytes) is
      type At_One is array (Positive range <>) of aliased Byte;
      procedure Random is new GNATCOLL.Random.Random_Array (Data => Byte, Data_Array => At_One);
      Local : At_One (1 .. Target'Length);
   begin
      Random (Local);
      Target := Bytes (Local);
   end Put_Random_Bytes;

   protected body Mutex is
      entry Lock when not Locked is
      begin
         Locked := True;
      end Lock;

      procedure Unlock is
      begin
         Locked := False;
      end Unlock;
   end Mutex;

end Libre_Frame;
