with Ada.Calendar;
with Ada.Containers.Vectors;
with Ada.Streams;
with Ada.Unchecked_Conversion;
with Interfaces;

private with Ada.Calendar.Conversions;
private with Ada.Calendar.Formatting;
private with Interfaces.C;
private with System.Atomic_Operations.Integer_Arithmetic;

package Libre_Frame is
   use Ada;

   Time_First : constant Calendar.Time;
   Time_Last  : constant Calendar.Time;

   -- STRINGS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   function Has_Prefix (Source, Pattern : String) return Boolean;
   function Has_Suffix (Source, Pattern : String) return Boolean;

   function Replace (Source, Pattern, By : String) return String;

   -- FILES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   Read_File_Error : exception;
   function Read_File (Name : String) return String;

   -- CONTAINERS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   generic
      type Index is range <>;
      type Item is private;
      type List is array (Index range <>) of Item;
   procedure Reverse_Array (Target : in out List);

   generic
      type Index is range <>;
      type Item is private;
      type List is array (Index range <>) of Item;

      with package Vectors is new Containers.Vectors (Index_Type => Index, Element_Type => Item);
   package Vectors_Conversion is
      function To_Array (Source : Vectors.Vector) return List;
      function To_Vector (Source : List) return Vectors.Vector;
   end Vectors_Conversion;

   -- BYTES ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   type Unsigned_8 is new Interfaces.Unsigned_8;
   type Unsigned_16 is new Interfaces.Unsigned_16;
   type Unsigned_32 is new Interfaces.Unsigned_32;
   type Unsigned_64 is new Interfaces.Unsigned_64;

   type Integer_8 is new Interfaces.Integer_8;
   type Integer_16 is new Interfaces.Integer_16;
   type Integer_32 is new Interfaces.Integer_32;
   type Integer_64 is new Interfaces.Integer_64;

   subtype Natural_8 is Integer_8 range 0 .. Integer_8'Last;
   subtype Natural_16 is Integer_16 range 0 .. Integer_16'Last;
   subtype Natural_32 is Integer_32 range 0 .. Integer_32'Last;
   subtype Natural_64 is Integer_64 range 0 .. Integer_64'Last;

   subtype Positive_8 is Integer_8 range 1 .. Integer_8'Last;
   subtype Positive_16 is Integer_16 range 1 .. Integer_16'Last;
   subtype Positive_32 is Integer_32 range 1 .. Integer_32'Last;
   subtype Positive_64 is Integer_64 range 1 .. Integer_64'Last;

   subtype Byte is Unsigned_8;
   type Bytes is array (Natural_64 range <>) of aliased Byte;
   type Bytes_Access is access all Bytes;

   function To_Bytes (Source : Streams.Stream_Element_Array) return Bytes;
   function To_Bytes (Source : String) return Bytes;

   function To_Stream_Element_Array (Source : Bytes) return Streams.Stream_Element_Array;

   function To_String (Source : Bytes) return String;
   function Hexadecimal (Source : Bytes) return String;

   procedure Put_Random_Bytes (Target : out Bytes);

   subtype Two_Bytes is Bytes (0 .. 1);
   subtype Four_Bytes is Bytes (0 .. 3);
   subtype Eight_Bytes is Bytes (0 .. 7);

   function To_Unsigned_8 is new Unchecked_Conversion (Source => Byte, Target => Unsigned_8);
   function To_Unsigned_16 is new Unchecked_Conversion (Source => Two_Bytes, Target => Unsigned_16);
   function To_Unsigned_32 is new Unchecked_Conversion (Source => Four_Bytes, Target => Unsigned_32);
   function To_Unsigned_64 is new Unchecked_Conversion (Source => Eight_Bytes, Target => Unsigned_64);

   function To_Bytes is new Unchecked_Conversion (Source => Unsigned_8, Target => Byte);
   function To_Bytes is new Unchecked_Conversion (Source => Unsigned_16, Target => Two_Bytes);
   function To_Bytes is new Unchecked_Conversion (Source => Unsigned_32, Target => Four_Bytes);
   function To_Bytes is new Unchecked_Conversion (Source => Unsigned_64, Target => Eight_Bytes);

   -- CONCURRENT ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   protected type Mutex (Already_Locked : Boolean := False) is
      entry Lock;
      procedure Unlock;
   private
      Locked : Boolean := Already_Locked;
   end Mutex;

   type Atomic_8 is new Natural_8 with Default_Value => 0, Atomic;
   type Atomic_16 is new Natural_16 with Default_Value => 0, Atomic;
   type Atomic_32 is new Natural_32 with Default_Value => 0, Atomic;
   type Atomic_64 is new Natural_64 with Default_Value => 0, Atomic;

   procedure Add (Item : aliased in out Atomic_8; Value : Atomic_8);
   procedure Add (Item : aliased in out Atomic_16; Value : Atomic_16);
   procedure Add (Item : aliased in out Atomic_32; Value : Atomic_32);
   procedure Add (Item : aliased in out Atomic_64; Value : Atomic_64);

   function Add (Item : aliased in out Atomic_8; Value : Atomic_8) return Atomic_8;
   function Add (Item : aliased in out Atomic_16; Value : Atomic_16) return Atomic_16;
   function Add (Item : aliased in out Atomic_32; Value : Atomic_32) return Atomic_32;
   function Add (Item : aliased in out Atomic_64; Value : Atomic_64) return Atomic_64;

   function "+" (Left, Right : Atomic_8) return Atomic_8 is abstract;
   function "-" (Left, Right : Atomic_8) return Atomic_8 is abstract;
   function "*" (Left, Right : Atomic_8) return Atomic_8 is abstract;
   function "/" (Left, Right : Atomic_8) return Atomic_8 is abstract;
   function "+" (Left, Right : Atomic_16) return Atomic_16 is abstract;
   function "-" (Left, Right : Atomic_16) return Atomic_16 is abstract;
   function "*" (Left, Right : Atomic_16) return Atomic_16 is abstract;
   function "/" (Left, Right : Atomic_16) return Atomic_16 is abstract;
   function "+" (Left, Right : Atomic_32) return Atomic_32 is abstract;
   function "-" (Left, Right : Atomic_32) return Atomic_32 is abstract;
   function "*" (Left, Right : Atomic_32) return Atomic_32 is abstract;
   function "/" (Left, Right : Atomic_32) return Atomic_32 is abstract;
   function "+" (Left, Right : Atomic_64) return Atomic_64 is abstract;
   function "-" (Left, Right : Atomic_64) return Atomic_64 is abstract;
   function "*" (Left, Right : Atomic_64) return Atomic_64 is abstract;
   function "/" (Left, Right : Atomic_64) return Atomic_64 is abstract;

private
   use System;
   use type Interfaces.C.long;

   Time_First : constant Calendar.Time :=
     Calendar.Conversions.To_Ada_Time (-2177452800); -- 1901-01-01 00:00:00 +0000 UTC
   Time_Last  : constant Calendar.Time :=
     Calendar.Conversions.To_Ada_Time (9223372036); -- 2262-04-11 23:47:16 +0000 UTC

   package Integer_8_Arithmetic is new Atomic_Operations.Integer_Arithmetic (Atomic_8);
   package Integer_16_Arithmetic is new Atomic_Operations.Integer_Arithmetic (Atomic_16);
   package Integer_32_Arithmetic is new Atomic_Operations.Integer_Arithmetic (Atomic_32);
   package Integer_64_Arithmetic is new Atomic_Operations.Integer_Arithmetic (Atomic_64);

   procedure Add (Item : aliased in out Atomic_8; Value : Atomic_8) renames Integer_8_Arithmetic.Atomic_Add;
   procedure Add (Item : aliased in out Atomic_16; Value : Atomic_16) renames Integer_16_Arithmetic.Atomic_Add;
   procedure Add (Item : aliased in out Atomic_32; Value : Atomic_32) renames Integer_32_Arithmetic.Atomic_Add;
   procedure Add (Item : aliased in out Atomic_64; Value : Atomic_64) renames Integer_64_Arithmetic.Atomic_Add;

   function Add (Item : aliased in out Atomic_8; Value : Atomic_8) return Atomic_8
   renames Integer_8_Arithmetic.Atomic_Fetch_And_Add;

   function Add (Item : aliased in out Atomic_16; Value : Atomic_16) return Atomic_16
   renames Integer_16_Arithmetic.Atomic_Fetch_And_Add;

   function Add (Item : aliased in out Atomic_32; Value : Atomic_32) return Atomic_32
   renames Integer_32_Arithmetic.Atomic_Fetch_And_Add;

   function Add (Item : aliased in out Atomic_64; Value : Atomic_64) return Atomic_64
   renames Integer_64_Arithmetic.Atomic_Fetch_And_Add;

end Libre_Frame;
