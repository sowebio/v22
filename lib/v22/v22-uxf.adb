-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-utf.adb
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Text I/O package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Integer_Text_IO;

with v22.Fls;
with v22.Msg;
with v22.Prg;
with v22.Sys;
with v22.Tio;

package body v22.Uxf is

   function Image is new UXStrings.Conversions.Scalar_Image (Encoding_Scheme);
   function Value is new UXStrings.Conversions.Scalar_Value (Encoding_Scheme);

   ----------------------------------------------------------------------------
   --  API - Terminal
   ----------------------------------------------------------------------------


   ----------------------------------------------------------------------------
   --  API - Text File
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Append (Handle : in out File; Name : String) is
   begin
      UTI.Open (Handle, UTI.Append_File, Name);
   end Append;

   ----------------------------------------------------------------------------
   procedure Create (Handle : in out File; Name : String) is
   begin
      UTI.Create (Handle, UTI.Out_File, Name, UTF_8, UTI.LF_Ending);
   end Create;

   ----------------------------------------------------------------------------
   function Get_Line (Handle : in out File) return String is
   begin
      return UTI.Get_Line (Handle);
   end Get_Line;

   procedure Get_Line (Handle : in out File; V : in out String) is
   begin
      V := UTI.Get_Line (Handle);
   exception
      when Constraint_Error =>
         Msg.Error ("Exception catched in v22.Uxf.Get_Line: CONSTRAINT_ERROR");
         V := "";
   end Get_Line;

   ----------------------------------------------------------------------------

   procedure Open_Read (Handle : in out File; Name : String; Scheme : Encoding_Scheme := UTF_8) is
   begin
      UTI.Open (Handle, UTI.In_File, Name, Scheme, UTI.LF_Ending);
   end Open_Read;

------------------------------------------------------------------------------
end v22.Uxf;
------------------------------------------------------------------------------
