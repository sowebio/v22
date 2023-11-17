-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-crl-callbacks.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - curl interface
--
--  @description
--
--  @authors
--  Andreas Almroth - aa - https://web.archive.org/web/20070403105909/http://www.almroth.com/adacurl (author)
--  Stéphane Rivière - sr - sriviere@soweb.io (low level sources amalgation, debugging, rewriting, and high level functions)
--
--  @versions
--  See git log
------------------------------------------------------------------------------

with System; use System;
with System.Address_To_Access_Conversions;
with Unchecked_Conversion;

package body v22.Crl.Callbacks is

   function F2f is new Unchecked_Conversion (System.Address, Recfile_P);

   function fopen (Name : chars_ptr; Mode : chars_ptr) return File_P;
   pragma Import (C, fopen, "fopen");

   function fwrite (Ptr : chars_ptr; Size : size_t; Nitems : size_t; Stream : File_P) return size_t;
   pragma Import (C, fwrite, "fwrite");

   function Write_Data (Buf : chars_ptr; Size : size_t; Nmemb : size_t; Stream : File_P) return size_t is
      Written : size_t;
   begin
      Written := fwrite (Buf, Size, Nmemb, Stream);
      return Written;
   end Write_Data;

   function My_fwrite (Buf : chars_ptr; Size : size_t; Nmemb : size_t; Outp : System.Address) return size_t is
      X : Recfile_P := F2f (Outp);
   begin
      if X /= null then
         if X.Stream = System.Null_Address then
            X.Stream := fopen (X.Filename, New_String ("wb"));

            if X.Stream = System.Null_Address then
               return -1;
            end if;
         end if;
      end if;

      return fwrite (Buf, Size, Nmemb, X.Stream);

   end My_fwrite;

------------------------------------------------------------------------------
end v22.Crl.Callbacks;
------------------------------------------------------------------------------
