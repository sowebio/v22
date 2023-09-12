-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-crl-callbacks.ads
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

with Interfaces.C;         use Interfaces.C;
with Interfaces.C.Strings; use Interfaces.C.Strings;

with v22.Crl; use v22.Crl;

package v22.Crl.Callbacks is

   type Recfile is record
      Filename : chars_ptr;
      Stream   : File_P;
   end record;
   pragma Convention (C, Recfile);

   type Recfile_P is access all Recfile;

   function Write_Data (Buf : chars_ptr; Size : size_t; Nmemb : size_t; Stream : File_P) return size_t;
   --

   -- Added sr@20211029 to avoid convention passing error
   pragma Convention (C, Write_Data);

   function My_fwrite (Buf : chars_ptr; Size : size_t; Nmemb : size_t; Outp : File_P) return size_t;
   --

------------------------------------------------------------------------------
end v22.Crl.Callbacks;
------------------------------------------------------------------------------
