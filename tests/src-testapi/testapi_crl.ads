-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      TestApi_Crl.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - API test program
--
--  @description
--  Build application and documentation
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with System; use System;
with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with UXStrings; use UXStrings;

with v22;
with v22.Cfg;
with v22.Crl; use v22.Crl;
with v22.Crl.Callbacks; use v22.Crl.Callbacks;
with v22.Msg;
with v22.Fls;
with v22.Prg;

package TestApi_Crl is

   use v22;

   procedure Run;

-------------------------------------------------------------------------------
end TestApi_Crl;
-------------------------------------------------------------------------------
