-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_sql.ads
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

with Interfaces.C; use Interfaces.C;
with Interfaces.C.Strings;

with Gnoga.Types;
with Gnoga.Server.Database.MySQL;
with Gnoga.Server.Database.SQLite;

with UXStrings; use UXStrings;

with v22; use v22;
with v22.Msg;
with v22.Sql;
with v22.Tio;
with v22.Uxs; use v22.Uxs;

package TestApi_Sql is

   use Gnoga;
   use all type Gnoga.String;
   subtype String is UXString;

   procedure Run;

-------------------------------------------------------------------------------
end TestApi_Sql;
-------------------------------------------------------------------------------
