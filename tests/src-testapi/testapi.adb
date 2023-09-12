------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      TestApi.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
------------------------------------------------------------------------------
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
------------------------------------------------------------------------------

with Ada.Calendar;
with GNAT.Calendar.Time_IO;

with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;

with UXStrings; use UXStrings;

with v22;
with v22.Cfg;
with v22.Crl; use v22.Crl; -- for operators
with v22.Fls;
with v22.Msg;
with v22.Net;
with v22.Prg;
with v22.Sql;
with v22.Sys;
with v22.Tio;
with v22.Uxs;

with TestApi_Cfg;
with TestApi_Crl;
with TestApi_Msg;
with TestApi_Sql;
with TestApi_Sys;
with TestApi_Tio;

-------------
-- TestApi --
-------------

procedure TestApi is

   use v22;
   use v22.Uxs;
   subtype String is UXString;

   package AC  renames Ada.Calendar;
   package GCT renames GNAT.Calendar.Time_IO;
   package GCL renames GNAT.Command_Line;
   package GOL renames GNAT.OS_Lib;
   package GS renames GNAT.Strings;

  -- subtype Integer_64 is Interfaces.Integer_64;

   Result : Integer := 0;

   Config : GCL.Command_Line_Configuration;
   --Cursor_Animation_1 : aliased Boolean := False;
   --Cursor_Animation_2 : aliased Boolean := False;
   Exception_Test : aliased Boolean := False;
   Package_Test : aliased Boolean := False;
   Memory_Reports : aliased Boolean := False;
   String_Option : aliased GS.String_Access;
   Long_Option : aliased Integer := 0;

begin

   Sys.Set_Memory_Monitor (True);

   Prg.Set_Version (0, 8);
   Msg.Set_Display(True);
   Msg.Set_Debug (False);
   Tio.Cursor_Off;

   Msg.Line;
   Msg.Std ("v22 framework - API test program.");
   Msg.Std ("Copyright (C) Sowebio SARL 2020-2022" & From_Latin_1 (GCT.Image (AC.Clock, "%Y")) & ", according to GPLv3.");
   Msg.Std (Prg.Get_Version & " - " & v22.Get_Version & " - " & v22.Get_Build);
   Msg.Line;

   ----------------------------------------------------------------------------

   GCL.Set_Usage (Config,
                  Usage => "[switches] [arguments] overview",
                  Help =>  "This is the short help text");

   GCL.Define_Switch (Config, Package_Test'Access,
                      Switch => "-p",
                      Help => "Enable package management test");
   GCL.Define_Switch (Config, Exception_Test'Access,
                      Switch => "-e",
                      Help => "Enable exception test");
   GCL.Define_Switch (Config, String_Option'Access,
                      Switch => "-s=", --  "-s:" to avoid "=" assignment
                      Help => "Enable option -s. Arg is a string ");
   GCL.Define_Switch (Config, Long_Option'Access,
                      Switch => "-l=",
                      Long_Switch => "--long=",
                      Help => "Enable long option. Arg is an integer");

   GCL.Getopt (Config); --  Command line processing

   ----------------------------------------------------------------------------

   Msg.Set_Header (True);
   Msg.Set_Disk (True);

   Msg.Std (Sys.Get_Alloc_Ada);
   Msg.Std (Sys.Get_Alloc_All);
   Msg.Line;

   ----------------------------------------------------------------------------

   Msg.Set_Task ("BASE 1");
   Msg.Title ("Get option demo");
   Msg.Line;

   if String_Option.all /= "" then
      Msg.Std ("Switch -s:        : " & From_Latin_1 (String_Option.all));
   end if;
   if Long_Option /= 0 then
      Msg.Std ("Switch -l --long= : " & To_String (Long_Option));
   end if;

   if (String_Option.all = "") and (Long_Option = 0) then
      Msg.Std ("Try ./test -h or --help");
      Msg.Std ("Try ./test -badoption");
      Msg.Std ("Try ./test -s=toto --long=123456 -1 -2 (or -12 instead)");
   end if;
   Msg.Line;

   ----------------------------------------------------------------------------

   Msg.Set_Task ("BASE 2");
   Msg.Title ("Basic informations");
   Msg.Line;

   Msg.Std ("Program name   : " & Prg.Name);
   Msg.Std ("User home      : " & Sys.Get_Home);
   Msg.Std ("Library version: " & v22.Get_Version);
   Msg.Std ("A time stamp  : " & Prg.Time_Stamp);
   Msg.Line;

   ----------------------------------------------------------------------------

   --  Module   Test suite
   --  v22.Cfg      x
   --  v22.Crl      x
   --  v22.Fls
   --  v22.Log   Used throughout the test program
   --  v22.Msg      x
   --  v22.Net
   --  v22.Prg
   --  v22.Sql      x
   --  v22.Sys      x
   --  v22.Tio      x
   --  v22.Uxs   Used throughout the test program

   TestApi_Cfg.Run;
   TestApi_Crl.Run;
   TestApi_Msg.Run;
   TestApi_Sql.Run;
   TestApi_Sys.Run (Package_Test);
   TestApi_Tio.Run;

   ----------------------------------------------------------------------------

   Msg.Set_Debug (False);
   Tio.Cursor_On;

   Msg.Line;
   Msg.Set_Task ("END");
   Msg.Title ("End of demo");
   Msg.Line;

   if Exception_Test then

      Msg.Line;
      Msg.Title ("Exception test trigered by a raise exception");
      Msg.Line;

   -- ------------------------------------------------------------------/\-----
     Raise_Exception;   --  < Uncomment for trigger exception test     /!!\
   -- ----------------------------------------------------------------/-!!-\---
   end if;

exception

   --  Invalid switches
   when GCL.Invalid_Switch =>
      Msg.Line;
      GOL.OS_Exit (2);

   --  -h or --help switches
   when GCL.Exit_From_Command_Line =>
      Msg.Line;
      GOL.OS_Exit (1);

   --  Runtime errors
   when Error : others =>
      v22.Exception_Handling (Error);

-----------------------------------------------------------------------------
end TestApi;
-----------------------------------------------------------------------------
