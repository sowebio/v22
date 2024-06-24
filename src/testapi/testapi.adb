------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi.adb
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
with Ada.Calendar.Arithmetic; use Ada.Calendar.Arithmetic;
with Ada.Calendar.Formatting;
with Ada.Calendar.Time_Zones;

with GNAT.Calendar.Time_IO;
with GNAT.Command_Line;
with GNAT.OS_Lib;
with GNAT.Strings;

with GNATCOLL.JSON;

with UXStrings; use UXStrings;

with v22; use v22;
with v22.Cfg;
with v22.Fls;
with v22.Msg;
with v22.Net;
with v22.Prg;
with v22.Sql;
with v22.Sys;
with v22.Tio;
with v22.Uxs; use v22.Uxs;

with TestApi_Cfg;
with TestApi_Msg;
with TestApi_Sql;
with TestApi_Sys;
with TestApi_Tio;

-------------
-- TestApi --
-------------

procedure TestApi is

   subtype String is UXString;

   package AC  renames Ada.Calendar;
   package ACF renames Ada.Calendar.Formatting;
   package GCT renames GNAT.Calendar.Time_IO;

   package GCL renames GNAT.Command_Line;
   package GOL renames GNAT.OS_Lib;
   package GS renames GNAT.Strings;

   package GJ renames GNATCOLL.JSON;

  -- subtype Integer_64 is Interfaces.Integer_64;

   Result : Integer := 0;

   Config : GCL.Command_Line_Configuration;
   String_Option : aliased GS.String_Access;
   Long_Option : aliased Integer := 0;

   Exception_Test : aliased Boolean := False;
   Package_Test : aliased Boolean := False;
   Memory_Reports : aliased Boolean := False;

begin

   Sys.Set_Memory_Monitor (On);

   Prg.Set_Version (0, 8);
   Msg.Set_Display (On);
   Msg.Set_Debug (Off);
   Tio.Set_Cursor (Off);

   Msg.New_Line;
   Msg.Info ("v22 Framework - API test program");
   Msg.Info ("Copyright (C) Sowebio SARL 2020-" & From_Latin_1 (GCT.Image (AC.Clock, "%Y")) & ", according to LGPLv3");
   Msg.Info (Prg.Get_Version & " - " & v22.Get_Version & " - " & v22.Get_Build);
   Msg.New_Line;

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

   Msg.Set_Header (On);
   Msg.Set_Disk (On);

   Msg.Info (Sys.Get_Alloc_Ada);
   Msg.Info (Sys.Get_Alloc_All);
   Msg.New_Line;

   ----------------------------------------------------------------------------

   Msg.Set_Task ("BASE 1");
   Msg.Title ("Get option demo");
   Msg.New_Line;

   if String_Option.all /= "" then
      Msg.Info ("Switch -s:        : " & From_Latin_1 (String_Option.all));
   end if;
   if Long_Option /= 0 then
      Msg.Info ("Switch -l --long= : " & To_String (Long_Option));
   end if;

   if (String_Option.all = "") and (Long_Option = 0) then
      Msg.Info ("Try ./test -h or --help");
      Msg.Info ("Try ./test -badoption");
      Msg.Info ("Try ./test -s=toto --long=123456 -1 -2 (or -12 instead)");
   end if;
   Msg.New_Line;

   ----------------------------------------------------------------------------

   Msg.Set_Task ("BASE 2");
   Msg.Title ("Basic informations");
   Msg.New_Line;

   Msg.Info ("Program name   : " & Prg.Name);
   Msg.Info ("User home      : " & Sys.Get_Home);
   Msg.Info ("Library version: " & v22.Get_Version);
   Msg.Info ("A time stamp  : " & Prg.Date_Time_Stamp);
   Msg.New_Line;

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

   --TestApi_Cfg.Run;
   --TestApi_Msg.Run;
   --TestApi_Sql.Run;
   --TestApi_Sys.Run (Package_Test);
   --TestApi_Tio.Run;

   ----------------------------------------------------------------------------
   --  Test zone

   --  declare
   --     --  Ovh_Consumer_Key : constant String := "579***3d5";
   --     --  Ovh_Application_Key : constant String := "4be***5b4";
   --     --  Ovh_Application_Secret : constant String := "069***b7b";
   --     Answer : String;
   --     Api_Query : GJ.JSON_Value := GJ.Create_Object;
   --  begin
   --
   --     Answer := Net.Api (Net.OVHcloud,
   --                        "eu.api.ovh.com/v1",
   --                        Ovh_Consumer_Key,
   --                        Ovh_Application_Key,
   --                        Ovh_Application_Secret,
   --                        Net.Get,
   --                        "domain/zone/absolem.org"
   --                       );
   --     Msg.Info (Answer);
   --
   --     -- JSON the wrong way
   --     Answer := Net.Api (Net.OVHcloud,
   --                        "eu.api.ovh.com/v1",
   --                        Ovh_Consumer_Key,
   --                        Ovh_Application_Key,
   --                        Ovh_Application_Secret,
   --                        Net.Post,
   --                        "domain/zone/absolem.org/record",
   --                        -- {"fieldType":"TXT","subDomain":AAAA_X","target":"CCCC_C"}
   --                        "{" & DQ & "fieldType" & DQ & ":" & DQ & "TXT"    & DQ & "," &
   --                              DQ & "subDomain" & DQ & ":" & DQ & "AAAA_X" & DQ & "," &
   --                              DQ & "target"    & DQ & ":" & DQ & "CCCC_C" & DQ & "}"
   --                       );
   --     Msg.Info (Answer);
   --
   --     --  Populate JSON query (JSON the right way)
   --     GJ.Set_Field (Api_Query, "fieldType", "TXT");
   --     GJ.Set_Field (Api_Query, "subDomain", "BBBB_Y");
   --     GJ.Set_Field (Api_Query, "target", "DDDD_D");
   --
   --     Answer := Net.Api (Net.OVHcloud,
   --                        "eu.api.ovh.com/v1",
   --                        Ovh_Consumer_Key,
   --                        Ovh_Application_Key,
   --                        Ovh_Application_Secret,
   --                        Net.Post,
   --                        "domain/zone/absolem.org/record",
   --                        From_Latin_1 (GJ.Write (Api_Query)));
   --     Msg.Info (Answer);
   --
   --     --  Answer := Net.Send_Sms (Net.OVHcloud,
   --     --                          "eu.api.ovh.com/v1",
   --     --                          Ovh_Consumer_Key,
   --     --                          Ovh_Application_Key,
   --     --                          Ovh_Application_Secret,
   --     --                          "sms-rs1***-2",
   --     --                          "V***U.COM",
   --     --                          "+336***:+336***",
   --     --                          "Message SMS multiple avec accents en UTF-8 (éèàùôöœï) à Olivier et Stéphane et..." & LF &
   --     --                          "...saut de ligne !" & LF & LF &
   --     --                          "Signé : le canard en plastique :)"
   --     --                          );
   --     --  Msg.Info (Answer);
   --
   --  end;


   declare
         --  TZ   : AC.Time_Zones.Time_Offset := AC.Time_Zones.UTC_Time_Offset;
         --  Day : AC.Time := AC.Formatting.Value ("2024-03-01 00:00:00.00"); --, TZ);
         --  Day_Previous : AC.Time := Day - 1;
         Day_Previous : AC.Time;
   begin

      --Msg.Info (To_String_Unsigned (AC.Formatting.Day (Day_Previous)));

      -- Date
      Msg.Info ("Date: " & From_ASCII (GCT.Image (AC.Clock, "%Y%m%d")));

      -- Day previous
      declare
         Day_Previous : AC.Time;
         Day_SOW : AC.Time;
         --  DOW : ACF.Day_Name;
         --  DOW_Number : Integer;
         --  DOW_Name : String;
      begin
         --  DOW := ACF.Day_Of_Week (Day_Previous);
         --  DOW_Name := From_Latin_1 (ACF.Day_Name'Image (DOW));
         --  Msg.Info ("Dow_Name: " & DOW_Name);

         --  DOW := ACF.Day_Of_Week (Day_Previous);
         --  DOW_Number := ACF.Day_Name'Pos (DOW);
         --  Msg.Info ("Dow_Number: " & To_String_Unsigned (DOW_Number));

         -- Previous day
         Day_Previous := AC.Formatting.Value (To_Latin_1 (Prg.Date) & " 00:00:00.00") - 1;
         Msg.Info ("Day_Previous: " & From_ASCII (GCT.Image (Day_Previous, "%Y%m%d")));

         Msg.Info ("DOW_Name: " & From_Latin_1 (ACF.Day_Name'Image (ACF.Day_Of_Week (Day_Previous))));
         Msg.Info ("DOW_Number: " & To_String_Unsigned (Integer (ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous))) + 1 ));

         -- First day of week's date
         --Day_SOW := Day_Previous - ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous));
         Msg.Info ("Day_SOW: " & From_ASCII (GCT.Image (Day_Previous - ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous)), "%Y%m%d")));

         -- Last day of week's date
         --Day_SOW := Day_Previous + (6 - ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous)));
         Msg.Info ("Day_EOW: " & From_ASCII (GCT.Image (Day_Previous + (6 - ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous))), "%Y%m%d")));

         Day_Previous := AC.Formatting.Value ("2024-03-01 00:00:00.00") - 1;
         Msg.Info ("Day_Previous: " & From_ASCII (GCT.Image (Day_Previous, "%Y%m%d")));

         Msg.Info ("DOW_Name: " & From_Latin_1 (ACF.Day_Name'Image (ACF.Day_Of_Week (Day_Previous))));
         Msg.Info ("DOW_Number: " & To_String_Unsigned (Integer (ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous))) + 1 ));

         Day_SOW := Day_Previous - ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous));
         Msg.Info ("Day_SOW: " & From_ASCII (GCT.Image (Day_SOW, "%Y%m%d")));


         Day_Previous := AC.Formatting.Value ("2024-01-01 00:00:00.00") - 1;
         Msg.Info ("Day_Previous: " & From_ASCII (GCT.Image (Day_Previous, "%Y%m%d")));

         Msg.Info ("DOW_Name: " & From_Latin_1 (ACF.Day_Name'Image (ACF.Day_Of_Week (Day_Previous))));
         Msg.Info ("DOW_Number: " & To_String_Unsigned (Integer (ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous))) + 1 ));

         Day_SOW := Day_Previous - ACF.Day_Name'Pos (ACF.Day_Of_Week (Day_Previous));
         Msg.Info ("Day_SOW: " & From_ASCII (GCT.Image (Day_SOW, "%Y%m%d")));

         Msg.Info ("Month: " & From_ASCII (GCT.Image (AC.Formatting.Value (To_Latin_1 (Prg.Date) & " 00:00:00.00"), "%Y%m")) & "%");


         --  Day_SOW := AC.Formatting.Value (To_Latin_1 (Prg.Date) & " 00:00:00.00") - ACF.Day_Name'Pos (DOW);
         --  Msg.Info ("Day_SOW: " & From_ASCII (GCT.Image (Day_SOW, "%Y%m%d")));

      end;

         --From_ASCII (GCT.Image (AC.Clock, "%Y%m%d"));


   end;

   ----------------------------------------------------------------------------

   Msg.Set_Debug (Off);
   Tio.Set_Cursor (On);

   Msg.New_Line;
   Msg.Set_Task ("END");
   Msg.Title ("End of demo");
   Msg.New_Line;

   if Exception_Test then

      Msg.New_Line;
      Msg.Title ("Exception test trigered by a raise exception");
      Msg.New_Line;

   -- ------------------------------------------------------------------/\-----
     Raise_Exception;   --  < Uncomment for trigger exception test     /!!\
   -- ----------------------------------------------------------------/-!!-\---
   end if;

exception

   --  Invalid switches
   when GCL.Invalid_Switch =>
      Msg.New_Line;
      GOL.OS_Exit (2);

   --  -h or --help switches
   when GCL.Exit_From_Command_Line =>
      Msg.New_Line;
      GOL.OS_Exit (1);

   --  Runtime errors
   when Error : others =>
      v22.Exception_Handling (Error);

-----------------------------------------------------------------------------
end TestApi;
-----------------------------------------------------------------------------
