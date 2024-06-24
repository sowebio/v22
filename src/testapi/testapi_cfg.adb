-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_cfg.ads
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

package body TestApi_Cfg is

   procedure Run is
   begin

      -------------------------------------------------------------------------
      Msg.Set_Task ("CFG T1");
      Msg.Title ("Configuration file demo");
      Msg.New_Line;

      Msg.Info ("Default name creation (prog name + .cfg)");
      Fls.Delete_File ("test.cfg"); -- demo mode
      if Cfg.Open then
         Cfg.Set ("Section_1", "Parameter_11", "Value_11");
         Cfg.Set ("Section_2", "Parameter_21", "Value_21");
         Cfg.Set ("Section_3", "Parameter_31", "Value_31");
         Msg.Info ("Get Parameter_11: " &
                    Cfg.Get ("Section_1", "Parameter_11"));
         Msg.Info ("Get Parameter_21: " &
                    Cfg.Get ("Section_2", "Parameter_21"));
         Msg.Info ("Get Parameter_31: " &
                    Cfg.Get ("Section_3", "Parameter_31"));
         Cfg.Close;
      else
         Prg.Set_Exit_Status (4);
      end if;

      Msg.Info ("Create with a custom name (custom.ini)");
      Msg.Info ("with a more complex scheme (see test.adb)");
      Fls.Delete_File ("custom.ini"); -- demo mode

      if Cfg.Open ("custom.ini") then

         Cfg.Comment ("-----------------------------------------------------");
         Cfg.Comment ("A comment line is already write at the end of the");
         Cfg.Comment ("configuration file. So you can write headers and");
         Cfg.Comment ("intermediate comments to obtain a good looking");
         Cfg.Comment ("configuration file :)");
         Cfg.Comment ("-----------------------------------------------------");
         Cfg.Comment ("");
         Cfg.Comment ("Section 1 deals with blah blah...");
         Cfg.Comment ("");
         Cfg.Set ("Section_1", "Parameter_11", "Value_11");
         Cfg.Set ("Section_1", "Parameter_12", "Value_12");
         Cfg.Set ("Section_1", "Parameter_13", "Value_13");
         Cfg.Comment ("");
         Cfg.Comment ("Section 2 deals with blah blah...");
         Cfg.Comment ("");
         Cfg.Set ("Section_2", "Parameter_21", "Value_21",
                  "Only this line should stay...");
         Cfg.Set ("Section_2", "Parameter_22", "Value_22");
         Cfg.Set ("Section_2", "Parameter_23", "Value_23");
         Cfg.Comment ("");
         Cfg.Comment ("Section 3 deals with blah blah...");
         Cfg.Comment ("");
         Cfg.Set ("Section_3", "Parameter_31", "Value_31");
         Cfg.Set ("Section_3", "Parameter_32", "Value_32");
         Cfg.Set ("Section_3", "Parameter_33", "Value_33");
         Cfg.Close;

         Fls.Copy_File ("custom.ini", "custom.untouched");

         if Cfg.Open ("custom.ini") then

            Msg.Info ("Get Parameter_12: " &
                       Cfg.Get ("Section_1", "Parameter_12"));
            Msg.Info ("Get Parameter_22: " &
                       Cfg.Get ("Section_2", "Parameter_22"));
            Msg.Info ("Get Parameter_32: " &
                       Cfg.Get ("Section_3", "Parameter_32"));
            Cfg.Delete ("Section_1", "Parameter_11");
            Cfg.Delete ("Section_1", "Parameter_12");
            Cfg.Delete ("Section_1", "Parameter_13");
            Msg.Info ("Delete all section_1 parameters. At the");
            Msg.Info ("deleting of the last parameter, the");
            Msg.Info ("[section_1] line, now useless, is also");
            Msg.Info ("deleted");
            Cfg.Delete ("Section_3", "Parameter_31");
            Cfg.Delete ("Section_3", "Parameter_32");
            Cfg.Delete ("Section_3", "Parameter_33");
            Cfg.Delete ("Section_2", "Parameter_22");
            Cfg.Delete ("Section_2", "Parameter_23");
            Msg.Info ("At the end of the configuration file");
            Msg.Info ("demo, only Section_2 and parameter_21");
            Msg.Info ("should remain");
            Cfg.Delete ("Section_unknowed", "Parameter_22");
            Cfg.Delete ("Section_2", "Parameter_unknown");
            Cfg.Close;
         end if;

         Msg.Info ("Trailing comment preservation test");
         if Cfg.Open ("dontdelete.me") then
            Cfg.Set ("Section_1", "Parameter_11", "New_Value: " & Prg.Date_Time_Stamp);
            Cfg.Set ("Section_2", "Parameter_21", "New_Value: " & Prg.Date_Time_Stamp);
            Cfg.Set ("Section_3", "Parameter_31", "New_Value: " & Prg.Date_Time_Stamp);
            Cfg.Close;
         end if;
         Msg.New_Line;
      else
         Prg.Set_Exit_Status (2);
      end if;
      Msg.Set_Disk (Off);

   end Run;

-------------------------------------------------------------------------------
end TestApi_Cfg;
-------------------------------------------------------------------------------
