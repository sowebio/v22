-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_sys.adb
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

package body TestApi_Sys is

   procedure Run (Package_Test : Boolean) is

      Host_Name : String := "root@n250c1.genesix.org";

   begin

      -------------------------------------------------------------------------
      Msg.Set_Task ("SYS T1");
      Msg.Title ("Memory reports demo");
      Msg.New_Line;

      Msg.Title ("Memory: Report");
      Sys.Get_Memory_Dump (1);

      Msg.Title ("Memory: Allocations_Count");
      Sys.Get_Memory_Dump (1, Sys.Allocations_Count);

      Msg.Title ("Memory: Sort_Total_Allocs");
      Sys.Get_Memory_Dump (1, Sys.Sort_Total_Allocs);

      Msg.New_Line; Msg.Title ("");
      Msg.Title ("Memory: Marked_Blocks");
      Msg.Title (""); Msg.New_Line;
      Sys.Get_Memory_Dump (1, Sys.Marked_Blocks);
      Msg.New_Line;
      Msg.Title ("Memory: Reporting Ada and All languages ");
      Msg.New_Line;
      Msg.Info (Sys.Get_Alloc_Ada);
      Msg.Info (Sys.Get_Alloc_All);
      Msg.New_Line;

      -------------------------------------------------------------------------
      Msg.Set_Task ("SYS T2");
      Msg.Title ("Shell execute demo");

      Msg.New_Line;
      Msg.Info ("Execute cat test.cfg and display results.");
      Msg.New_Line;

      declare
         SE_Result : Integer := 0;
         SE_Output : String := "";
      begin
         Sys.Shell_Execute ("cat test.cfg", SE_Result, SE_Output);
         if SE_Result = 0 then
            Tio.Put_Line (SE_Output);
            Tio.New_Line;
         end if;
      end;

      declare
         SE_Result : Integer := 0;
      begin
         Sys.Shell_Execute ("find test.cfg", SE_Result);
         Tio.Put_Line (SE_Result);
         Tio.New_Line;
      end;

      declare
         SE_Result : Integer := 0;
      begin
         Sys.Shell_Execute ("find i.dont.exist", SE_Result);
         Tio.Put_Line (SE_Result);
         Tio.New_Line;
      end;

      -------------------------------------------------------------------------
      Msg.Set_Task ("SYS T3");
      Msg.Title ("Local package install");
      Msg.New_Line;

      Tio.Put ("Is 'apt' package installed? ");
      Tio.Put_Line (Sys.Is_Package ("apt"));

      Tio.Put ("Is 'joe' package installed? ");
      Tio.Put_Line (Sys.Is_Package ("joe"));

      Tio.Put ("Is 'le' package installed? ");
      Tio.Put_Line (Sys.Is_Package ("le"));

      if Package_Test then

         Msg.New_Line;

         if Sys.Install_Packages ("joe,le") then
            Msg.Info ("'joe' and 'le' packages has been installed.");
         else
             Msg.Error ("At least one package has not been installed.");
         end if;

         Msg.New_Line;

         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe"));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le"));

         Msg.New_Line;

         if Sys.Install_Packages ("le") then
            Msg.Info ("'le' is already installed, so Sys.Install_Packages returns true again.");
         end if;

         Msg.New_Line;

         if Sys.Purge_Packages ("joe,le") then
            Msg.Info ("'joe' and 'le' packages has been purged.");
         else
             Msg.Error ("At least one package has not been purged.");
         end if;

         Msg.New_Line;

         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe"));

         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le"));

         Msg.New_Line;

         ----------------------------------------------------------------------
         Msg.Set_Task ("SYS T4");
         Msg.Title ("Distant package install");
         Msg.New_Line;

         Tio.Put ("Is 'apt' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("apt", Host_Name));
         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe", Host_Name));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le", Host_Name));

         Msg.New_Line;

         if Sys.Install_Packages ("joe,le", Host_Name) then
            Msg.Info ("'joe' and 'le' packages has been installed.");
         else
             Msg.Error ("At least one package has not been installed.");
         end if;

         Msg.New_Line;

         Tio.Put ("Is 'apt' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("apt", Host_Name));
         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe", Host_Name));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le", Host_Name));

         Msg.New_Line;

         if Sys.Install_Packages ("le", Host_Name) then
            Msg.Info ("'le' is already installed, so Sys.Install_Packages returns true again.");
         end if;

         Msg.New_Line;

         if Sys.Purge_Packages ("joe,le", Host_Name) then
            Msg.Info ("'joe' and 'le' packages has been purged.");
         else
             Msg.Error ("At least one package has not been purged.");
         end if;

         Msg.New_Line;

         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe", Host_Name));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le", Host_Name));

         Msg.New_Line;

      end if;

      -------------------------------------------------------------------------
      Msg.Set_Task ("SYS T5");
      Msg.Title ("Check command");
      Msg.New_Line;

      Tio.Put ("Is NoCommand installed? ");
      Tio.Put_Line (Sys.Is_Command ("nocommand"));

      Tio.Put ("Where is NoCommand installed? ");
      Tio.Put_Line (Sys.Command_Path ("nocommand"));

      Tio.Put ("Is Bash installed? ");
      Tio.Put_Line (Sys.Is_Command ("bash"));

      Tio.Put ("Where is Bash installed? ");
      Tio.Put_Line (Sys.Command_Path ("bash"));

   end Run;

-------------------------------------------------------------------------------
end TestApi_Sys;
-------------------------------------------------------------------------------
