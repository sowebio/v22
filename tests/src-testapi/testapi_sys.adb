-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      TestApi_Sys.adb
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

      Msg.Set_Task ("SYS T1");
      Msg.Title ("Memory reports demo");

      --Msg.Line; Msg.Title ("");
      Msg.Title ("Memory: Report");
      --Msg.Title (""); Msg.Line;
      Sys.Get_Memory_Dump (1);

      --Msg.Line; Msg.Title ("");
      Msg.Title ("Memory: Allocations_Count");
      --Msg.Title (""); Msg.Line;
      Sys.Get_Memory_Dump (1, Sys.Allocations_Count);

      --Msg.Line; Msg.Title ("");
      Msg.Title ("Memory: Sort_Total_Allocs");
      --Msg.Title (""); Msg.Line;
      Sys.Get_Memory_Dump (1, Sys.Sort_Total_Allocs);

      Msg.Line; Msg.Title ("");
      Msg.Title ("Memory: Marked_Blocks");
      Msg.Title (""); Msg.Line;
      Sys.Get_Memory_Dump (1, Sys.Marked_Blocks);
      Msg.Line;
      Msg.Title ("Memory: Reporting Ada and All languages ");
      Msg.Line;
      Msg.Std (Sys.Get_Alloc_Ada);
      Msg.Std (Sys.Get_Alloc_All);
      Msg.Line;

      ----------------------------------------------------------------------------

      Msg.Set_Task ("SYS T2");
      Msg.Title ("Shell execute demo");

      Msg.Line;
      Msg.Std ("Execute cat test.cfg and display results.");
      Msg.Line;

      declare
         SE_Result : Integer := 0;
         SE_Output : String := "";
      begin
         Sys.Shell_Execute ("cat test.cfg", SE_Result, SE_Output);
         if SE_Result = 0 then
            Tio.Put_Line (SE_Output);
            Tio.Line;
         end if;
      end;

      declare
         SE_Result : Integer := 0;
      begin
         Sys.Shell_Execute ("find test.cfg", SE_Result);
         Tio.Put_Line (SE_Result);
         Tio.Line;
      end;

      declare
         SE_Result : Integer := 0;
      begin
         Sys.Shell_Execute ("find i.dont.exist", SE_Result);
         Tio.Put_Line (SE_Result);
         Tio.Line;
      end;

      -------------------------------------------------------------------------

      Msg.Set_Task ("SYS T3");
      Msg.Title ("Local package install");
      Msg.Line;

      Tio.Put ("Is 'apt' package installed? ");
      Tio.Put_Line (Sys.Is_Package ("apt"));

      Tio.Put ("Is 'joe' package installed? ");
      Tio.Put_Line (Sys.Is_Package ("joe"));

      Tio.Put ("Is 'le' package installed? ");
      Tio.Put_Line (Sys.Is_Package ("le"));

      if Package_Test then

         Msg.Line;

         if Sys.Install_Packages ("joe,le") then
            Msg.Std ("'joe' and 'le' packages has been installed.");
         else
            Msg.Err ("At least one package has not been installed.");
         end if;

         Msg.Line;

         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe"));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le"));

         Msg.Line;

         if Sys.Install_Packages ("le") then
            Msg.Std ("'le' is already installed, so Sys.Install_Packages returns true again.");
         end if;

         Msg.Line;

         if Sys.Purge_Packages ("joe,le") then
            Msg.Std ("'joe' and 'le' packages has been purged.");
         else
            Msg.Err ("At least one package has not been purged.");
         end if;

         Msg.Line;

         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe"));

         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le"));

         Msg.Line;

         ----------------------------------------------------------------------

         Msg.Set_Task ("SYS T4");
         Msg.Title ("Distant package install");
         Msg.Line;

         Tio.Put ("Is 'apt' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("apt", Host_Name));
         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe", Host_Name));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le", Host_Name));

         Msg.Line;

         if Sys.Install_Packages ("joe,le", Host_Name) then
            Msg.Std ("'joe' and 'le' packages has been installed.");
         else
            Msg.Err ("At least one package has not been installed.");
         end if;

         Msg.Line;

         Tio.Put ("Is 'apt' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("apt", Host_Name));
         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe", Host_Name));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le", Host_Name));

         Msg.Line;

         if Sys.Install_Packages ("le", Host_Name) then
            Msg.Std ("'le' is already installed, so Sys.Install_Packages returns true again.");
         end if;

         Msg.Line;

         if Sys.Purge_Packages ("joe,le", Host_Name) then
            Msg.Std ("'joe' and 'le' packages has been purged.");
         else
            Msg.Err ("At least one package has not been purged.");
         end if;

         Msg.Line;

         Tio.Put ("Is 'joe' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("joe", Host_Name));
         Tio.Put ("Is 'le' package installed? ");
         Tio.Put_Line (Sys.Is_Package ("le", Host_Name));

         Msg.Line;

      end if;

      ----------------------------------------------------------------------------

      Msg.Set_Task ("SYS T5");
      Msg.Title ("Check command");
      Msg.Line;

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
