-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-sys.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - System package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Environment_Variables;

with Interfaces.C;

with v22.Msg;
with v22.Prg;
with v22.Tio;

package body v22.Sys is

   package AEV renames Ada.Environment_Variables;
   
   function Command_Path (Command_Name : String) return String is
      Exec_Error : Integer;
      Find_Command : constant String := "which " & Command_Name;
      Exec_Output : String;
   begin
      Shell_Execute (Find_Command, Exec_Error, Exec_Output);
      return Exec_Output;
   end Command_Path;
   
   --  function Command_Path (Command_Name : String) return String is
   --  begin
   --     return Command_Path (To_String (Command_Name ));
   --  end Command_Path;

   ---------------------------------------------------------------------------
   function Get_Alloc_Ada return String is
      Watermark : constant GM.Watermark_Info := GM.Get_Ada_Allocations;
   begin
      return "Ada Cur: [" & From_ASCII (Watermark.Current'Img) & " ] Max: " &
                      "[" & From_ASCII (Watermark.High'Img) & " ]";
   end Get_Alloc_Ada;

   function Get_Alloc_All return String is
      Watermark : constant GM.Watermark_Info := GM.Get_Allocations;
   begin
      return "All Cur: [" & From_ASCII (Watermark.Current'Img) & " ] Max: " &
                      "[" & From_ASCII (Watermark.High'Img) & " ]";
   end Get_Alloc_All;

   ---------------------------------------------------------------------------
   function Get_Env (Name : String) return String is
   begin
      if AEV.Exists (To_ASCII (Name)) then
         return From_ASCII (AEV.Value (To_ASCII (Name)));
      else
         return Null_UXString;
      end if;
   end Get_Env;
  
   ---------------------------------------------------------------------------
   function Get_Home return String is
   begin
      return Get_Env ("HOME");
   end Get_Home;

   procedure Get_Memory_Dump (Size : Positive;
                       Report_View : Report := Memory_Usage) is
   begin                          -- Report_View cast
      GNATCOLL.Memory.Dump (Size, GNATCOLL.Memory.Report_Type (Report_View));
   end Get_Memory_Dump;
   
  ---------------------------------------------------------------------------  
   function Get_System_Name return String is
      System_Name : String := Tio.Read_File ("/etc/issue.net");
   begin
      if (Index (System_Name, "Debian") > 0) then
         System_Name := "debian";
      elsif (Index (System_Name, "Ubuntu") > 0) then
         System_Name := "ubuntu";
      else
         System_Name := "System not handled (" & System_Name & ")";
      end if;
      return System_Name;
   end Get_System_Name;
   
   function Get_System_Version return String is
      System_Name : constant String := Tio.Read_File ("/etc/issue.net");
      System_Version : String := "";
   begin
      if (Index (System_Name, "Debian") > 0) then
         System_Version := Tail (System_Name, 18);
      elsif (Index (System_Name, "Ubuntu") > 0) then
         System_Version := Slice (System_Name, 8, 12);
      else
         System_Version := "System not handled (" & System_Name & ")";
      end if;
      return System_Version;
   end Get_System_Version;

   ---------------------------------------------------------------------------
   function Is_Package (Package_Name : String; Host_Name : String := "") return Boolean is
      Exec_Error : Integer;
      Check_Command : constant String := "dpkg -s " & Package_Name;
      Exec_Output : String;
      Exec_Output_Install_Ok : constant String := "Status: install ok installed";
   begin
      if Empty (Host_Name) then
         -- Exec_Error checking is irrelevant with dpkg-query, allways check 
         -- message output. In addition, dpkg-query -W -f='${Status}' method
         -- is not recommended as the result is not reliable. Indeed, with a 
         -- package available in, for example, two architectures, as libcurl4
         -- (libcurl4:amd64, libcurl4:i386), the response string 
         -- 'install ok installed' appears two times instead of once.
         -- Exec_Error checking is also irrelevant with dpkg -s. String status
         -- texting is explicitly mandatory
         Sys.Shell_Execute (Check_Command & STD_ERR_OUT_REDIRECT, Exec_Error, Exec_Output); --, Exec_Output);
      else
         Sys.Shell_Execute ("ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & DQ & 
                            Check_Command & DQ & STD_ERR_OUT_REDIRECT, Exec_Error, Exec_Output);
      end if;
      Msg.Dbg ("Is_Install Exec_Output: " & Exec_Output);
      return (Index (Exec_Output, Exec_Output_Install_Ok) > 0);
   end Is_Package;

   ---------------------------------------------------------------------------
   function Install_Package (Package_Name : String; Host_Name : String) return Boolean is
      Exec_Error : Integer;
      -- apt and aptitude are for user's end (more human friendly but 
      -- unstable CLI between versions), apt-get is for scripts (stable CLI)
      Installer : String := "apt-get";
      Package_Name_Trimmed : constant String := Trim_Both (Package_Name);
      Result : Boolean := True;
   begin
   
      if not Is_Package (Package_Name_Trimmed, Host_Name) then
         if Empty (Host_Name) then
            Msg.Std ("Local install of " & Package_Name_Trimmed);
            if Prg.Is_User_Not_Root then
               Installer := "sudo " & Installer;
            end if;
            Msg.Dbg ("Command: " & Installer & " install -y " & Package_Name_Trimmed & STD_ERR_OUT_REDIRECT);
            Sys.Shell_Execute (Installer & " install -y " & Package_Name_Trimmed & STD_ERR_OUT_REDIRECT, Exec_Error);
         else
            Msg.Std ("Remote install of " & Package_Name_Trimmed);
            -- Use of Exec_Output seems to disturb results. Definitly we should handle redirection analysis through STD and ERR files
            Sys.Shell_Execute ("ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & 
               DQ & Installer & " install -y " & Package_Name_Trimmed & DQ & STD_ERR_OUT_REDIRECT, Exec_Error);
         end if;
         
         if Exec_Error = 0 then
            Msg.Std (Package_Name_Trimmed & " installed successfully.");
         else
            Msg.Err ("v20.Sys.Install_Package > Exec error installing: " & Package_Name_Trimmed & " Error code: " & To_String (Exec_Error));
            Result := False;
         end if;
      else
         Msg.Std ("Package " & Package_Name_Trimmed & " already installed.");
      end if;
      return Result;
   end Install_Package;

   function Install_Packages (Packages_List : String; Host_Name : String := "") return Boolean is
      Packages_String : constant String := Packages_List;
      Packages_Count : Natural;
      Result : Boolean := True;
   begin
      Msg.Std ("Check packages to install.");
      Packages_Count := Field_Count (Packages_String, VD);
      if (Packages_Count > 0) then
         -- Install packages
         for I in 1 .. Packages_Count loop
            -- The returned value is not relevant as managed dependencies by package manager may interfere
            Result := Install_Package (Field_By_Index (Packages_String, I, VD), Host_Name);
         end loop;
         -- Check installed packages
         Result := True;
         for I in 1 .. Packages_Count loop
            if not Is_Package (Field_By_Index (Packages_String, I, VD), Host_Name) then
               Result := False;
               exit;
            end if;
         end loop;
      end if;
      return Result;
   end Install_Packages;
      
   ---------------------------------------------------------------------------
   function Purge_Package (Package_Name : String; Host_Name : String) return Boolean is
      Exec_Error : Integer;
      -- type Table_Installers is array (1 .. 3) of String;
      -- apt and aptitude are for user's end (more human friendly but 
      -- unstable CLI between versions), apt-get is for scripts (stable CLI)
      Installer : String := "apt-get";
      Result : Boolean := True;
   begin
      if Is_Package (Package_Name, Host_Name) then
         Msg.Std ("Installing " & Package_Name);
         if Empty (Host_Name) then
            if Prg.Is_User_Not_Root then
               Installer := "sudo " & Installer;
            end if;
            Sys.Shell_Execute (Installer & " purge -y " & Package_Name & STD_ERR_OUT_REDIRECT, Exec_Error);
         else
            Sys.Shell_Execute ("ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & 
               DQ & Installer & " purge -y " & Package_Name & DQ & STD_ERR_OUT_REDIRECT, Exec_Error); 
         end if;
         
         if Exec_Error = 0 then
            Msg.Std (Package_Name & " installed successfully.");
         else
            Msg.Err ("v22.Sys.Install_Package > Exec error purging: " & Package_Name);
            Result := False;
         end if;
      else
         Msg.Std ("Package " & Package_Name & " not yet installed.");
      end if;
      return Result;
   end Purge_Package;

   function Purge_Packages (Packages_List : String; Host_Name : String := "") return Boolean is
      Packages_String : constant String := Packages_List;
      Packages_Count : Natural;
      Result : Boolean := True;
   begin
      Msg.Std ("Check packages to purge.");
      Packages_Count := Field_Count (Packages_String, VD);
      if (Packages_Count > 0) then
         -- Purge package
         for I in 1 .. Packages_Count loop
            -- The returned value is not relevant as managed dependencies by package manager may interfere
            Result := Purge_Package (Field_By_Index (Packages_String, I, VD), Host_Name);
         end loop;
         -- Check purged packages
         Result := True;
         for I in 1 .. Packages_Count loop
            if Is_Package (Field_By_Index (Packages_String, I, VD), Host_Name) then
               Result := False;
               exit;
            end if;
         end loop;
      end if;
      return Result;
   end Purge_Packages;
   
   ---------------------------------------------------------------------------
   procedure Reset_Memory_Monitor is
   begin
      GNATCOLL.Memory.Reset;
   end Reset_Memory_Monitor; 
   
   ---------------------------------------------------------------------------
   procedure Set_Env (Name : String; Value : String) is
   begin
      AEV.Set (To_ASCII (Name), To_ASCII (Value));
   end Set_Env;

   ---------------------------------------------------------------------------
   procedure Set_Memory_Monitor (State : Boolean := True) is
   begin
      GNATCOLL.Memory.Configure (Activate_Monitor => State);
   end Set_Memory_Monitor;

   ---------------------------------------------------------------------------
   function Is_Command (Command : String) return Boolean is
      function Sys (Arg : Interfaces.C.char_array) return Integer;
      pragma Import (C, Sys, "system");
      -- Stript parameters if exists (or not) because "which" needs command without parameter
      Command_Processed : constant String := Field_By_Index (Trim_Both (Command), 1, SP);
   begin
      return (Sys (Interfaces.C.To_C (To_ASCII ("which " & Command_Processed & STD_ERR_OUT_REDIRECT)) ) = 0);
   end Is_Command;
   
   ---------------------------------------------------------------------------
   procedure Shell_Execute_Output (Command : String;
                                   Result : out Integer;
                                   Output : out String) is
      Arguments : GOL.Argument_List_Access;
      Command_Exit_Code : aliased Integer; --  Must reside in memory (pointer)
   begin
      if Is_Command (Command) then
         Arguments := GOL.Argument_String_To_List (To_ASCII (Command));
         Output := From_ASCII (GE.Get_Command_Output
               (Command => Arguments (Arguments'First).all,
              Arguments => Arguments (Arguments'First + 1 .. Arguments'Last),
              Input => "",
              Status => Command_Exit_Code'Access));
         GOL.Free (Arguments);
         Result := Command_Exit_Code;
      else
         Output := "";
         Result := 255;
      end if;
   end Shell_Execute_Output;
   
   ---------------------------------------------------------------------------
   --  https://rosettacode.org/wiki/Execute_a_system_command#Ada
   procedure Shell_Execute (Command : String; Result : out Integer) is
      function Sys (Arg : Interfaces.C.char_array) return Integer;
      pragma Import (C, Sys, "system");
   begin
      if Is_Command (Command) then
         Result := Sys (Interfaces.C.To_C (To_ASCII (Command)));
      else
         Result := 255;
      end if;
   end Shell_Execute;
     
   procedure Shell_Execute (Command : String) is
      Dummy : Integer := 0;
   begin
      Shell_Execute (Command, Dummy);
   end Shell_Execute;
   
   procedure Shell_Execute (Command : String;
                            Result : out Integer;
                            Output : out String) is
   begin
      Shell_Execute_Output (Command, Result, Output);
   end Shell_Execute;
      
-------------------------------------------------------------------------------
end v22.Sys;
-------------------------------------------------------------------------------
