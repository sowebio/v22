-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-sys.ads
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

with GNAT.Expect;
with GNAT.OS_Lib;
with GNATCOLL.Memory;
with GNAT.Debug_Pools;

with v22.Uxs; use v22.Uxs;

package v22.Sys is

   package GE renames GNAT.Expect;
   package GOL renames GNAT.OS_Lib;
   package GM renames GNATCOLL.Memory;

   type Report is new GNAT.Debug_Pools.Report_Type;
   --  Report usage (g-debpoo.ads) :
   --    - Sys.All_Reports
   --    - Sys.Memory_Usage
   --    - Sys.Allocations_Count
   --    - Sys.Sort_Total_Allocs
   --    - Sys.Marked_Blocks

   function Command_Path (Command_Name : String) return String;
   -- Return full qualified command path.

   function Get_Alloc_Ada return String;
   --  Return current and max allocations done from Ada excluding others
   --  languages. Format of returned string : Ada Cur: [ 868 ] Max: [ 1600 ]

   function Get_Alloc_All return String;
   --  Return current and max allocations done from all languages including
   --  Ada. Format of returned string : Ada Cur: [ 868 ] Max: [ 1600 ]
   --  This uses system calls to find out the program's resident Size (RSS)
   --  information, both the peak and the current Size.

   --  Environment
   
   function Get_Env (Name : String) return String;
   --  Returns String value of String or String environment variable Name.

   procedure Set_Env (Name : String; Value : String);
   --  Set an environment variable

   function Get_Home return String;
   --  Returns HOME path.

   procedure Get_Memory_Dump (Size : Positive;
                               Report_View : Report := Memory_Usage);
   --  Dump information about memory usage. Size is the number of the biggest
   --  memory users we want to show. Report indicates which sorting order is
   --  used in the report.
      
   function Get_System_Name return String;
   --  Returns system name like "Debian" or "Ubuntu" or "not handled 
   --  (unprocessed system string returned)".

   function Get_System_Version return String;
   --  Returns system version like 10, 11 for Debian or 18.04, 20.04, 22.04 
   --  for Ubuntu or "System not handled (unprocessed system string returned)". 
   --  For Ubuntu systems, subversion like 18.04.6 and LTS string are omitted.
   
   function Is_Command (Command : String) return Boolean;
   --  Return true if command exists and reachable from path.

   function Is_Package (Package_Name : String; Host_Name : String := "") return Boolean;
   --  Return true if Package_Name is installed. 
   
   function Install_Packages (Packages_List : String; Host_Name : String := "") return Boolean;
   --  Install packages for Debian, Ubuntu or derivatives distributions.
   
   function Purge_Packages (Packages_List : String; Host_Name : String := "") return Boolean;
   --  Install packages for Debian, Ubuntu or derivatives distributions.

   procedure Reset_Memory_Monitor;
   --  Reset all internal data (i.e. reset all displayed counters. This is in
   --  general not needed, unless you want to know what memory is used by
   --  specific parts of your application.

   procedure Set_Memory_Monitor (State : Boolean := True);
   --  If Activate_Monitor is true, the program will monitor all memory
   --  allocations and deallocations, and through the Get_Memory_Dump
   --  procedure below be able to report the memory usage. The overhead is
   --  almost null when the monitor is disabled.

   --  Shell execute ----------------------------------------------------------

   procedure Shell_Execute (Command : String);
   procedure Shell_Execute (Command : String; Result : out Integer);
   procedure Shell_Execute (Command : String;
                            Result : out Integer;
                            Output : out String);
   --  Executes shell command. Return the exit code if passed from the
   --  executed command. Without Output parameter, the command console output
   --  is displayed by default but can be redirected. If Output is used, then
   --  the executed command output is return in this parameter.

private

   function Install_Package (Package_Name : String; Host_Name : String) return Boolean;
   --  Install a Debian or Ubuntu package.

   function Purge_Package (Package_Name : String; Host_Name : String) return Boolean;
   --  Purge a Debian or Ubuntu package.

   procedure Shell_Execute_Output (Command : String;
                                   Result : out Integer;
                                   Output : out String);

-------------------------------------------------------------------------------
end v22.Sys;
-------------------------------------------------------------------------------
