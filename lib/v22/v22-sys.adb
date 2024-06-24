-------------------------------------------------------------------------------
-- 
--  _|      _|    _|_|      _|_|    
--  _|      _|  _|    _|  _|    _| 
--  _|      _|      _|        _|    
--    _|  _|      _|        _|      
--      _|      _|_|_|_|  _|_|_|_|  
--
--  @file      v22-sys.adb
--  @copyright See authors list below and README.md file
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
   
   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------
   
   ----------------------------------------------------------------------------
   function Command_Path (Command_Name : String) return String is
      Exec_Error : Integer;
      Find_Command : constant String := "which " & Command_Name;
      Exec_Output : String;
   begin
      Shell_Execute (Find_Command, Exec_Error, Exec_Output);
      return Exec_Output;
   end Command_Path;
   
   ----------------------------------------------------------------------------
   procedure CRC16_Initialize (Mode_In : CRC16_Mode_Type := CRC16_CCITT_AUG) is
   begin
      if Mode_In = CRC16_CCITT_AUG then
         CRC16_Init := 16#1D0F#;
         CRC16_Table (0 .. 255) :=
           (16#0000#, 16#1021#, 16#2042#, 16#3063#, 16#4084#, 16#50a5#, 16#60c6#, 16#70e7#, 16#8108#, 16#9129#, 16#a14a#,
            16#b16b#, 16#c18c#, 16#d1ad#, 16#e1ce#, 16#f1ef#, 16#1231#, 16#0210#, 16#3273#, 16#2252#, 16#52b5#, 16#4294#,
            16#72f7#, 16#62d6#, 16#9339#, 16#8318#, 16#b37b#, 16#a35a#, 16#d3bd#, 16#c39c#, 16#f3ff#, 16#e3de#, 16#2462#,
            16#3443#, 16#0420#, 16#1401#, 16#64e6#, 16#74c7#, 16#44a4#, 16#5485#, 16#a56a#, 16#b54b#, 16#8528#, 16#9509#,
            16#e5ee#, 16#f5cf#, 16#c5ac#, 16#d58d#, 16#3653#, 16#2672#, 16#1611#, 16#0630#, 16#76d7#, 16#66f6#, 16#5695#, 
            16#46b4#, 16#b75b#, 16#a77a#, 16#9719#, 16#8738#, 16#f7df#, 16#e7fe#, 16#d79d#, 16#c7bc#, 16#48c4#, 16#58e5#,
            16#6886#, 16#78a7#, 16#0840#, 16#1861#, 16#2802#, 16#3823#, 16#c9cc#, 16#d9ed#, 16#e98e#, 16#f9af#, 16#8948#,
            16#9969#, 16#a90a#, 16#b92b#, 16#5af5#, 16#4ad4#, 16#7ab7#, 16#6a96#, 16#1a71#, 16#0a50#, 16#3a33#, 16#2a12#, 
            16#dbfd#, 16#cbdc#, 16#fbbf#, 16#eb9e#, 16#9b79#, 16#8b58#, 16#bb3b#, 16#ab1a#, 16#6ca6#, 16#7c87#, 16#4ce4#,
            16#5cc5#, 16#2c22#, 16#3c03#, 16#0c60#, 16#1c41#, 16#edae#, 16#fd8f#, 16#cdec#, 16#ddcd#, 16#ad2a#, 16#bd0b#,
            16#8d68#, 16#9d49#, 16#7e97#, 16#6eb6#, 16#5ed5#, 16#4ef4#, 16#3e13#, 16#2e32#, 16#1e51#, 16#0e70#, 16#ff9f#,
            16#efbe#, 16#dfdd#, 16#cffc#, 16#bf1b#, 16#af3a#, 16#9f59#, 16#8f78#, 16#9188#, 16#81a9#, 16#b1ca#, 16#a1eb#,
            16#d10c#, 16#c12d#, 16#f14e#, 16#e16f#, 16#1080#, 16#00a1#, 16#30c2#, 16#20e3#, 16#5004#, 16#4025#, 16#7046#,
            16#6067#, 16#83b9#, 16#9398#, 16#a3fb#, 16#b3da#, 16#c33d#, 16#d31c#, 16#e37f#, 16#f35e#, 16#02b1#, 16#1290#,
            16#22f3#, 16#32d2#, 16#4235#, 16#5214#, 16#6277#, 16#7256#, 16#b5ea#, 16#a5cb#, 16#95a8#, 16#8589#, 16#f56e#,
            16#e54f#, 16#d52c#, 16#c50d#, 16#34e2#, 16#24c3#, 16#14a0#, 16#0481#, 16#7466#, 16#6447#, 16#5424#, 16#4405#, 
            16#a7db#, 16#b7fa#, 16#8799#, 16#97b8#, 16#e75f#, 16#f77e#, 16#c71d#, 16#d73c#, 16#26d3#, 16#36f2#, 16#0691#,
            16#16b0#, 16#6657#, 16#7676#, 16#4615#, 16#5634#, 16#d94c#, 16#c96d#, 16#f90e#, 16#e92f#, 16#99c8#, 16#89e9#,
            16#b98a#, 16#a9ab#, 16#5844#, 16#4865#, 16#7806#, 16#6827#, 16#18c0#, 16#08e1#, 16#3882#, 16#28a3#, 16#cb7d#,
            16#db5c#, 16#eb3f#, 16#fb1e#, 16#8bf9#, 16#9bd8#, 16#abbb#, 16#bb9a#, 16#4a75#, 16#5a54#, 16#6a37#, 16#7a16#,
            16#0af1#, 16#1ad0#, 16#2ab3#, 16#3a92#, 16#fd2e#, 16#ed0f#, 16#dd6c#, 16#cd4d#, 16#bdaa#, 16#ad8b#, 16#9de8#,
            16#8dc9#, 16#7c26#, 16#6c07#, 16#5c64#, 16#4c45#, 16#3ca2#, 16#2c83#, 16#1ce0#, 16#0cc1#, 16#ef1f#, 16#ff3e#,
            16#cf5d#, 16#df7c#, 16#af9b#, 16#bfba#, 16#8fd9#, 16#9ff8#, 16#6e17#, 16#7e36#, 16#4e55#, 16#5e74#, 16#2e93#,
            16#3eb2#, 16#0ed1#, 16#1ef0#);
      
      elsif Mode_In = CRC16_BUYPASS_VERIPHONE then
         CRC16_Init := 16#0000#;
         CRC16_Table (0 .. 255) :=
           (16#0000#, 16#8005#, 16#800F#, 16#000A#, 16#801B#, 16#001E#, 16#0014#, 16#8011#, 16#8033#, 16#0036#, 16#003C#,
            16#8039#, 16#0028#, 16#802D#, 16#8027#, 16#0022#, 16#8063#, 16#0066#, 16#006C#, 16#8069#, 16#0078#, 16#807D#,
            16#8077#, 16#0072#, 16#0050#, 16#8055#, 16#805F#, 16#005A#, 16#804B#, 16#004E#, 16#0044#, 16#8041#, 16#80C3#,
            16#00C6#, 16#00CC#, 16#80C9#, 16#00D8#, 16#80DD#, 16#80D7#, 16#00D2#, 16#00F0#, 16#80F5#, 16#80FF#, 16#00FA#,
            16#80EB#, 16#00EE#, 16#00E4#, 16#80E1#, 16#00A0#, 16#80A5#, 16#80AF#, 16#00AA#, 16#80BB#, 16#00BE#, 16#00B4#,
            16#80B1#, 16#8093#, 16#0096#, 16#009C#, 16#8099#, 16#0088#, 16#808D#, 16#8087#, 16#0082#, 16#8183#, 16#0186#,
            16#018C#, 16#8189#, 16#0198#, 16#819D#, 16#8197#, 16#0192#, 16#01B0#, 16#81B5#, 16#81BF#, 16#01BA#, 16#81AB#,
            16#01AE#, 16#01A4#, 16#81A1#, 16#01E0#, 16#81E5#, 16#81EF#, 16#01EA#, 16#81FB#, 16#01FE#, 16#01F4#, 16#81F1#,
            16#81D3#, 16#01D6#, 16#01DC#, 16#81D9#, 16#01C8#, 16#81CD#, 16#81C7#, 16#01C2#, 16#0140#, 16#8145#, 16#814F#,
            16#014A#, 16#815B#, 16#015E#, 16#0154#, 16#8151#, 16#8173#, 16#0176#, 16#017C#, 16#8179#, 16#0168#, 16#816D#,
            16#8167#, 16#0162#, 16#8123#, 16#0126#, 16#012C#, 16#8129#, 16#0138#, 16#813D#, 16#8137#, 16#0132#, 16#0110#,
            16#8115#, 16#811F#, 16#011A#, 16#810B#, 16#010E#, 16#0104#, 16#8101#, 16#8303#, 16#0306#, 16#030C#, 16#8309#,
            16#0318#, 16#831D#, 16#8317#, 16#0312#, 16#0330#, 16#8335#, 16#833F#, 16#033A#, 16#832B#, 16#032E#, 16#0324#,
            16#8321#, 16#0360#, 16#8365#, 16#836F#, 16#036A#, 16#837B#, 16#037E#, 16#0374#, 16#8371#, 16#8353#, 16#0356#,
            16#035C#, 16#8359#, 16#0348#, 16#834D#, 16#8347#, 16#0342#, 16#03C0#, 16#83C5#, 16#83CF#, 16#03CA#, 16#83DB#,
            16#03DE#, 16#03D4#, 16#83D1#, 16#83F3#, 16#03F6#, 16#03FC#, 16#83F9#, 16#03E8#, 16#83ED#, 16#83E7#, 16#03E2#,
            16#83A3#, 16#03A6#, 16#03AC#, 16#83A9#, 16#03B8#, 16#83BD#, 16#83B7#, 16#03B2#, 16#0390#, 16#8395#, 16#839F#,
            16#039A#, 16#838B#, 16#038E#, 16#0384#, 16#8381#, 16#0280#, 16#8285#, 16#828F#, 16#028A#, 16#829B#, 16#029E#,
            16#0294#, 16#8291#, 16#82B3#, 16#02B6#, 16#02BC#, 16#82B9#, 16#02A8#, 16#82AD#, 16#82A7#, 16#02A2#, 16#82E3#,
            16#02E6#, 16#02EC#, 16#82E9#, 16#02F8#, 16#82FD#, 16#82F7#, 16#02F2#, 16#02D0#, 16#82D5#, 16#82DF#, 16#02DA#,
            16#82CB#, 16#02CE#, 16#02C4#, 16#82C1#, 16#8243#, 16#0246#, 16#024C#, 16#8249#, 16#0258#, 16#825D#, 16#8257#,
            16#0252#, 16#0270#, 16#8275#, 16#827F#, 16#027A#, 16#826B#, 16#026E#, 16#0264#, 16#8261#, 16#0220#, 16#8225#,
            16#822F#, 16#022A#, 16#823B#, 16#023E#, 16#0234#, 16#8231#, 16#8213#, 16#0216#, 16#021C#, 16#8219#, 16#0208#,
            16#820D#, 16#8207#, 16#0202#);
      
      end if;
      
   end CRC16_Initialize;

   ----------------------------------------------------------------------------
   function CRC16_Compute (String_Hex : String) return Unsigned_16 is
   --  https://crccalc.com (test)
   --  https://reveng.sourceforge.io/crc-catalogue/16.htm (catalog)
   --  https://www.nongnu.org/avr-libc/user-manual/crc16_8h_source.html
   --  https://github.com/madler/crcany
   --  https://barrgroup.com/embedded-systems/how-to/crc-calculation-c-code
   --  https://www.zlib.net/crc_v3.txt
      Std_String_Hex : Standard.String := To_Latin_1 (String_Hex);
      Index : Unsigned_16;
      CRC_Compute : CRC16_Type := CRC16_Init;
   begin
      for I in 1 .. Std_String_Hex'Length loop
         --Index := Shift_Right (CRC, 8) xor To_Unsigned_16 (String_Hex (I..I)); -- returns a string type of one char
         --CRC_Compute := Shift_Left ((CRC_Compute and 16#FF#), 8) xor CRC16_Table (Index); -- 'and 16#FF#' not needed
         Index := Shift_Right (CRC_Compute, 8) xor To_Unsigned_16 (Std_String_Hex (I)); -- returns a character type
         CRC_Compute := Shift_Left (CRC_Compute, 8) xor CRC16_Table (Index);
      end loop;
      return CRC_Compute;
   end CRC16_Compute;

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
      if Is_Empty (Host_Name) then
         -- Exec_Error checking is irrelevant with dpkg-query, allways check 
         -- message output. In addition, dpkg-query -W -f='${Status}' method
         -- is not recommended as the result is not reliable. Indeed, with a 
         -- package available in, for example, two architectures, as libcurl4
         -- (libcurl4:amd64, libcurl4:i386), the response string 
         -- 'install ok installed' appears two times instead of once.
         -- Exec_Error checking is also irrelevant with dpkg -s. String status
         -- texting is explicitly mandatory
         Sys.Shell_Execute (Check_Command & STD_ERR_OUT_REDIRECT, Exec_Error, Exec_Output); 
      else
         Sys.Shell_Execute ("ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & DQ & 
                            Check_Command & DQ & STD_ERR_OUT_REDIRECT, Exec_Error, Exec_Output);
      end if;
      Msg.Debug ("Is_Install Exec_Output: " & Exec_Output);
      return (Index (Exec_Output, Exec_Output_Install_Ok) > 0);
   end Is_Package;
   
   ---------------------------------------------------------------------------
   function Is_Packages (Packages_List : String; Host_Name : String := "") return Boolean is
      Packages_String : constant String := Packages_List;
      Packages_Count : Natural;
      Result : Boolean := True;
   begin
      --Msg.Info ("Check installed packages.");
      Packages_Count := Field_Count (Packages_String, VD);
      if (Packages_Count > 0) then
         -- Check installed packages
         for I in 1 .. Packages_Count loop
            if not Is_Package (Field_By_Index (Packages_String, I, VD), Host_Name) then
               Result := False;
               Msg.Error ("Sys.Is_Packages > Missing packkage: " & Field_By_Index (Packages_String, I, VD));
            end if;
         end loop;
      end if;
      return Result;
   end Is_Packages;
   
   ---------------------------------------------------------------------------
   function Install_Packages (Packages_List : String; Host_Name : String := "") return Boolean is
      Packages_String : constant String := Packages_List;
      Packages_Count : Natural;
      Result : Boolean := True;
   begin
      Msg.Info ("Check packages to install.");
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
   function Is_Command (Command : String) return Boolean is
      function Sys (Arg : Interfaces.C.char_array) return Integer;
      pragma Import (C, Sys, "system");
      -- Stript parameters if exists (or not) because "which" needs command without parameter
      Command_Processed : constant String := Field_By_Index (Trim_Both (Command), 1, SP);
   begin
      return (Sys (Interfaces.C.To_C (To_ASCII ("which " & Command_Processed & STD_ERR_OUT_REDIRECT)) ) = 0);
   end Is_Command;
   
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
   procedure Set_Memory_Monitor (Switch : On_Off) is
   begin
      GNATCOLL.Memory.Configure (Activate_Monitor => (Switch = On));
   end Set_Memory_Monitor;
    
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
   
   procedure Shell_Execute (Command : String; Result : out Integer; Output : out String) is
   begin
      Shell_Execute_Output (Command, Result, Output);
   end Shell_Execute;
   
   ---------------------------------------------------------------------------
   function To_Unsigned_16_High_Byte (Word : Unsigned_16) return Unsigned_8 is
    begin
      return Unsigned_8 (Shift_Right (Word, 8) and 16#00FF#);
    end To_Unsigned_16_High_Byte;
  
   ---------------------------------------------------------------------------
   function To_Unsigned_16_Low_Byte (Word : Unsigned_16) return Unsigned_8 is
   begin
      return Interfaces.Unsigned_8 (Word and 16#00FF#);
   end To_Unsigned_16_Low_Byte;

   ---------------------------------------------------------------------------
   function To_Unsigned_16 (High, Low : Unsigned_8) return Unsigned_16 is
   begin
      return Shift_Left (Unsigned_16 (High), 8) or Unsigned_16 (Low);
   end To_Unsigned_16;

   function To_Unsigned_16 (String_In : String) return Unsigned_16 is
      Temp : Unsigned_16 := Unsigned_16 (Integer'Value ("16#" & To_Latin_1 (String_In) & "#"));
   begin
      return Temp + (if Temp > 9 then 55 else 48);
   end To_Unsigned_16;
   
   function To_Unsigned_16 (Char_In : Character) return Unsigned_16 is
   begin
      return Unsigned_16 (Character'Pos (Char_In));
   end To_Unsigned_16;
   
   ---------------------------------------------------------------------------
   function To_Unsigned_8 (String_In : String) return Unsigned_8 is
      Temp : Unsigned_8 := Unsigned_8 (Integer'Value ("16#" & To_Latin_1 (String_In) & "#"));
   begin
      return Temp + (if Temp > 9 then 55 else 48);
   end To_Unsigned_8;
   
   function To_Unsigned_8 (Char_In : Character) return Unsigned_8 is
   begin
      return Unsigned_8 (Character'Pos (Char_In));
   end To_Unsigned_8;

   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

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
         if Is_Empty (Host_Name) then
            Msg.Info ("Local install of " & Package_Name_Trimmed);
            if Prg.Is_User_Not_Root then
               Installer := "sudo " & Installer;
            end if;
            Msg.Debug ("Command: " & Installer & " install -y " & Package_Name_Trimmed & STD_ERR_OUT_REDIRECT);
            Sys.Shell_Execute (Installer & " install -y " & Package_Name_Trimmed & STD_ERR_OUT_REDIRECT, Exec_Error);
         else
            Msg.Info ("Remote install of " & Package_Name_Trimmed);
            -- Use of Exec_Output seems to disturb results. Definitly we should handle redirection analysis through STD and ERR files
            Sys.Shell_Execute ("ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & 
               DQ & Installer & " install -y " & Package_Name_Trimmed & DQ & STD_ERR_OUT_REDIRECT, Exec_Error);
         end if;
         
         if Exec_Error = 0 then
            Msg.Info (Package_Name_Trimmed & " installed successfully.");
         else
            Msg.Error ("v20.Sys.Install_Package > Exec error installing: " & Package_Name_Trimmed & " Error code: " & To_String (Exec_Error));
            Result := False;
         end if;
      else
         Msg.Info ("Package " & Package_Name_Trimmed & " already installed.");
      end if;
      return Result;
   end Install_Package;
      
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
         Msg.Info ("Installing " & Package_Name);
         if Is_Empty (Host_Name) then
            if Prg.Is_User_Not_Root then
               Installer := "sudo " & Installer;
            end if;
            Sys.Shell_Execute (Installer & " purge -y " & Package_Name & STD_ERR_OUT_REDIRECT, Exec_Error);
         else
            Sys.Shell_Execute ("ssh -q -o StrictHostKeyChecking=no " & Host_Name & " " & 
               DQ & Installer & " purge -y " & Package_Name & DQ & STD_ERR_OUT_REDIRECT, Exec_Error); 
         end if;
         
         if Exec_Error = 0 then
            Msg.Info (Package_Name & " installed successfully.");
         else
            Msg.Error ("v22.Sys.Install_Package > Exec error purging: " & Package_Name);
            Result := False;
         end if;
      else
         Msg.Info ("Package " & Package_Name & " not yet installed.");
      end if;
      return Result;
   end Purge_Package;

   function Purge_Packages (Packages_List : String; Host_Name : String := "") return Boolean is
      Packages_String : constant String := Packages_List;
      Packages_Count : Natural;
      Result : Boolean := True;
   begin
      Msg.Info ("Check packages to purge.");
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
   procedure Shell_Execute_Output (Command : String; Result : out Integer; Output : out String) is
      Arguments : GOL.Argument_List_Access;
      Command_Exit_Code : aliased Integer; --  Must reside in memory (pointer)
   begin
      if Is_Command (Command) then
         Arguments := GOL.Argument_String_To_List (To_ASCII (Command));
         
         Output := From_Latin_1 (GE.Get_Command_Output
               (Command => Arguments (Arguments'First).all,
              Arguments => Arguments (Arguments'First + 1 .. Arguments'Last),
              Input => "",
              Status => Command_Exit_Code'Access));
         
         --  raised ADA.ASSERTIONS.ASSERTION_ERROR : dynamic_predicate failed
         --  Output := From_ASCII (GE.Get_Command_Output
         --        (Command => Arguments (Arguments'First).all,
         --       Arguments => Arguments (Arguments'First + 1 .. Arguments'Last),
         --       Input => "",
         --       Status => Command_Exit_Code'Access));
         
         GOL.Free (Arguments);
         Result := Command_Exit_Code;
      else
         Output := "";
         Result := 255;
      end if;
   end Shell_Execute_Output;
      
-------------------------------------------------------------------------------
end v22.Sys;
-------------------------------------------------------------------------------
