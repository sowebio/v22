-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-net.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Network package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with v22.Fls;
with v22.Msg;
with v22.Prg;
with v22.Sys;
with v22.Tio;

package body v22.Net is

   --  SSH_Temp_Command_Key : String := "-v22-net-command-private.key";
   --  SSH_Temp_Command_Output : String := "-v22-net-command.output";
   --  SSH_Temp_Delete_Directory : String := "-v22-net-delete-directory-tree.key";
   --  SSH_Temp_Mount_Key : String := "-v22-net-mount-private.key";

 ---------------------------------------------------------------------------
   function Command (Target : in String ; Command_In : in String ; SE_Output : out String) return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & SSH_Temp_Command_Key;
      Temp_Output : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-command.output";
      Command_String : String := "";
      Key_To_Use : String := "";
      SE_Result : Integer := 0;
      Result : Boolean := False;
   begin

      if not Empty (SSH_Key) then
         Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
         Key_To_Use := "-i " & Temp_Key_Name & " ";
      end if;

      -- Command must be between quotation marks to keep the redirections safe on the distant host
      Command_String := "ssh" & SSH_Default_Options & Key_To_Use & Target & " " & DQ & Command_In & DQ & " > " & Temp_Output;
      Sys.Shell_Execute (Command_String, SE_Result);

      if not Empty (SSH_Key) then
         Fls.Delete_File (Temp_Key_Name);
      end if;

      SE_Output := Tio.Read_File (Temp_Output);
      Fls.Delete_File (Temp_Output);

      if (SE_Result = 0) then
         if SSH_Message then
            Msg.Std ("Remote command " & Command_In & " on " & Target & " successful");
         end if;
         Result := True;
      else
         -- A command in error does not necessarily mean an order error.
         -- One may want to test the non-existence of a file, which will
         -- nevertheless trigger a non-zero error code. Hence the error message
         -- in the exception handling block
         if SSH_Message then
            Msg.Err ("Net.Send_Command > Command error with: " & Command_String & " on " & Target & " Error: " & To_String (SE_Result));
         end if;
         if Set_Exception then
            raise Error_Command;
         end if;
      end if;

      return Result;
   end Command;

---------------------------------------------------------------------------
   function Command (Target : in String ; Command_In : in String) return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & SSH_Temp_Command_Key;
      Command_String : String := "";
      Key_To_Use : String := "";
      Result : Boolean := False;
      SE_Result : Integer := 0;
   begin

      if not Empty (SSH_Key) then
         Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
         Key_To_Use := "-i " & Temp_Key_Name & " ";
      end if;

      -- Command must be between quotation marks to keep the redirections safe on the distant host
      Command_String := "ssh" & SSH_Default_Options & Key_To_Use & Target & " " & DQ & Command_In &
                              DQ & (if SSH_Output then Null_String else STD_ERR_OUT_REDIRECT);

      Sys.Shell_Execute (Command_String, SE_Result);

      if not Empty (SSH_Key) then
         Fls.Delete_File (Temp_Key_Name);
      end if;

      if (SE_Result = 0) then
         if  SSH_Message then
            Msg.Std ("Remote command: " & Command_In & " on " & Target & " successful");
         end if;
         Result := True;
      else
         -- A command in error does not necessarily mean an order error.
         -- One may want to test the non-existence of a file, which will
         -- nevertheless trigger a non-zero error code. Hence the error message
         -- in the exception handling block
         if SSH_Message then
            Msg.Err ("Net.Send_Command > Command error with: " & Command_String & " on " & Target & " Error: " & To_String (SE_Result));
         end if;
         if Set_Exception then
            raise Error_Command;
         end if;
      end if;

      return Result;

   end Command;

   procedure Command (Target : in String ; Command_In : in String) is
      Dummy : Boolean;
   begin
      Dummy := Command (Target, Command_In);
   end Command;

   ---------------------------------------------------------------------------
   function Copy_File (Target : in String ; File_Tx : in String; Directory_Rx : in String ; Options : in String := "") return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-copy_file-private.key";
      Command_String : String := "";
      Exec_Error : Integer;
      Key_To_Use : String := "";
      Result : Boolean := False;
   begin

      if not Empty (SSH_Key) then
         Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
         Key_To_Use := "-i " & Temp_Key_Name & " ";
      end if;

      -- scp <options> $file1 $file2 $fileN root@$host:$dir/[/distant_received_filename]
      Command_String := "scp " & Options & " " & SSH_Default_Options & Key_To_Use & File_Tx & " " &
                                 Target & ":" & Directory_Rx & (if SSH_Output then Null_String else STD_ERR_OUT_REDIRECT);

      -- Default distant directory creation for safety
      if not Net.Directory_Exists (Target, Directory_Rx) then
         Command (Target, "mkdir --parents " & Directory_Rx);
      end if;

      Sys.Shell_Execute (Command_String, Exec_Error);

      if not Empty (SSH_Key) then
         Fls.Delete_File (Temp_Key_Name);
      end if;

      if (Exec_Error = 0) then
         if  SSH_Message then
            Msg.Std ("Remote copy: " & File_Tx & " to " & Directory_Rx & " successful");
         end if;
         Result := True;
      else
         Msg.Err ("Net.Send_File > Copy error with: " & Command_String & " to " & Target & " Error: " & To_String (Exec_Error));
         if Set_Exception then
            raise Error_Copy_File;
         end if;
      end if;

      return Result;

   end Copy_File;

   procedure Copy_File (Target : in String ; File_Tx : in String; Directory_Rx : in String ; Options : in String := "") is
      Dummy : Boolean;
   begin
      Dummy := Copy_File (Target, File_Tx, Directory_Rx, Options);
   end Copy_File;

   ----------------------------------------------------------------------------
   function Delete_Directory_Tree (Target : in String ; Dir_Tree : String) return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-delete-directory-tree.key";
      Command_String : String := "";
      Exec_Error : Integer;
      Key_To_Use : String := "";
      Result : Boolean := False;
   begin
      if not Is_Root_Directory (Dir_Tree) then

         if not Empty (SSH_Key) then
            Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
            Key_To_Use := "-i " & Temp_Key_Name & " ";
         end if;

         Command_String := "ssh" & SSH_Default_Options &
                                 Key_To_Use & Target & " " &
                                 DQ & "rm -fr " & Dir_Tree & DQ & (if SSH_Output then Null_String else STD_ERR_OUT_REDIRECT);

         Sys.Shell_Execute (Command_String, Exec_Error);
         if (Exec_Error = 0) then
            Result := True;
         end if;

         if not Empty (SSH_Key) then
            Fls.Delete_File (Temp_Key_Name);
         end if;

      else
         Msg.Err ("Fls.Delete_Directory_Tree - Attempt to delete a root directory: " & Dir_Tree);
      end if;
      return Result;
   end Delete_Directory_Tree;

   ----------------------------------------------------------------------------
   function Delete_File (Target : in String ; File_To_Delete : in String) return Boolean is
      Result : Boolean := True;
   begin
      if File_Exists (Target, File_To_Delete) then
         Result := Net.Command (Target, "rm -- force " & File_To_Delete);
      end if;
      return Result;
   end Delete_File;

   procedure Delete_File (Target : in String ; File_To_Delete : in String) is
      Dummy : Boolean;
   begin
      Dummy := Delete_File (Target, File_To_Delete);
   end Delete_File;

   ---------------------------------------------------------------------------
   function Directory_Exists (Target : in String ; Name : String) return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-directory_exists-private.key";
      Command_String : String := "";
      Exec_Error : Integer;
      Key_To_Use : String := "";
      Result : Boolean := False;
   begin
      if not Empty (SSH_Key) then
         Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
         Key_To_Use := "-i " & Temp_Key_Name & " ";
      end if;

      Command_String := "ssh" & SSH_Default_Options &
                                 Key_To_Use & Target & " " &
                                 DQ & "test -d " & Name & DQ &
                             (if SSH_Output then Null_String else STD_ERR_OUT_REDIRECT);

      Sys.Shell_Execute (Command_String, Exec_Error);
      if (Exec_Error = 0) then
         Result := True;
      end if;

      if not Empty (SSH_Key) then
         Fls.Delete_File (Temp_Key_Name);
      end if;

      return Result;
   end Directory_Exists;

   ---------------------------------------------------------------------------
   function File_Exists (Target : in String ; Name : String) return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-file_exists-private.key";
      Command_String : String := "";
      Exec_Error : Integer;
      Key_To_Use : String := "";
      Result : Boolean := False;
   begin
      if not Empty (SSH_Key) then
         Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
         Key_To_Use := "-i " & Temp_Key_Name & " ";
      end if;

      Command_String := "ssh" & SSH_Default_Options &
                                 Key_To_Use & Target & " " &
                                 DQ & "test -f " & Name & DQ &
                             (if SSH_Output then Null_String else STD_ERR_OUT_REDIRECT);
      Sys.Shell_Execute (Command_String, Exec_Error);
      if (Exec_Error = 0) then
         Result := True;
      end if;

      if not Empty (SSH_Key) then
         Fls.Delete_File (Temp_Key_Name);
      end if;

      return Result;
   end File_Exists;

  ---------------------------------------------------------------------------
   function Get_Network_From_Ip (Ip : in String) return String is
      Result : String := "";
   begin
      if Is_Ip_Ok (Ip) then
         Result := Field_By_Index (Ip, 1, ".") & "." &
                   Field_By_Index (Ip, 2, ".") & "." &
                   Field_By_Index (Ip, 3, ".");
      end if;
      return Result;
   end Get_Network_From_Ip;

   ---------------------------------------------------------------------------
   function Is_Ip_Ok (Ip : in String) return Boolean is
      Ip_Part : String := "";
      Ip_Num : Natural;
      Result : Boolean := False;
   begin
      if not Empty (Ip) then
         if (Field_Count (Ip, ".") = 4) then
            for I in 1..4 loop
               Ip_Part := Field_By_Index (Ip, I, ".");
               if Is_Numeric (Ip_Part) then
                  Ip_Num := To_Integer (Ip_Part);
                  if not ((Ip_Num >= 0) and (Ip_Num <= 255)) then
                     exit;
                  end if;
                  if (I = 4) then
                     Result := True;
                  end if;
               else
                  exit;
               end if;
            end loop;
         end if;
      end if;
      return Result;
   end Is_Ip_Ok;

   ---------------------------------------------------------------------------
   function Is_Ping_Ok (Target : in String) return Boolean is
      SE_Result : Integer := 0;
      SE_Output : String := "";
   begin
      -- SE_Output is mandatory to capture (and suppress) PING output
      -- "1 packets transmitted, 1 received, 0% packet loss,"
      Sys.Shell_Execute ("ping -c 1 " & Target, SE_Result, SE_Output);
      return (SE_Result = 0);
   end Is_Ping_Ok;

   ----------------------------------------------------------------------------
   function Is_Root_Directory (Dir_Tree : String) return Boolean is
      Test_Dir_Tree : String := Dir_Tree;
      Slash : constant String := "/";
      Root_Dirs : constant String := "bin,boot,dev,etc,home,lib,lib32,lib64," &
         "libx32,lost+found,media,mnt,opt,proc,root,run,sbin,srv,sys," &
         "tmp,usr,var,Test_Delete_Directory_Tree";
      Result : Boolean := False;
   begin
      -- Dir_Tree must be fully qualified, ie starting with a slash (/)
      if Starts_With (Dir_Tree, Slash) then
         --  Add a slash if Dir_Tree does not ends with it
         if not Ends_With (Dir_Tree, Slash) then
            Test_Dir_Tree := Test_Dir_Tree & Slash;
         end if;
         --  If Dir_Tree counts only two slashes and at least one char between
         if (Char_Count (Test_Dir_Tree, Slash) = 2) and
            (Length (Dir_Tree) > 2) then
            Test_Dir_Tree := Slice (Test_Dir_Tree, 2, Length (Test_Dir_Tree) - 1);
            if not Empty (Field_By_Name (Root_Dirs, Test_Dir_Tree, Slash)) then
               Result := True;
            end if;
         end if;
      end if;
      return Result;
   end Is_Root_Directory;

   ----------------------------------------------------------------------------
   function Is_Ssh_Ok (Target : in String) return Boolean is
      SE_Result : Integer := 0;
   begin
      Sys.Shell_Execute ("ssh -q -o BatchMode=yes -o StrictHostKeyChecking=no -o ConnectTimeout=5 " & Target & " 'exit 0' ", SE_Result); -- , SE_Output);
      return (SE_Result = 0);
   end Is_Ssh_Ok;

   ----------------------------------------------------------------------------
   procedure Mount (Target : String) is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-mount-private.key";
      Mount_Point : constant String := v22.Get_Tmp_Dir & "v20-net-mount/" & Target;
      Exec_Error : Integer;
      Command_String : String := "";
      Key_To_Use : String := "";
   begin
      -- Default creation for safety
      Sys.Shell_Execute ("mkdir --parents " & Mount_Point & STD_ERR_OUT_REDIRECT, Exec_Error);
      if (Exec_Error = 0) then

         if not Empty (SSH_Key) then
            Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
            Key_To_Use := " -o IdentityFile=" & Temp_Key_Name & " ";
         end if;

         -- Mount
         Command_String := "sshfs " & Target & ":/ " & Mount_Point & Key_To_Use &
                                (if SSH_Output then Null_String else STD_ERR_OUT_REDIRECT);
         Sys.Shell_Execute (Command_String, Exec_Error);

         if not Empty (SSH_Key) then
            Fls.Delete_File (Temp_Key_Name);
         end if;

         if (Exec_Error = 0) then
            if  SSH_Message then
               Msg.Std (Target & " mounted in " & Mount_Point);
            end if;
         else
            Msg.Err ("v22.Net.Mount > Error mounting: " & Target & " in " & Mount_Point & " Error: " & To_String (Exec_Error));
            if Set_Exception then
               raise Error_Mount;
            end if;
         end if;
      else
         Msg.Err ("v22.Net.Mount > Error creating local mount point: " & Mount_Point & " Error: " & To_String (Exec_Error));
         if Set_Exception then
            raise Error_Mount;
         end if;
      end if;

   end Mount;

   ----------------------------------------------------------------------------
   function Mount_Remote (Remote_Host : String ; Target_To_Mount : String ; Mount_Point : String ; Mount_Options : in String := "") return Boolean is
      Result : Boolean := False;
   begin
      if Net.Command (Remote_Host, "mkdir --parents " & Mount_Point) then
         if Net.Command (Remote_Host, "mount " & Mount_Options & " " & Target_To_Mount & " " & Mount_Point) then
            Result := True;
         else
            Msg.Err ("v22.Net.Mount_Remote > Error mounting: " & Target_To_Mount & " linked to " & Mount_Point & " on " & Remote_Host);
         end if;
      else
         Msg.Err ("v22.Mount_Remote > Error creating mount point: " & Mount_Point & " targetting " & Target_To_Mount & " on " & Remote_Host);
      end if;
      return Result;
   end Mount_Remote;

   procedure Mount_Remote (Remote_Host : String ; Target_To_Mount : String ; Mount_Point : String ; Mount_Options : in String := "") is
      Dummy : Boolean;
   begin
      Dummy := Mount_Remote (Remote_Host, Target_To_Mount, Mount_Point, Mount_Options);
   end Mount_Remote;

   ---------------------------------------------------------------------------
   procedure Set_Exception (Set_Unset : Boolean := True) is
   begin
      SSH_Exception := Set_Unset;
   end Set_Exception;

   function Set_Exception return Boolean is
   begin
      return SSH_Exception;
   end Set_Exception;

   ---------------------------------------------------------------------------
   function Set_Hostname (Target : String ; Hostname : String) return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-directory_exists-private.key";
      Command_String : String := "";
      Exec_Error : Integer;
      Key_To_Use : String := "";
      Result : Boolean := False;
   begin
      if not Empty (SSH_Key) then
         Tio.Write_File (Temp_Key_Name, SSH_Key, "0600");
         Key_To_Use := "-i " & Temp_Key_Name & " ";
      end if;

      Command_String := "ssh" & SSH_Default_Options &
                                 Key_To_Use & Target & " " &
                                 DQ & "hostnamectl set-hostname " & Hostname & DQ &
                             (if SSH_Output then Null_String else STD_ERR_OUT_REDIRECT);

      Sys.Shell_Execute (Command_String, Exec_Error);
      if (Exec_Error = 0) then
         Result := True;
      end if;

      if not Empty (SSH_Key) then
         Fls.Delete_File (Temp_Key_Name);
      end if;

      return Result;
   end Set_Hostname;

   ---------------------------------------------------------------------------
   function Set_Key (Key : String := "") return Boolean is
      Temp_Key_Name : constant String := v22.Get_Tmp_Dir & Prg.Time_Stamp & "-v22-net-set_key-private.key";
      Exec_Error : Integer;
      Result : Boolean := False;
   begin
      Tio.Write_File (Temp_Key_Name, Key, "0600");
      Sys.Shell_Execute ("ssh-keygen -l -f " & Temp_Key_Name & STD_ERR_OUT_REDIRECT, Exec_Error);
      if (Exec_Error = 0) then
         SSH_Key := Key;
         Result := True;
      else
         Msg.Err ("v22.Net.Set_Key > Key invalid Error: " & To_String (Exec_Error));
      end if;
      Fls.Delete_File (Temp_Key_Name);
      return Result;
   end Set_Key;

   procedure Set_Key is
   begin
      SSH_Key := "";
   end Set_Key;

   ---------------------------------------------------------------------------
   procedure Set_Message (Msg : Boolean := True) is
   begin
      SSH_Message := Msg;
   end Set_Message;

   ---------------------------------------------------------------------------
   procedure Set_Output (Output : Boolean := True) is
   begin
      SSH_Output := Output;
   end Set_Output;

   ---------------------------------------------------------------------------
   procedure Unmount (Target : String) is
      Mount_Point : constant String := v22.Get_Tmp_Dir & "v20-net-mount/" & Target;
      Exec_Error : Integer;
   begin
      Sys.Shell_Execute ("umount " & Mount_Point & STD_ERR_OUT_REDIRECT, Exec_Error);
      if (Exec_Error = 0) then
         Msg.Std (Target & " unmounted from " & Mount_Point);
         Sys.Shell_Execute ("rmdir " & Mount_Point & STD_ERR_OUT_REDIRECT, Exec_Error);
         if (Exec_Error = 0) then
            if  SSH_Message then
               Msg.Std ("Delete local mount point: " & Mount_Point);
            end if;
         else
            Msg.Err ("v22.Net.Unmount > Error deleting local mount point: " & Mount_Point & " Error: " & To_String (Exec_Error));
            if Set_Exception then
               raise Error_Unmount;
            end if;
         end if;
      else
         Msg.Err ("v22.Net.Unmount > Error unmounting: " & Target & " from " & Mount_Point & " Error: " & To_String (Exec_Error));
         if Set_Exception then
            raise Error_Unmount;
         end if;
      end if;
   end Unmount;

   ----------------------------------------------------------------------------
   function Unmount_Remote (Remote_Host : String ; Mount_Point : String) return Boolean is
         Result : Boolean := False;
   begin
      if Net.Command (Remote_Host, "umount " & Mount_Point) then
         Net.Command (Remote_Host, "rmdir " & Mount_Point);
         if not Net.Directory_Exists (Remote_Host, Mount_Point) then
            Result := True;
         else
            Msg.Err ("v22.Net.Mount_Remote > Mount point: " & Mount_Point & " not deleted on " & Remote_Host);
         end if;
      else
         Msg.Err ("v22.Net.Mount_Remote > Mount point: " & Mount_Point & " not unmounted on " & Remote_Host);
      end if;
      return Result;
   end Unmount_Remote;

   procedure Unmount_Remote (Remote_Host : String ; Mount_Point : String) is
      Dummy : Boolean;
   begin
      Dummy := Unmount_Remote (Remote_Host, Mount_Point);
   end Unmount_Remote;

------------------------------------------------------------------------------
--- PUBLIC VARIABLES AND CONSTANTS
------------------------------------------------------------------------------

-- Tor     = Tor IP + Random download delay + Random download rate + Shuffle products
-- not Tor = Raw IP + 1s download delay + 200K download rate

--  Sys_Tor : Boolean := False;
--
--  Sys_Tor_Command : String := "";
--  Sys_Tor_Status : String := "";  -- RAW|TOR
--  Sys_Tor_Get_Counter : Integer := 0; -- Get counter
--  Sys_Tor_IP_Rotate : Integer := 50;  -- IP rotation each 50 Get
--
--  Sys_Random_Range : constant Integer := 5;  -- Rand (5) => 0..5 (6 values)
--  Sys_Random_Offset : constant Integer := 3; -- + 3 -> 3 to 8 seconds delay
--
--  Sys_Output   : constant String := Script_Temp & "sys.out";
--  Sys_Error    : constant String := Script_Temp & "sys.err";
--  Sys_Redirect : constant String := " 2>" & Sys_Error & " 1>" & Sys_Output; --2>/dev/null

------------------------------------------------------------------------------

--  Web_Get_File_Command : constant String := "curl";
--  Web_Get_File_Random_Range : constant Integer := 90;  -- Rand (90) => 0..90 (91 values)
--  Web_Get_File_Random_Offset : constant Integer := 90; -- + 90 -> 90 to 170 kbps
--
--  Web_Output   : constant String := Script_Temp & "web.out";
--  Web_Error    : constant String := Script_Temp & "web.err";
--  Web_Redirect : constant String := " 2>" & Web_Error & " 1>" & Web_Output; --2>/dev/null

------------------------------------------------------------------------------

--  TAB : constant String := To_String(Chr(9)); -- Tab
--  LF : constant String := To_String(Chr(12)); -- Line Feed
--  DQ : constant String := To_String(Chr(34)); -- Double quote
--
--  CD : constant String := ","; -- Comma delimiter
--  FD : constant String := ":"; -- Field delimiter
--  TD : constant String := "~"; -- Tilde delimiter
--  AD : constant String := "^"; -- Accent delimiter
--
--  -- Element - <p>blahblah</p>
--  TS : constant String := "<";  -- HTML start TAG of ELEMENT
--  TE : constant String := "</"; -- HTML end TAG of ELEMENT
--  TC : constant String := ">";  -- HTML close TAG of ELEMENT
--
--  -- Attribute - href="blahblah"
--  AS : constant String := "="; -- HTML start TAG of ATTRIBUTE
--  AE : constant String := DQ;   -- HTML end TAG of ATTRIBUTE
--  AC : constant String := DQ;   -- HTML close TAG of ATTRIBUTE

------------------------------------------------------------------------------
-- SYS FUNCTIONS
------------------------------------------------------------------------------

-- Sys_Tor_IP_Change : Tor IP change
-- Sys_Delay         : Get a file after a random delay (between 3..8s by default)
-- Sys_Exec          : Execute shell Command_String and return result

------------------------------------------------------------------------------
--procedure Sys_Tor_IP_Change is
------------------------------------------------------------------------------
-- Description : Tor IP change
-- Arguments   : None
-- Return      : None
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--
--  begin
--
--    if Exists (Web_Output) then
--      Delete_File (Web_Output);
--    end if;
--
--    if Exists (Web_Error) then
--      Delete_File (Web_Error);
--    end if;
--
--    Log_Msg (+"Changing Tor IP");
--
--    -- NEWNYM : Switch to clean circuits, so new application requests don't share any circuits with old ones.
--    -- Also clears the client-side DNS cache. (Tor MAY rate-limit its response to this signal.)
--    --
--    -- ANSWER : The server responds with "250 OK" if the signal is recognized (or simply closes the socket
--    -- if it was asked to close immediately), or "552 Unrecognized signal" if the signal is unrecognized.
--    --
--    -- Netcat EOL is CRLF. CR suppression by sed $'s/\r$//'
--    -- web.out :
--    -- 250 OK                 <--- fake passwd
--    -- 250 OK                 <--- IP change command
--    -- 250 closing connection <--- quit
--
--    Shell_Execute ("(echo authenticate '" & DQ & DQ & "'; echo signal newnym; echo quit) | nc localhost 9051 | sed $'s/\r$//'" & Web_Redirect, Exec_Error);
--    if (Exec_Error = 0) then
--      Log_Msg (+"Tor IP changed");
--    else
--      Log_Err (+"Sys_Tor_IP_Change - Error: " & Image (Exec_Error));
--    end if;
--
--  end Sys_Tor_IP_Change;

------------------------------------------------------------------------------
--procedure Sys_Delay is
------------------------------------------------------------------------------
-- Description : Random delay between 3..8s (by default)
-- Arguments   : See public variables Sys_Random_Range and Sys_Random_Offset
-- Return      : None
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--  Random_Value : String := "";
--  Random_Seed, Random_Result : Integer := 0;
--
--  begin
--
--    if Sys_Tor then
--      -- Training
--      Random_Seed := Rand (10) + 10;
--        for I in 1 .. Random_Seed loop
--         Random_Result := Rand (I);
--      end loop;
--
--      Random_Value := Image (Rand (Sys_Random_Range) + Sys_Random_Offset);
--      Log_Msg ("Random wait: " & Random_Value & "s");
--    else
--      Random_Value := "2";
--      Log_Msg ("Fixed wait: " & Random_Value & "s");
--    end if;
--
--    Shell_Execute ("sleep " & Random_Value, Exec_Error);
--    if (Exec_Error > 0) then
--      Log_Err (+"Sys_Delay - Error: " & Image (Exec_Error));
--    end if;
--
--  end Sys_Delay;


------------------------------------------------------------------------------
--- WEB FUNCTIONS
------------------------------------------------------------------------------

-- Web_Get_Buffer          : Get a Buffer from Url without delay
-- Web_Get_File            : Get a file from Url after a random delay (between 3..8s by default)
-- Web_Read_File           : Read a file and returning it in a buffer without EOL character
-- Web_Get_Html_Value      : Return a Keyword value in a Line starting at a strict Trigger. Handle HTML tags & attributes
-- Web_Get_Html_Value_List : Iterative multiple values handler through a buffer starting at a strict. Handle HTML tags & attributes

-- HTML Vocabulary -----------------------------------------------------------
--
-- ELEMENT :
-- <p>This is the content of the paragraph element</p> (could be on more than one line)
-- TAGS of element : <p> and </p>
-- VALUE of element : This is the content of the paragraph element.
--
-- ATRIBUTES of ELEMENT
-- <a href="/this/is/a/link/">Learn about the a href attribute</a>
-- TAGS of element : <a href="/this/is/a/link/" and </a>
-- VALUE of element : Learn about the a href attribute
-- ATTRIBUTE of element : href
-- ATRIBUTE VALUE of element : /this/is/a/link/

------------------------------------------------------------------------------
--function Web_Get_Buffer (Url : String ; EOL_Char : String) return String is
------------------------------------------------------------------------------
-- Description : Get a Buffer from Url without delay
-- Arguments   : Url
-- Return      : Buffer
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--  File_Handle : File_Type;
--  Line_Buffer : String := "";
--  Result_Buffer : String := "";
--
--  begin
--
--    Shell_Execute (Sys_Tor_Command &
--                   Web_Get_File_Command & " " &
--                   "--location " & -- follow redirections
--                   "--silent " &   -- follow redirections
--                   "--output " &   -- file name output
--                   DQ & Script_Temp & "get_buffer" & DQ & " " &
--                   Url, Exec_Error);
--
--    if (Exec_Error = 0) then
--      if Exists (Script_Temp & "get_buffer") then
--
--        Open (File_Handle, Script_Temp & "get_buffer");
--        while not (End_Of_File (File_Handle)) loop
--          Get_Line (File_Handle, Line_Buffer);
--          Result_Buffer := Result_Buffer & Line_Buffer & EOL_Char;
--        end loop;
--        Close (File_Handle);
--
--      else
--        Log_Err (+"Web_Get_Buffer - Can't open Get buffer file");
--      end if;
--    else
--      Log_Err (+"Web_Get_Buffer - Error: " & Image (Exec_Error));
--    end if;
--
--    return Result_Buffer;
--
--  end Web_Get_Buffer;

------------------------------------------------------------------------------
--function Web_Get_File (Url : String ; File_Output : String) return Boolean is
------------------------------------------------------------------------------
-- Description : Get a file from Url after a random delay (between 3..8s by default)
-- Arguments   : Url
-- Return      : None
------------------------------------------------------------------------------

--  Exec_Error : Integer := 0;
--  Result : Boolean := True;
--  IP_Value : String := Web_Get_Buffer (+"https://api.ipify.org",+"");
--  Random_Value : String := "";
--  Random_Seed, Random_Result : Integer := 0;
--
--  begin
--
--    -- File backup
--
--    Fls_Backup_Name (File_Output);
--
--    -- IP Rotation
--    if Sys_Tor then
--      Sys_Tor_Get_Counter := Sys_Tor_Get_Counter + 1;
--      if ((Sys_Tor_Get_Counter mod Sys_Tor_IP_Rotate) = 0) then
--        Sys_Tor_Get_Counter := 1;
--        Sys_Tor_IP_Change;
--      end if;
--    end if;
--
--    -- Random delay before getting file
--    if not Web_Test_No_Download then
--      Sys_Delay;
--    end if;
--
--    if Sys_Tor then
--      -- Compute download rate with preliminary training
--      Random_Seed := Rand (10) + 10;
--        for I in 1 .. Random_Seed loop
--         Random_Result := Rand (I);
--      end loop;
--      Random_Value := Image (Rand (Web_Get_File_Random_Range) + Web_Get_File_Random_Offset);
--    else
--      Random_Value := "200";
--    end if;
--
--    Log_Msg (Sys_Tor_Status & ":" & IP_Value & "@" & Random_Value & "kbps - Get " & Url & " > " & File_Output);
--
--    -- Tor Ip rotation each Sys_Tor_IP_Rotate rounds
--    if Sys_Tor then
--      Log_Msg ("Next Ip rotation in: " & Image(Sys_Tor_IP_Rotate - Sys_Tor_Get_Counter));
--    end if;
--
--    -- Download
--
--    Shell_Execute (Sys_Tor_Command &
--                   Web_Get_File_Command & " " &
--                   "--location " &                          -- follow redirections
--                   "--limit-rate " & Random_Value & "k " &  -- random rate limit
--                   "--output " &                            -- file name output
--                   DQ & File_Output & DQ & " " &
--                   DQ & Url & DQ, Exec_Error);
--
--    if (Exec_Error > 0) then
--      Log_Err (+"Web_Get_File - Error: " & Image (Exec_Error));
--      Result := False;
--    end if;
--
--    return (Result and Exists (File_Output));
--
--  end Web_Get_File;

------------------------------------------------------------------------------
--function Web_Read_File (File_To_Read : String) return String is
------------------------------------------------------------------------------
-- Description : Read a file and returning it in a buffer without EOL character
-- Arguments   : File_To_Read
-- Return      : Buffer's file
------------------------------------------------------------------------------

--  File_Handle : File_Type;
--  Line_Buffer : String := "";
--  Result_Buffer : String := "";
--
--  begin
--
--    if Exists (File_To_Read) then
--      Open (File_Handle, File_To_Read);
--      while not (End_Of_File (File_Handle)) loop
--        Get_Line (File_Handle, Line_Buffer);
--        Result_Buffer := Result_Buffer & Line_Buffer;
--      end loop;
--      Close (File_Handle);
--    else
--      Log_Err (+"Web_Read_File - File does not exist: " & File_To_Read);
--    end if;
--
--    return Result_Buffer;
--
--  end Web_Read_File;

------------------------------------------------------------------------------
--function Web_Get_Html_Value (Line : String; Trigger : String; Keyword : String) return String is
------------------------------------------------------------------------------
-- Description : Return a Keyword value in a Line containing strict Trigger. Handle HTML tags & attributes
-- Arguments   : Line, Trigger, Keyword
-- Return      : Tag value
------------------------------------------------------------------------------

--  Buffer_String : String := Line;
--  Buffer_length : Natural := Length (Buffer_String);
--  Trigger_length : Natural := Length (Trigger);
--  Keyword_length : Natural := Length (Keyword);
--  Result_String, Result_Char : String := "";
--
--  Start_Index, Start_Trigger, TE_Index : Natural := 0;
--
--  Element_List : String := "a:li:p:span";
--  Attribute_List : String := "alt:class:href:title";
--
--  type States is (S_Idle, S_Wait_For_TC, S_Wait_For_TE);
--  State : States := S_Idle;
--
--  begin
--
--  -- TS : "<"  HTML start TAG of ELEMENT
--  -- TE : "</" HTML end TAG of ELEMENT
--  -- TC : ">"  HTML close TAG of ELEMENT
--
--    --Log_Dbg ("Buffer: " & Buffer_String);
--    Log_Dbg ("Buffer length: " & Image (Buffer_Length));
--    Log_Dbg ("Trigger: " & DQ & Trigger & DQ);
--    Log_Dbg ("Keyword: " & Keyword);
--
--    Start_Trigger := Index (Buffer_String, DQ & Trigger & DQ);
--    if (Start_Trigger > 0) then
--
--      Buffer_String := Slice (Buffer_String, Start_Trigger + Trigger_length + 1, Buffer_Length);
--      Buffer_Length := Length (Buffer_String);
--
--      Log_Dbg ("Trigger Index: " & Image (Start_Trigger));
--      Log_Dbg ("Trigger Output: " & Slice (Buffer_String, 1, Trigger_length + 1));
--
--      if Vst_Search_Field (Element_List, Keyword, FD) then
--
--        -- Element value
--
--        Log_Dbg (+"Keyword: Element");
--
--        Start_Index := Index (Buffer_String, TS & Keyword);
--        if (Start_Index > 0) then
--
--          Log_Dbg ("Keyword Index: " & Image (Start_Index));
--
--          State := S_Wait_For_TC;
--
--          for I in (Start_Index + length (TS) + Keyword_length) .. Buffer_Length loop
--            Result_Char := Slice (Buffer_String, I, I);
--            if State = S_Wait_For_TC then
--              if Result_Char = TC then
--                State := S_Wait_For_TE;
--              end if;
--            elsif State = S_Wait_For_TE then
--              Result_String := Result_String & Result_Char;
--              TE_Index := Index_Backward (Result_String, TE & Keyword & TC);
--              if (TE_Index > 1) then
--                Result_String := Slice (Result_String, 1, TE_Index - 1);
--                exit;
--              end if;
--            end if;
--          end loop;
--
--        end if;
--
--      else
--
--        -- Attribute value
--
--        Log_Dbg (+"Keyword: Attribute");
--
--        Start_Index := Index (Buffer_String, Keyword & "=" & DQ);
--        if (Start_Index > 0) then
--
--          -- + 2 because 2 DQ (double quote) around Keyword in Buffer_String...
--          if ((Start_Index + Keyword_length + 2) < Buffer_Length) then
--            for I in (Start_Index + Keyword_length + 2) .. Buffer_Length loop
--              Result_Char := Slice (Buffer_String, I, I);
--              if Result_Char = DQ then
--                exit;
--              else
--                Result_String := Result_String & Result_Char;
--              end if;
--            end loop;
--          end if;
--        end if;
--
--        if Result_Char /= DQ then
--           Result_String := "";
--        end if;
--
--      end if;
--    end if;
--
--    Log_Dbg (Result_String);
--
--    return Result_String;
--
--  end Web_Get_Html_Value;

-- <p class="show-for-sr">MEREROUKA 2 o40</p>

------------------------------------------------------------------------------
--  procedure Web_Get_Html_Value_List (Buffer : String;              -- Page to process
--                                     Trigger : String;             -- Identifier where to start
--                                     Keyword : String;             -- Element or Attribute
--                                     Div_Counter : in out Natural;  -- <div ...> </div> counter for parse break
--                                     Buffer_Index : in out Natural; -- Current buffer index to resume at next call
--                                     Result_String : out String)   -- Value found
--                                     is
------------------------------------------------------------------------------
-- Description : Iterative multiple values handler through a buffer starting at a strict. Handle HTML tags & attributes
-- Arguments : See above
-- Return : n/a
--
-- Todo : replace Keyword by Element and Attribute to process string like  :
-- <p class="show-for-sr">MEREROUKA 2 o40</p>
--  ^        ^            ^
--  |        +- Attribute +- Value
--  +- Element
--
-- Element - <p>blabla</p>
-- TS := "<"  HTML start TAG of ELEMENT
-- TE := "</" HTML end TAG of ELEMENT
-- TC := ">"  HTML close TAG of ELEMENT
--
-- Attribute - href="blabla"
-- AS : "=" HTML start TAG of ATTRIBUTE
-- AE : DQ  HTML end TAG of ATTRIBUTE
-- AC : DQ  HTML close TAG of ATTRIBUTE
------------------------------------------------------------------------------

--  Buffer_String : String := Buffer;
--  Result_Char, Trigger_Keyword, Trigger_Keyword_End, Trigger_Value_End : String := "";
--
--  Buffer_Length : Natural := Length (Buffer_String);
--  Keyword_Length : Natural := Length (Keyword);
--
--  Start_Index, Start_Trigger, Start_Iterate, End_Index : Natural := 0;
--  Element_List : String := "a:li:p:span";
--  Attribute_List : String := "alt:class:href:title";
--
--  type States is (Search_TS_AS, Search_TE_AE, Search_TC_AC);
--  State : States := Search_TS_AS;
--
--  Pattern_Buffer : String := "";
--  Pattern_Div_Open : String := "<div ";
--  Pattern_Div_Close : String := "</div>";
--
--  Keyword_Element : Boolean := Vst_Search_Field (Element_List, Keyword, FD);
--
--  begin
--
--    --Log_Dbg ("Buffer: " & Buffer_String);
--    Log_Dbg ("Buffer length: " & Image (Buffer_Length));
--    Log_Dbg ("Trigger: " & DQ & Trigger & DQ);
--    Log_Dbg ("Keyword: " & Keyword);
--
--    Log_Dbg ("Buffer_Index: " & Image (Buffer_Index));
--
--    if (Buffer_Index = 0) then
--      -- First call : seek to Trigger
--      Start_Trigger := Index (Buffer_String, DQ & Trigger & DQ);
--      Start_Iterate := Start_Trigger + Length (DQ & Trigger & DQ) + 1;
--    else
--      -- Next calls : resume at the previous exit
--      Start_Trigger := Buffer_Index;
--      Start_Iterate := Start_Trigger;
--    end if;
--
--    Log_Dbg ("Start Trigger/Index: " & Image (Start_Trigger));
--
--    -- Reach Trigger in page
--
--    if (Start_Trigger > 0) then
--
--      if (Start_Iterate > Buffer_Length) then
--        Start_Iterate := Buffer_Length;
--      end if;
--
--      if (Buffer_Index = 0) then
--        Log_Dbg ("TRIGGER FOUND: " & DQ & Trigger & DQ);
--      end if;
--
--      -- Element or Attribute presets
--
--      if Keyword_Element then
--        Log_Dbg (+"Element");
--        Trigger_Keyword := TS & Keyword & " ";
--        Trigger_Keyword_End := TC;
--        Trigger_Value_End := TE & Keyword & TC;
--      else
--        Log_Dbg (+"Attribute");
--        Trigger_Keyword := Keyword & AS;
--        Trigger_Keyword_End := AE;
--        Trigger_Value_End := AC;
--      end if;
--
--      Result_String := "";
--
--      -- Iterate
--
--      for I in Start_Iterate .. Buffer_Length loop
--
--        Result_String := Result_String & Slice (Buffer_String, I, I);
--
--        -- <div>.. </div> counting/decounting
--
--        if (Length (Result_String) >= Length (Pattern_Div_Open)) then
--          Pattern_Buffer := Slice (Result_String, Length (Result_String) - Length (Pattern_Div_Open) + 1, Length (Result_String));
--          if (Pattern_Buffer = Pattern_Div_Open) then
--            Div_Counter := Div_Counter + 1;
--            Log_Dbg ("Div_Counter incremented: " & Image (Div_Counter));
--          end if;
--        end if;
--
--        if (Length (Result_String) >= Length (Pattern_Div_Close)) then
--          Pattern_Buffer := Slice (Result_String, Length (Result_String) - Length (Pattern_Div_Close) + 1, Length (Result_String));
--          if (Pattern_Buffer = Pattern_Div_Close) then
--            Div_Counter := Div_Counter - 1;
--            Log_Dbg ("Div_Counter decremented: " & Image (Div_Counter));
--            if Div_Counter = 0 then
--              Log_Dbg (+"Exit by <div> break");
--              exit;
--            end if;
--          end if;
--        end if;
--
--        -- Processing string
--
--        if State = Search_TS_AS then
--          End_Index := Index_Backward (Result_String, Trigger_Keyword);
--          if (End_Index > 0) then
--            State := Search_TE_AE;
--            Result_String := "";
--            Log_Dbg ("KEYWORD FOUND: " & Trigger_Keyword);
--          end if;
--        elsif State = Search_TE_AE then
--          End_Index := Index_Backward (Result_String, Trigger_Keyword_End);
--          if (End_Index > 0) then
--            State := Search_TC_AC;
--            Result_String := "";
--            Log_Dbg ("VALUE REACHED: " & Trigger_Keyword_End);
--            Log_Dbg (+"Start recording 'value'");
--          end if;
--        elsif State = Search_TC_AC then
--          End_Index := Index_Backward (Result_String, Trigger_Value_End);
--          if (End_Index > 0) then
--            Result_String := Slice (Result_String, 1, End_Index - 1);
--            if (Length (Result_String) > 0) then
--              Buffer_Index := I + 1;
--              Log_Dbg ("VALUE FOUND: " & Result_String);
--              Log_Dbg (+"End recording 'value'");
--              Log_Dbg (+"Exit by 'value' returned");
--              Log_Dbg (+"Exit Index: " & Image (Buffer_Index));
--              exit;
--            else
--              State := Search_TS_AS;
--              Result_String := "";
--              Log_Dbg (+"VALUE EMPTY and DISCARDED, resume KEYWORD SEARCH");
--            end if;
--          end if;
--
--        end if;
--
--      end loop;
--
--    end if;
--
--  -- pause
--
--  end Web_Get_Html_Value_List;


   -- See gnx-instance
   --  ---------------------------------------------------------------------------
   --  procedure Exception_Termination (Message : String) is
   --  begin
   --     Log.Err (Message & ": exception raised, program end");
   --     Log.Title ("");
   --     GOL.OS_Exit (100);
   --  end Exception_Termination;

-------------------------------------------------------------------------------
end v22.Net;
-------------------------------------------------------------------------------
