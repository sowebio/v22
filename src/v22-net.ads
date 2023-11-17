-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-net.ads
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

--  with Ada.Calendar;
--  with Ada.Command_Line;
--  with Ada.Directories;

with GNAT.OS_Lib;

with v22.Uxs; use v22.Uxs;

package v22.Net is

   package GOL renames GNAT.OS_Lib;

   function Command (Target : in String ; Command_In : in String ; SE_Output : out String) return Boolean;
   function Command (Target : in String ; Command_In : in String) return Boolean;
   procedure Command (Target : String ; Command_In : String);
   --  Send remote command to host. Returns True if command successful (remote
   --  exitcode equal to 0).

   function Copy_File (Target : in String ; File_Tx : in String; Directory_Rx : in String ; Options : in String := "") return Boolean;
   procedure Copy_File (Target : in String ; File_Tx : in String; Directory_Rx : in String ; Options : in String := "");
   -- Copy to distant host. Returns True if copy successful.

   function Delete_Directory_Tree (Target : in String ; Dir_Tree : String) return Boolean;
   --  Delete a directory tree Dir_Tree. The directory and all of its contents
   --  (possibly including other directories) are deleted but adding a '*' at
   --  the end of the path preserve the last directory of the path (/one/two/
   --  deletes two but /on/two/* preserve two.
   --
   --  Return True if Dir_Tree is successfully deleted or was already deleted.
   --  Return False if operation is unsuccessful (i.e. if base directory tree
   --  was non existent or still exists after the deleting attempt).
   --
   --  Dir_Tree must be fully qualified, ie starting with a slash (/).
   --  This function prevents deletion of the following root directories (see
   --  Is_Root_Directory for further details). Pay close attention, you can't
   --  delete /etc but you are allowed to delete /etc/network !

   function Delete_File (Target : in String ; File_To_Delete : in String) return Boolean;
   procedure Delete_File (Target : in String ; File_To_Delete : in String);
   -- Remove File_To_Delete in remote host Target. Returns True if delete successful.

   function Directory_Exists (Target : in String ; Name : String) return Boolean;
   --  Returns True if distant directory Name exists.

   function File_Exists (Target : in String ; Name : String) return Boolean;
   --  Returns True if distant file Name exists.

   function Get_Exception return On_Off;
   --  Returns Exception status.

   function Get_Network_From_Ip (Ip : in String) return String;
   --  Returns the network part of a /32 classless IP address.

   function Is_Ip_Ok (Ip : in String) return Boolean;
   --  Ip validation.

   function Is_Ping_Ok (Target : in String) return Boolean;
   -- Return true if target answer to a ping.

   function Is_Root_Directory (Dir_Tree : String) return Boolean;
   --  This function checks the following root directories: bin, boot, dev,
   --  etc, home, lib, lib32, lib64, libx32, lost+found, media, mnt, opt,
   --  proc, root, run, sbin, srv, sys, tmp, usr, var. Returns True if
   --  Dir_Tree is a root directory. Dir_Tree must be fully qualified, ie
   --  starting with a slash (/).

   function Is_Ssh_Ok (Target : in String) return Boolean;
   --  Return true if target accepts a valid SSH connexion.

   procedure Mount (Target : String);
   --  Mount a host

   function Mount_Remote (Remote_Host : String ; Target_To_Mount : String ; Mount_Point : String ; Mount_Options : in String := "") return Boolean;
   procedure Mount_Remote (Remote_Host : String ; Target_To_Mount : String ; Mount_Point : String ; Mount_Options : in String := "");
   --  Mount a Mount_Point targetting Target_To_Mount on Remote_Host with
   --  options Mount_Options. All mount options are accepted. Returns true if
   --  operation is successful.

   procedure Set_Exception (Switch : On_Off := On);
   -- Enable Exception processing, which is disabled by default.

   function Set_Hostname (Target : String ; Hostname : String) return Boolean;
   -- Set Hostname for a Target host. Returns true if command ok.

   function Set_Key (Key : String := "") return Boolean;
   -- Set SSH private key used to log in distant hosts with commands like
   -- Send_Command and Send_File. Key validity is checked. Returns true if
   -- Key is properly set.

   procedure Set_Key;
   -- Delete the key previously set.

   procedure Set_Message (Switch : On_Off := On);
   -- Control console message when using commands like Send_Command and
   -- Send_File. Default is console message enable. A call without parameter
   -- enable console output.

   procedure Set_Output (Switch : On_Off := On);
   -- Control console output when using commands like Send_Command and
   -- Send_File. Default is console output enable. A call without parameter
   -- enable console output.

   procedure Unmount (Target : String);
   -- Unmount a host.

   function Unmount_Remote (Remote_Host : String ; Mount_Point : String) return Boolean;
   procedure Unmount_Remote (Remote_Host : String ; Mount_Point : String);
   --  Unmount a Mount_Point on a Remote_Host. Mount_Point is then deleted.
   --  Returns true if the whole operation is successful.

   Error_Mount : exception;
   --  Raised when mount error.

   Error_Command : exception;
   --  Raised when send command error.

   Error_Copy_File : exception;
   --  Raised when send file error.

   Error_Unmount : exception;
   --  Raised when unmount error.

   -- Exception handling example

   -- procedure Exception_Termination (Message : String);
   -- Program termination handling

   -------------------------------------------------------------------------
   --  procedure xxx is
   --
   --  begin
   --
   --     Net.Command (+"",+"");
   --
   --  exception
   --
   --     when Net.Error_Mount =>
   --        Net.Exception_Termination ("Instance.Mount");
   --     when Net.Error_Unmount =>
   --        Net.Exception_Termination ("Instance.Unmount");
   --     when Net.Error_Command =>
   --        Net.Exception_Termination ("Instance.Send_Command");
   --     when Net.Error_Copy =>
   --        Net.Exception_Termination ("Instance.Send_File");
   --     --when Net.Error : others =>
   --     --   Net.Exception_Termination ("Instance.Unregistered");
   --
   --  end xxx;

------------------------------------------------------------------------------
private

   SSH_Default_Options : constant String := " -q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null ";
   SSH_Exception : On_Off := Off;
   SSH_Key : String := "";
   SSH_Message : On_Off := On;
   SSH_Output :  On_Off := On;

   SSH_Temp_Command_Key : String := "-v22-net-command-private.key";
   --  SSH_Temp_Command_Output : String := "-v22-net-command.output";
   --  SSH_Temp_Delete_Directory : String := "-v22-net-delete-directory-tree.key";
   --  SSH_Temp_Mount_Key : String := "-v22-net-mount-private.key";

-------------------------------------------------------------------------------
end v22.Net;
-------------------------------------------------------------------------------
