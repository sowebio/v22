-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-net.ads
--  @copyright See authors list below and README.md file
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

with GNAT.OS_Lib;
with GNAT.SHA1;

with GNATCOLL.JSON;

with v22.Fls;
with v22.Msg;
with v22.Prg;
with v22.Sys;
with v22.Tio;
with v22.Uxs; use v22.Uxs;

package v22.Net is

   package GOL renames GNAT.OS_Lib;
   package GJ renames GNATCOLL.JSON;

   type Api_Providers is (None, Ovh, Matomo);
   type Api_Methods is (None, Get, Put, Post, Delete);

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   function Api (Api_Provider : Api_Providers;
                 Api_End_Point : String;
                 Api_Consumer_Key : String;
                 Api_Application_Key : String := "";
                 Api_Application_Secret : String := "";
                 Api_Method : Api_Methods;
                 Api_Query : String := "";
                 Api_Body : String := ""
                ) return String;
   --  Communication with API

   function Command (Target : String; Command_In : String; SE_Output : out String) return Boolean;
   function Command (Target : String; Command_In : String) return Boolean;
   procedure Command (Target : String; Command_In : String);
   --  Send remote command to host. Returns True if command successful (remote
   --  exitcode equal to 0).

   function Copy_File (Target : String; File_Tx : String; Directory_Rx : String; Options : String := "") return Boolean;
   procedure Copy_File (Target : String; File_Tx : String; Directory_Rx : String; Options : String := "");
   --  Copy to distant host. Returns True if copy successful.
   --  Options allows extra parameters, like -r for recursive copy.

   function Copy_Rsync (Target : String; Directory_Tx : String; Directory_Rx : String; Excludes_Directories : String := "") return Boolean;
   procedure Copy_Rsync (Target : String; Directory_Tx : String; Directory_Rx : String; Excludes_Directories : String := "");
   --  Rsync efficient copy to distant host with unlimited excluding masks.
   --  Returns True if copy successful.

   function Delete_Directory_Tree (Target : String; Dir_Tree : String) return Boolean;
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

   function Delete_File (Target : String; File_To_Delete : String) return Boolean;
   procedure Delete_File (Target : String; File_To_Delete : String);
   --  Remove File_To_Delete in remote host Target. Returns True if delete successful.

   function Directory_Exists (Target : String; Name : String) return Boolean;
   --  Returns True if distant directory Name exists.

   function File_Exists (Target : String; Name : String) return Boolean;
   --  Returns True if distant file Name exists.

   function Get_Exception return On_Off;
   --  Returns Exception status.

   function Get_Network_From_Ip (Ip : String) return String;
   --  Returns the network part of a /32 classless IP address.

   function Is_Ip_Ok (Ip_In : String) return Boolean;
   --  Ip validation.

   function Is_Ping_Ok (Target : String) return Boolean;
   --  Return true if target answer to a ping.

   function Is_Root_Directory (Dir_Tree : String) return Boolean;
   --  This function checks the following root directories: bin, boot, dev,
   --  etc, home, lib, lib32, lib64, libx32, lost+found, media, mnt, opt,
   --  proc, root, run, sbin, srv, sys, tmp, usr, var. Returns True if
   --  Dir_Tree is a root directory. Dir_Tree must be fully qualified, ie
   --  starting with a slash (/).

   function Is_Ssh_Ok (Target : String) return Boolean;
   --  Return true if target accepts a valid SSH connexion.

   procedure Mount (Target : String);
   --  Mount a host

   function Mount_Remote (Remote_Host : String; Target_To_Mount : String; Mount_Point : String; Mount_Options : String := "") return Boolean;
   procedure Mount_Remote (Remote_Host : String; Target_To_Mount : String; Mount_Point : String; Mount_Options : String := "");
   --  Mount a Mount_Point targetting Target_To_Mount on Remote_Host with
   --  options Mount_Options. All mount options are accepted. Returns true if
   --  operation is successful.

   function Send_Mail (From : String; To : String; Subject : String; Body_Text : String; Sender : String := "") return Boolean;
   --  Send a email
   --  Sender: Notification from sender
   --  From email: no-reply@sender.com
   --  To email: bob@receiver.com
   --  Subject: test email
   --  2 lines text: blabla<CR>blibli
   --  echo 'blabla<CR>blibli' | mail --append="FROM:Sender <no-reply@tx.com>" --subject="test email" bob@rx.com

   function Send_Sms (Api_Provider : Api_Providers;
                      Api_End_Point : String;
                      Api_Consumer_Key : String;
                      Api_Application_Key : String;
                      Api_Application_Secret : String := "";
                      Sms_Account : String;
                      Sms_Sender : String;
                      Sms_Receivers_List : String;
                      Sms_Message : String := ""
                     ) return String;
   --  Send a SMS
   --  The sender must be registered against the SMS sending service.
   --  Accepts UTF-8 encoding. Use <LF> to move to the next line.
   --  Returns a hoster dependant result string; For OVH, by example: Result : {"ids":[572940644,572940647],
   --  "tag":"ja0jfkwkqqj38d4r","validReceivers":["+336***","+336***"],"totalCreditsRemoved":6,"invalidReceivers":[]}

   procedure Set_Exception (Switch : On_Off := On);
   --  Enable Exception processing, which is disabled by default.

   function Set_Hostname (Target : String; Hostname : String) return Boolean;
   --  Set Hostname for a Target host. Returns true if command ok.

   function Set_Key (Key : String := "") return Boolean;
   --  Set SSH private key used to log in distant hosts with commands like
   --  Send_Command and Send_File. Key validity is checked. Returns true if
   --  Key is properly set.

   procedure Set_Key;
   --  Delete the key previously set.

   procedure Set_Message (Switch : On_Off := On);
   --  Control console message when using commands like Send_Command and
   --  Send_File. Default is console message enable. A call without parameter
   --  enable console output.

   procedure Set_Output (Switch : On_Off := On);
   --  Control console output when using commands like Send_Command and
   --  Send_File. Default is console output enable. A call without parameter
   --  enable console output.

   procedure Set_Password (Password : String := "");
   --  Set SSH private password used to log in distant hosts with commands like
   --  Send_Command and Send_File.

   procedure Set_Password;
   --  Delete the password previously set.

   procedure Unmount (Target : String);
   --  Unmount a host.

   function Unmount_Remote (Remote_Host : String; Mount_Point : String) return Boolean;
   procedure Unmount_Remote (Remote_Host : String; Mount_Point : String);
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

   --  Exception handling example

   --  procedure Exception_Termination (Message : String);
   --  Program termination handling

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

   Rsync_Default_Options : constant String := SP & "--verbose --progress -e" & SP & DQ & "ssh -q -o StrictHostKeyChecking=no" & DQ;
   SSH_Default_Options : constant String := SP & "-q -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null" & SP;
   SSH_Exception : On_Off := Off;
   SSH_Key : String := "";
   SSH_Password : String := "";
   SSH_Message : On_Off := On;
   SSH_Output :  On_Off := On;
   SSH_Temp_Command_Key : String := "-v22-net-command-private.key";

   --  SSH_Temp_Command_Output : String := "-v22-net-command.output";
   --  SSH_Temp_Delete_Directory : String := "-v22-net-delete-directory-tree.key";
   --  SSH_Temp_Mount_Key : String := "-v22-net-mount-private.key";

-------------------------------------------------------------------------------
end v22.Net;
-------------------------------------------------------------------------------
