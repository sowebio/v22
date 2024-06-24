-------------------------------------------------------------------------------
-- 
--  _|      _|    _|_|      _|_|    
--  _|      _|  _|    _|  _|    _| 
--  _|      _|      _|        _|    
--    _|  _|      _|        _|      
--      _|      _|_|_|_|  _|_|_|_|  
--
--  @file      v22-fls.ads
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - File System package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Exceptions;

with v22.Uxs; use v22.Uxs;

package v22.Fls is

   package AD renames Ada.Directories;
   subtype FKind is AD.File_Kind;
   
   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------   
   
   procedure Backup_File (File_To_Backup : String);
   -- Rename file with .bak.n suffix. Iterate n=0..9 searching a free n bak 
   -- file. If n is free then write .bak.n, if n=9, delete .bak.0

   procedure Copy_File (Source_Name, Target_Name : String);
   --  Copy a Source_Name file to a Target_Name file destination with no
   --  attributes preservation and overwrite file if exists.
   
   procedure Copy_Directory_Tree (Dir_Tree_Origin : String; Dir_Tree_Destination : String);
   --  Copy a directory tree with no attributes preservation and overwrite
   --  file if exists.

   function Create_Directory_Tree (Dir_Tree : String) return Boolean;
   --  Create a directory tree Dir_Tree. Each non-existent directory named by
   --  Dir_Tree is created (possibly including other intermediate directories).
   --  Return False if operation is unsuccessfull (i.e. if base directory tree
   --  is unconsistent or still don't exist after the creating attempt). Extra
   --  inner slashes are processed by runtime. Return True if directory tree 
   --  already exists.

   function Delete_Directory_Tree (Dir_Tree : String) return Boolean;
   procedure Delete_Directory_Tree (Dir_Tree : String);
   
   --  Delete a directory tree Dir_Tree. The directory and all of its contents
   --  (possibly including other directories) are deleted. Return True if
   --  Dir_Tree is successfully deleted or was already deleted. Return False if
   --  directory still exists after the deleting attempt). 
   --
   --  Dir_Tree must be fully qualified, ie starting with a slash (/).
   --
   --  This function prevents deletion of the following root directories (see
   --  Is_Root_Directory for further details). Pay close attention, you can't
   --  delete /etc but you are allowed to delete /etc/network !
   --
   --  /!\ This function uses Ada.Directories.Delete_Tree, which raises an 
   --  exception if the directory tree to delete contains a *broken* symbolic
   --  link (a file like any other). This latter is seen as *non-existent*
   --  and, when the parent directory is deleted, an exception occurs : 
   --  raised ADA.IO_EXCEPTIONS.USE_ERROR : directory tree rooted at
   --  <directory tree> could not be deleted (because *not empty*). Funny, but
   --  not so much. Pure C code problem in Ada RTS. Stacked C calls in russian
   --  puppet mode until a logical problem arises.

   procedure Delete_File (Name : String);
   --  Delete a Name file only if this latter exists. No exception wil be
   --  raised if the file to delete does not exists.

   procedure Delete_Lines (File_Name, Pattern : String);
   --  Search and remove file lines matching Pattern in File_Name.
   
   function Download_File (Url : String;
                           Dlfile : String;
                           Name : String := "";
                           DlSize : Integer := 0) return Boolean;
   --  Download to Dlfile. Don't download if Dlfile already exists with its 
   --  Size equals DlSize. Name is used to named file in text messages and is 
   --  purely informational. If Name empty, use Url as default.


   --function Exists (Name : String) return Boolean renames AD.Exists;
   function Exists (Name : String) return Boolean;
   --  Returns True if file or directory Name exists.

   function Extract_Directory (Name : String) return String;
   --  Returns directory from Name.
   
   function Extract_Filename (Name : String) return String;
   --  Returns filename from Name.

   function File_Size (Name : String) return Integer;
   --  Size of file

   --function Get_Directory return String renames AD.Current_Directory;
   function Get_Directory return String;
   --  Returns the full directory name for the current default directory.
   
   function If_Root_Directory (Dir_Tree : String) return Boolean;
   --  This function checks the following root directories: bin, boot, dev, 
   --  etc, home, lib, lib32, lib64, libx32, lost+found, media, mnt, opt, 
   --  proc, root, run, sbin, srv, sys, tmp, usr, var. Returns True if 
   --  Dir_Tree is a root directory. Dir_Tree must be fully qualified, ie 
   --  starting with a slash (/).

   procedure Move_File (Source_Name, Target_Name : String);
   --  Move a Source_Name file to a Target_Name file destination. Copy_Form is
   --  “preserve=all_attributes,mode=overwrite” (full attributes preservation
   --  and overwrite file if exists).

   procedure Rename (Old_Name, New_Name : String);
   --  Rename an Old_Name file or directory to a New_Name file or directory.
   --  If exists a file New_File, it will be overwritten. 
   
   function Search_Lines (File_Name, Pattern : String) return Boolean;
   --  Search at least a line matching Pattern in File_Name and return true if
   --  found.

   function Set_Directory (Directory : String) return Boolean;
   --  Change to a directory Directory. Create Directory if this latter does
   --  not exist. Return False if operation is unsuccessfull.

-------------------------------------------------------------------------------
private

   --  Form parameter:
   --      The allowed values for preserve= are:
   --        no_attributes:  Do not try to preserve any file attributes. This
   --                        is the default if no preserve= is found in Form.
   --        all_attributes: Try to preserve all file attributes (timestamps,
   --                        access rights).
   --        timestamps:     Preserve the timestamp of the copied file, but not
   --                        the other file attributes.
   --
   --      The allowed values for mode= are:
   --        copy:           Only copy if the destination file does not already
   --                        exist. If it already exists, Copy_File will fail.
   --        overwrite:      Copy the file in all cases. Overwrite an already
   --                        existing destination file. This is the default if
   --                        no mode= is found in Form.
   --        append:         Append the original file to the destination file.
   --                        If the destination file does not exist, the
   --                        destination file is a copy of the source file.
   --                        When mode=append, the field preserve=, if it
   --                        exists, is not taken into account.

   Copy_Form : constant String := "preserve=all_attributes,mode=overwrite";
   --  Copy_Form : constant String := "preserve=no_attributes,mode=overwrite";
   
   procedure Log_Err (Err_Exc : Ada.Exceptions.Exception_Occurrence);
   
-------------------------------------------------------------------------------
end v22.Fls;
-------------------------------------------------------------------------------
