-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-tio.adb
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Text I/O package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Integer_Text_IO;

with UXStrings.Text_IO;

with v22.Fls;
with v22.Msg;
with v22.Prg;
with v22.Sys;
with v22.Tio;

package body v22.Tio is

   package UTI renames UXStrings.Text_IO;

   ----------------------------------------------------------------------------
   --  API - Terminal
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Animated_Delay (Delay_Seconds : Positive) is
      type Animation is array (1 .. 7) of Character;
      Progress : constant Animation := ('/', '-', '\', '|', '/', '-', '|');
   begin
      for I in 1 .. Delay_Seconds loop
         if I mod 10 = 0 then
            Tio.Put ("|");
         elsif I mod 5 = 0 then
            Tio.Put ("!");
         else
            Tio.Put (".");
         end if;
         for J in 1 .. 7 loop
            Put (Progress (J));
            Cursor_Line_Backward (1);
            delay 0.143; -- 0.143 x 7 = 1001 ms ~ 1s
         end loop;
      end loop;
      New_Line;
   end Animated_Delay;

   ----------------------------------------------------------------------------
   function Confirm_Twice (User_Prompt_1 : String ; User_Prompt_2 : String) return Boolean is
      Answer : Character;
      Result : Boolean := False;
   begin
      Put_Line (User_Prompt_1 & " 'y/n' ?");
      Tio.Get_Immediate (Answer);
      if (Answer = 'y' or Answer = 'Y') then
         Put_Line (User_Prompt_2 & " by pressing 'c' ?");
         Tio.Get_Immediate (Answer);
         Result := (Answer = 'c' or Answer = 'C');
      end if;
      New_Line;
      return Result;
   end Confirm_Twice;

   ----------------------------------------------------------------------------
   procedure Beep is
   begin
      ATI.Put (Item => ASCII.BEL);
      ATI.Flush;
   end Beep;

   ----------------------------------------------------------------------------
   procedure Bell is
      Sound_File : String := "/usr/share/sounds/freedesktop/stereo/complete.oga";
      Sound_Player : String := "/usr/bin/paplay";
   begin
      --  Run only with user rights to avoid pa_context_connect() failed: Connection refused
      if Prg.Is_User_Not_Root then
         if Fls.Exists (Sound_Player) and Fls.Exists (Sound_File) then
            --  Pulseaudio paplay & freedesktop sounds come as standard on Ubuntu 18.04 LTS and 22.04 LTS
            Sys.Shell_Execute (Sound_Player & " " & Sound_File);
         else
            Beep;
         end if;
      else
         Beep;
      end if;
   end Bell;

   ----------------------------------------------------------------------------
   procedure Clear_Screen is
   begin
      if Tio.Get_Ansi then
         ATI.Put (ASCII.ESC & "[2J");
      end if;
      ATI.Flush;
   end Clear_Screen;

   ----------------------------------------------------------------------------
   procedure Cursor_Move (X : Row; Y : Column) is
   begin
      ATI.Flush;
      if Tio.Get_Ansi then
         ATI.Put (ASCII.ESC & "[");
         Ada.Integer_Text_IO.Put (Item => X, Width => 1);
         ATI.Put (Item => ';');
         Ada.Integer_Text_IO.Put (Item => Y, Width => 1);
         ATI.Put (Item => 'f');
      end if;
   end Cursor_Move;

   ----------------------------------------------------------------------------
   procedure Cursor_Line_Forward (X : Row) is
   begin
      ATI.Put (Item => ASCII.ESC);
      ATI.Put ("[");
      Ada.Integer_Text_IO.Put (Item => X, Width => 1);
      ATI.Put (Item => 'C');
   end Cursor_Line_Forward;

   ----------------------------------------------------------------------------
   procedure Cursor_Line_Backward (X : Row) is
   begin
      if Tio.Get_Ansi then
         ATI.Put (ASCII.ESC & "[");
         Ada.Integer_Text_IO.Put (Item => X, Width => 1);
      ATI.Put (Item => 'D');
      end if;
   end Cursor_Line_Backward;

   ----------------------------------------------------------------------------
   procedure Cursor_Line_Erase is
   begin
      if Tio.Get_Ansi then
         ATI.Put (ASCII.ESC & "[K");
      end if;
   end Cursor_Line_Erase;

   ----------------------------------------------------------------------------
   procedure Cursor_Save is
   begin
      if Tio.Get_Ansi then
         ATI.Put (ASCII.ESC & "[s");
      end if;
   end Cursor_Save;

   ----------------------------------------------------------------------------
   procedure Cursor_Restore is
   begin
      if Tio.Get_Ansi then
         ATI.Put (ASCII.ESC & "[u");
      end if;
   end Cursor_Restore;

   ----------------------------------------------------------------------------
   procedure Set_Ansi (Switch : On_Off)  is
   begin
      Ansi_State := (Switch = On);
   end Set_Ansi;

   ----------------------------------------------------------------------------
   procedure Set_Cursor (Switch : On_Off) is
   begin
      if Tio.Get_Ansi then
         if Switch = On then
            ATI.Put (ASCII.ESC & "[?25h");
         else
            ATI.Put (ASCII.ESC & "[?25l");
         end if;
      end if;
   end Set_Cursor;

   ----------------------------------------------------------------------------
   function Get_Ansi return Boolean is
   begin
      return Ansi_State;
   end Get_Ansi;

   ----------------------------------------------------------------------------
   function Get_Line return String is
      Result : String := "";
   begin
      return From_Latin_1 (ATI.Get_Line);
   end Get_Line;

   ----------------------------------------------------------------------------
   function Get_Password return String is
      Exit_Char : constant Character := Character'Val(10); -- LF (return code key)
      Current_Char : ASCII_Character;
      Result : String := "";
   begin
      Tio.Put ("Password:");
      while True loop
         Get_Immediate (Current_Char);
         --Put (To_Val(To_String(Current_Char)));
         if Current_Char = Exit_Char then
            exit;
         else
            Result := Result & From_ASCII (Current_Char);
         end if;
      end loop;
      Tio.New_Line;
      return Result;
   end Get_Password;

   ----------------------------------------------------------------------------
   procedure Pause is
      Dummy : Character;
   begin
      Put ("Press any key to continue or [Ctrl-C] to abort...");
      Get_Immediate (Dummy);
   end Pause;

   ----------------------------------------------------------------------------
   procedure Put (B : Boolean) is
   begin
      Put (From_Latin_1 ((if (B) then "True" else "False")));
   end Put;

   procedure Put (B : On_Off) is
   begin
      ATI.Put (On_Off'Image (B));
   end Put;

   procedure Put (V : String) is
   begin
      --  ATI.Put (To_Latin_1 (Trim_Left (V))); -- Suppress the space left for positive sign
      UTI.Put (V);
   end Put;

   procedure Put (I : Integer) is
   begin
      Put (To_String (I));
   end Put;

   procedure Put (I : Long_Integer) is
   begin
      Put (To_String (Integer (I)));
   end Put;

   procedure Put (I : Integer_64) is
   begin
      Put (To_String (Integer (I)));
   end Put;

   procedure Put (M : Money) is
   begin
      Put (From_Latin_1 (Money'Image (M)));
   end Put;

   ----------------------------------------------------------------------------
   procedure Put_Line (B : Boolean) is
   begin
      Put (From_Latin_1 ((if (B) then "True" else "False")));
      New_Line;
   end Put_Line;

   procedure Put_Line (B : On_Off) is
   begin
      ATI.Put_Line (On_Off'Image (B));
   end Put_Line;

   procedure Put_Line (V : String) is
   begin
      UTI.Put_Line (V);
   end Put_Line;

   procedure Put_Line (I : Integer) is
   begin
      Put (To_String (I));
      New_Line;
   end Put_Line;

   procedure Put_Line (I : Long_Integer) is
   begin
      Put (To_String (Integer (I)));
      New_Line;
   end Put_Line;

   procedure Put_Line (I : Integer_64) is
   begin
      Put (To_String (Integer (I)));
      New_Line;
   end Put_Line;

   procedure Put_Line (C : Character) is
   begin
      Put (C);
      New_Line;
   end Put_Line;

   procedure Put_Line (M : Money) is
   begin
      Put (From_Latin_1 (Money'Image (M)));
   end Put_Line;

   ----------------------------------------------------------------------------
   --  API - Text File
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Append (Handle : in out File; Name : String) is
   begin
      ATI.Open (Handle, ATI.Append_File, To_Latin_1 (Name));
   end Append;

   ----------------------------------------------------------------------------
   procedure Create (Handle : in out File; Name : String) is
   begin
      ATI.Create (Handle, ATI.Out_File, To_Latin_1 (Name));
   end Create;

   ----------------------------------------------------------------------------
   function Get_Line (Handle : File) return String is
   begin
      return From_Latin_1 (ATI.Get_Line (Handle));
   end Get_Line;

   procedure Get_Line (Handle : File; V : out String) is
   begin
      V := From_Latin_1 (ATI.Get_Line (Handle));
   end Get_Line;

   ----------------------------------------------------------------------------
   procedure Open_Conf (Handle : in out File; Name : String ; Wipe_Before_Process : Boolean := False ;
                        Permissions : String := "") is
      Dir_File : constant String := Fls.Extract_Directory (Name);
      SE_Result : Integer := 0;
   begin
      --  Preventive house keeping
      if Fls.Exists (Name) then
         Fls.Backup_File (Name);
         if Wipe_Before_Process then
            Fls.Delete_File (Name);
         end if;
      end if;

      if Fls.Exists (Name) then
         Append (Handle, Name);
      else
         -- Ensure that the complete tree structure exists before creating file
         if Fls.Create_Directory_Tree (Dir_File) then
            Create (Handle, Name);
         else
            Msg.Error ("v22.Tio.Open_Conf > Can't create directory: " & Dir_File);
         end if;
      end if;

      -- Apply optional permissions
      if Fls.Exists (Name) and not Is_Empty (Permissions) then
         Sys.Shell_Execute ("chmod 0600 " & Name, SE_Result);
         if SE_Result /= 0 then
            Msg.Error  ("v22.Tio.Open_Conf > Can't apply permissions to: " & Name);
          end if;
      end if;

   end Open_Conf;

   ----------------------------------------------------------------------------
   procedure Open_Read (Handle : in out File; Name : String) is
   begin
      ATI.Open (Handle, ATI.In_File, To_Latin_1 (Name));
   end Open_Read;

   ----------------------------------------------------------------------------
   procedure Put (Handle : File; V : String) is
   begin
      ATI.Put (Handle, To_Latin_1 (V));
   end Put;

   ----------------------------------------------------------------------------
   procedure Put_Line (Handle : File; C : Character) is
   begin
      Put (Handle, C);
      New_Line (Handle);
   end Put_Line;

   procedure Put_Line (Handle : File; V : String) is
   begin
      ATI.Put_Line (Handle, To_Latin_1 (V));
   end Put_Line;

   ----------------------------------------------------------------------------
   function Read_File (File_Name : String) return String is
      File_Handle : File;
      Line_Buffer, Result_Buffer : String := "";
   begin
      if Fls.Exists (File_Name) then
         Tio.Open_Read (File_Handle, File_Name);
         while not (End_Of_File (File_Handle)) loop
            Get_Line (File_Handle, Line_Buffer);
            Result_Buffer := Result_Buffer & Line_Buffer & LF;
         end loop;
         Close (File_Handle);
      else
         Msg.Error ("v22.Tio.Read_File > File does not exist: " & File_Name);
      end if;
      return Result_Buffer;
   end Read_File;

   ----------------------------------------------------------------------------
   procedure Write_File (File_Name : String ; Content : String ; Permissions : String := "") is
      Index_Next : Integer := 1;
      String_To_Process : String := "";
      Dir_File : constant String := Fls.Extract_Directory (File_Name);
      File_Handle : File;
      SE_Result : Integer := 0;
   begin

      -- Preventive housekeeping
      Fls.Delete_File (File_Name);

      -- Ensure that the full tree structure exists before creating file
      if Fls.Create_Directory_Tree (Dir_File) then
         Create (File_Handle, File_Name);

         -- Apply optional permissions
         if Fls.Exists (File_Name) and not Is_Empty (Permissions) then
            Sys.Shell_Execute ("chmod 0600 " & File_Name, SE_Result);
            if SE_Result /= 0 then
               Msg.Error  ("v22.Tio.Open_Conf > Can't apply permissions to: " & File_Name);
            end if;
         end if;

         -- Adding trailing LF if missing
         if Slice (Content, Length (Content),1) = LF then
            String_To_Process := Content;
         else
            String_To_Process := Content & LF;
         end if;
         --String_To_Process := Content & (if Slice (Content, Length (Content),1) = LF then "" else LF);

         while True loop
            Index_Next := Index (String_To_Process, LF);
            Put_Line (File_Handle, Slice (String_To_Process, 1, Index_Next - 1));
            String_To_Process := Slice (String_To_Process, Index_Next + 1, Length (String_To_Process));
            if Length (String_To_Process) <= 1 then -- Last field reached
               exit;
            end if;
         end loop;

         Close (File_Handle);

      else
         Msg.Error ("v22.Tio.Write_File > Can't create directory: " & Dir_File);
      end if;

   end Write_File;

------------------------------------------------------------------------------
end v22.Tio;
------------------------------------------------------------------------------
