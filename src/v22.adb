-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22.ads
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework
--
--  @description
--
--  @authors
--  Théodore Gigault - tg - developpement@soweb.io
--  Arthur Le Floch - alf - developpement@soweb.io
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Directories;
with Ada.Strings.Fixed;
with GNAT.Source_Info;

with v22.Prg;
with v22.Sql;
with v22.Sys;
with v22.Tio;
with v22.Uxs;

package body v22 is

   use v22;

   ----------------------------------------------------------------------------
   procedure Raise_Exception is
      v22_Exception_Test : exception;
   begin
      raise v22_Exception_Test;
   end Raise_Exception;

   ----------------------------------------------------------------------------
   procedure Exception_Handling (Exception_Hook : Ada.Exceptions.Exception_Occurrence) is

      Exception_Handle : Tio.File;
      Exception_File_Name : constant String := Prg.Start_Dir & "/" & Prg.Name & ".err";
      Trace_Output : String := From_Latin_1 (Ada.Exceptions.Exception_Information (Exception_Hook));
      Trace_Lines : constant Natural := Uxs.Field_Count (Trace_Output, LF);
      Current_Line : String;
      Current_Address : String;
      Number_Position, First_Space_Position : Natural;
      Output_Processed : String := "";

      Trace_without_Lines : Boolean := False;

      SE_Result : Integer := 0;
      SE_Output : String := "";

   begin

      Tio.Line;
      Tio.Put_Line (Title_Max_Length * "-");
      Tio.Line;
      Tio.Put_Line ("Exception time         : " & Prg.Time_Stamp);
      Tio.Put_Line ("Program uptime         : " & Prg.Duration_Stamp (Prg.Start_Time));
      Tio.Put_Line ("Program build DT stamp : " & v22.Get_Build);
      Tio.Put_Line ("Program name & version : " & Prg.Get_Version);
      Tio.Put_Line ("Library name & version : " & v22.Get_Version);
      Tio.Put_Line ("Start directory        : " & Prg.Start_Dir);
      Tio.Put_Line ("Home directory         : " & Sys.Get_Home);
      Tio.Put_Line ("Ada mem. alloc. (bytes): " & Sys.Get_Alloc_Ada);
      Tio.Put_Line ("All mem. alloc. (bytes): " & Sys.Get_Alloc_All);

      --  See v22 Ada Framework User Manual > Architecture > Design > Exceptions
      if (Trace_Lines > 0) and Sys.Is_Command ("addr2line") then

         for I in 1 .. Trace_Lines loop
            Current_Line := Uxs.Field_By_Index (Trace_Output, I, LF);
            Number_Position := UXStrings.Index (Current_Line, "???");
            First_Space_Position := UXStrings.Index (Current_Line, SP);

            if (Number_Position > 2) and (First_Space_Position > 2) then
               Current_Address := Slice (Current_Line, 1, UXStrings.Index (Current_Line, SP) - 1);
               Sys.Shell_Execute ("addr2line -e " & Prg.Name & " " & Current_Address, SE_Result, SE_Output);
               if (UXStrings.Index (SE_Output, "/") > 0) then
                  Output_Processed := Output_Processed & Slice (Current_Line, 1, Number_Position - 1) & Uxs.Tail_After_Match (SE_Output, "/") & LF;
               else
                  Output_Processed := Output_Processed & Current_Line & LF;
               end if;
               Trace_without_Lines := True;
            else
               Output_Processed := Output_Processed & Current_Line & LF;
            end if;
         end loop;

         if Trace_without_Lines then
            Trace_Output := Output_Processed;
         end if;

         Tio.Line;
         Tio.Put_Line (Trace_Output);
         Tio.Put_Line (Title_Max_Length * "-");
         Tio.Line;

         if Ada.Directories.Exists (To_UTF_8 (Exception_File_Name)) then
            Tio.Append (Exception_Handle, Exception_File_Name);
         else
            Tio.Create (Exception_Handle, Exception_File_Name);
         end if;

         if Tio.Is_Open (Exception_Handle) then
            Tio.Put_Line (Exception_Handle, Title_Max_Length * "-");
            Tio.Line (Exception_Handle);
            Tio.Put_Line (Exception_Handle, "Exception time         : " & Prg.Time_Stamp);
            Tio.Put_Line (Exception_Handle, "Program uptime         : " & Prg.Duration_Stamp (Prg.Start_Time));
            Tio.Put_Line (Exception_Handle, "Program build DT stamp : " & v22.Get_Build);
            Tio.Put_Line (Exception_Handle, "Program name & version : " & Prg.Get_Version);
            Tio.Put_Line (Exception_Handle, "Library name & version : " & v22.Get_Version);
            Tio.Put_Line (Exception_Handle, "Start directory        : " & Prg.Start_Dir);
            Tio.Put_Line (Exception_Handle, "Home directory         : " & Sys.Get_Home);
            Tio.Put_Line (Exception_Handle, "Ada mem. alloc. (bytes): " & Sys.Get_Alloc_Ada);
            Tio.Put_Line (Exception_Handle, "All mem. alloc. (bytes): " & Sys.Get_Alloc_All);
            Tio.Line (Exception_Handle);
            Tio.Put_Line (Exception_Handle, Trace_Output);
            Tio.Put_Line (Exception_Handle, Title_Max_Length * "-");
            --Tio.Line (Exception_Handle);
            Tio.Close (Exception_Handle);
         end if;

      end if;

      Tio.Put_Line ("Finalizations...");
      Tio.Line;

      Sql.Close;
      Tio.Cursor_On;
      Prg.Set_Exit_Status (9);

      Tio.Line;
      Tio.Put_Line (Title_Max_Length * "-");
      Tio.Line;

   end Exception_Handling;

   ----------------------------------------------------------------------------
   function Get_Version return String is
   begin
      return (Name & " v" &
              From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Major), Ada.Strings.Left)) & "." &
              From_Latin_1 (Ada.Strings.Fixed.Trim (Natural'Image (Version_Minor), Ada.Strings.Left)));
   end Get_Version;

   ----------------------------------------------------------------------------
   function Get_Build return String is
   begin
      return ("build " &
              From_Latin_1 (GNAT.Source_Info.Compilation_ISO_Date) & " " &
              From_Latin_1 (GNAT.Source_Info.Compilation_Time));
   end Get_Build;

   ----------------------------------------------------------------------------
   function Get_Log_Dir return String is
   begin
     return Log_Dir;
   end Get_Log_Dir;

   ----------------------------------------------------------------------------
   function Get_Tmp_Dir return String is
   begin
     return Tmp_Dir;
   end Get_Tmp_Dir;

-------------------------------------------------------------------------------
end v22;
-------------------------------------------------------------------------------
