-------------------------------------------------------------------------------
-- 
--  _|      _|    _|_|      _|_|    
--  _|      _|  _|    _|  _|    _| 
--  _|      _|      _|        _|    
--    _|  _|      _|        _|      
--      _|      _|_|_|_|  _|_|_|_|  
--
--  @file      v22-log.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - Log package
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Characters.Handling;

with v22.Fls;

package body v22.Msg is

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------
   
   ----------------------------------------------------------------------------
   procedure Debug (Message : String) is
   begin
      if Debug_On = On then
         Put (Message, "DBG");
      end if;
   end Debug;
   
   ----------------------------------------------------------------------------
   procedure Error (Message : String) is
   begin
      Put (Message, "ERR");
   end Error;

   ----------------------------------------------------------------------------
   function Get_Debug return On_Off is
   begin
      return (Debug_On);
   end Get_Debug;
   
   ----------------------------------------------------------------------------
   procedure New_Line is
   begin
      Tio.New_Line;
      if Disk_On then
         Tio.New_Line (Handle);
      end if;
   end New_Line;

   ----------------------------------------------------------------------------
   procedure Info (Message : Boolean) is
   begin
      Put (From_Latin_1 ((if (Message) then "True" else "False")), "MSG");
   end Info;
   
   procedure Info (Message : On_Off) is
   begin
      Put (From_Latin_1 (On_Off'Image (Message)), "MSG");
   end Info;
   
   procedure Info (Message : ASCII_Character) is
   begin
      Put (From_ASCII (Message), "MSG");
   end Info;
   
   procedure Info (Message : String) is
   begin
      Put (Trim_Left (Message), "MSG"); -- Suppress the space left for positive sign
   end Info;
   
   procedure Info (Message : Integer) is
   begin
      Put (To_String (Message), "MSG");
   end Info;
   
   procedure Info (Message : Long_Integer) is
   begin
      Put (To_String (Message), "MSG");
   end Info;
   
   procedure Info (Message : Long_Long_Integer) is
   begin
      Put (To_String (Message), "MSG");
   end Info;
   
   procedure Info (Message : Money) is
   begin
      Put (From_Latin_1 (Money'Image (Message)), "MSG");
   end Info;

   ----------------------------------------------------------------------------
   procedure Set_Debug (Switch : On_Off)  is
   begin
      Debug_On := Switch;
   end Set_Debug;
   
   ----------------------------------------------------------------------------
   procedure Set_Disk (Switch : On_Off) is
      Log_File_Name : constant String := Log_Dir_Store & Prg.Name & ".log";
      Log_Dir_Name : constant String := Fls.Extract_Directory (Log_File_Name);
   begin
      Disk_On := (Switch = On);
      if Disk_On then
         if Fls.Exists (Log_File_Name) then
            Tio.Append (Handle, Log_File_Name);
         else
            -- Ensure that the complete tree structure exists before creating file
            if Fls.Create_Directory_Tree (Log_Dir_Name) then
               Tio.Create (Handle, Log_File_Name);
            else
               Disk_On := False;
               Error ("Log.Set_Disk > Can't create directory: " & Log_File_Name);
            end if;
         end if;
         if not Tio.Is_Open (Handle) then
            Disk_On := False;
            Error ("Log.Set_Disk > can't log on disk to: " & Log_File_Name);
         end if;
      else
         if Tio.Is_Open (Handle) then
            Tio.Close (Handle);
         end if;
      end if;
   end Set_Disk;
   
   ----------------------------------------------------------------------------
   procedure Set_Display (Switch : On_Off)  is
   begin
      Display_On := (Switch = On);
   end Set_Display;
   
   ----------------------------------------------------------------------------
   procedure Set_Header (Switch : On_Off) is
   begin
      Header_On := (Switch = On);
   end Set_Header;

   ----------------------------------------------------------------------------
   procedure Set_Dir (Dir_In : String) is
   begin
      Log_Dir_Store := Dir_In;
   end Set_Dir;

   function Get_Dir return String is
   begin
      return Log_Dir_Store;
   end Get_Dir;
   
   ----------------------------------------------------------------------------
   procedure Set_Task (New_Task : String) is
   begin
      Task_State := New_Task;
   end Set_Task;
   
   ----------------------------------------------------------------------------
   procedure Title (Message : String) is
   begin
      Put (To_Upper (Message), "MSG", True);
   end Title;
   
   ----------------------------------------------------------------------------
   --  Private
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   procedure Put (Line_In : String;
                  Line_Level : String;
                  Title_On : Boolean := False) is
      Line : String := Line_In;
      Line_Disk : String := Line;
      Line_Task : String := "";
      Ansi_Begin : String := "";
      Ansi_End : String := "";
   begin
      --  Mutex for exclusive use as Msg.* must be reentrant   
      M.Lock;

      if Display_On and Tio.Ansi then
         if Line_Level = "DBG" then
            Ansi_Begin := CONSOLE_COLOR_YELLOW;
         elsif Line_Level = "ERR" then
            Ansi_Begin := CONSOLE_COLOR_RED;
         end if;
         Ansi_End := CONSOLE_COLOR_RESET;
      end if;

      --  Task length control
      if Length (Task_State) > Task_Max_Length then
         --                  these two numbers must be equals v     v
         Line_Task := Slice (Task_State, 1, Task_Max_Length - 1) & (1 * "*");
      elsif  Length (Task_State) < Task_Max_Length then
         Line_Task :=  Task_State &
                      (Task_Max_Length - Length (Task_State)) * " ";
      else
         Line_Task :=  Task_State;
      end if;

      --  Header console and disk mode
      if Header_On then

         --  Line Length control
         if (Header_Length + Length (Line) + 1) > Line_Max_Length then
            --                                      these two numbers v
            Line := Slice (Line, 1, Line_Max_Length - Header_Length - 1) & (1 * "*");
            --                                               must be equals ^                                                        
         end if;

         if Title_On then
            if (Header_Length + Length (Line) + 1) < Title_Max_Length then
               Line := Line &
                      (Title_Max_Length - Header_Length - Length (Line)) * "-";
            end if;
         end if;
         
         if Display_On then
            --  Console write with limited length line and ansi fancy
            Tio.Put_Line (Prg.Time_Stamp & " - " &
                            Line_Task & " - " &
                            Ansi_Begin & Line_Level & Ansi_End & " - " &
                            Line);
         end if;
         
      --  Free console mode
      else
         if Display_On then
            Tio.Put_Line (Ansi_Begin & Line & Ansi_End);
         end if;
      end if;
      
      --  Disk write with unlimited length line
      if Disk_On then
         if Title_On then
            if (Header_Length + Length (Line_Disk) + 1) < Title_Max_Length then
               Line_Disk := Line_Disk &
                (Title_Max_Length - Header_Length - Length (Line_Disk)) * "-";
            end if;
         end if;
         Tio.Put_Line (Handle, Prg.Time_Stamp & " - " &
                               Line_Task & " - " &
                               Line_Level & " - " &
                               Line_Disk);
      end if;

      --  Mutex for exclusive use as Msg.* must be reentrant   
      M.Release;
   end Put;
   
------------------------------------------------------------------------------
end v22.Msg;
------------------------------------------------------------------------------
