-------------------------------------------------------------------------------
-- 
--  _|      _|    _|_|      _|_|    
--  _|      _|  _|    _|  _|    _| 
--  _|      _|      _|        _|    
--    _|  _|      _|        _|      
--      _|      _|_|_|_|  _|_|_|_|  
--
--  @file      v22-log.adb
--  @copyright See authors list below and README.md file
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
   procedure Debug (Message : Boolean) is
   begin
      Msg_Mutex.Lock;
      if Debug_On = On then
         Put (From_Latin_1 ((if (Message) then "True" else "False")), "DBG");
      end if;
      Msg_Mutex.Unlock;
   end Debug;
   
   procedure Debug (Message : On_Off) is
   begin
      Msg_Mutex.Lock;
      if Debug_On = On then
         Put (From_Latin_1 (On_Off'Image (Message)), "MSG");
      end if;
      Msg_Mutex.Unlock;
   end Debug;
   
   procedure Debug (Message : String) is
   begin
      Msg_Mutex.Lock;
      if Debug_On = On then
         Put (Message, "DBG");
      end if;
      Msg_Mutex.Unlock;
   end Debug;
   
   procedure Debug_Latin_1 (Message : Standard.String) is
   begin
      Msg_Mutex.Lock;   
      if Debug_On = On then
         Put (From_Latin_1 (Message), "DBG");
      end if;
      Msg_Mutex.Unlock;
   end Debug_Latin_1;
   
   ----------------------------------------------------------------------------
   procedure Error (Message : String) is
   begin
      Msg_Mutex.Lock;   
      Put (Message, "ERR");
      Msg_Mutex.Unlock;
   end Error;
   
   procedure Error_Latin_1 (Message : Standard.String) is
   begin
      Msg_Mutex.Lock;   
      Put (From_Latin_1 (Message), "ERR");
      Msg_Mutex.Unlock;
   end Error_Latin_1;

   ----------------------------------------------------------------------------
   procedure Info (Message : Boolean) is
   begin
      Msg_Mutex.Lock;
      Put (From_Latin_1 ((if (Message) then "True" else "False")), "MSG");
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : On_Off) is
   begin
      Msg_Mutex.Lock;
      Put (From_Latin_1 (On_Off'Image (Message)), "MSG");
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : ASCII_Character) is
   begin
      Msg_Mutex.Lock;
      Put (From_ASCII (Message), "MSG");
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : String) is
   begin
      Msg_Mutex.Lock;
      Put (Trim_Left (Message), "MSG"); -- Suppress the space left for positive sign
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : Integer) is
   begin
      Msg_Mutex.Lock;
      Put (To_String (Message), "MSG");
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : Long_Integer) is
   begin
      Msg_Mutex.Lock;
      Put (To_String (Message), "MSG");
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : Long_Long_Integer) is
   begin
      Msg_Mutex.Lock;
      Put (To_String (Message), "MSG");
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : Float) is
   begin
      Msg_Mutex.Lock;
      Put (To_String (Message), "MSG");
      Msg_Mutex.Unlock;
   end Info;
   
   procedure Info (Message : Money) is
   begin
      Msg_Mutex.Lock;
      Put (From_Latin_1 (Money'Image (Message)), "MSG");
      Msg_Mutex.Unlock;
   end Info;
         
   procedure Info_Latin_1 (Message : Standard.String) is
   begin
      Msg_Mutex.Lock;
      Put (From_Latin_1 (Message), "MSG"); 
      Msg_Mutex.Unlock;
   end Info_Latin_1;
   
   ----------------------------------------------------------------------------
   function Is_Debug return On_Off is
   begin
      return (Debug_On);
   end Is_Debug;

   ----------------------------------------------------------------------------
   procedure New_Line is
   begin
      Msg_Mutex.Lock;
      Tio.New_Line;
      if Disk_On then
         Tio.New_Line (Handle);
      end if;
      Msg_Mutex.Unlock;
   end New_Line;
   
   ----------------------------------------------------------------------------
   procedure Set_Debug (Switch : On_Off)  is
   begin
      Msg_Mutex.Lock;
      Debug_On := Switch;
      Msg_Mutex.Unlock;
   end Set_Debug;
   
   ----------------------------------------------------------------------------
   procedure Set_Disk (Switch : On_Off) is
      Log_File_Name : constant String := Log_Dir_Store & Prg.Name & ".log";
      Log_Dir_Name : constant String := Fls.Extract_Directory (Log_File_Name);
   begin
      Msg_Mutex.Lock;
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
      Msg_Mutex.Unlock;
   end Set_Disk;
   
   ----------------------------------------------------------------------------
   procedure Set_Display (Switch : On_Off)  is
   begin
      Msg_Mutex.Lock;
      Display_On := (Switch = On);
      Msg_Mutex.Unlock;
   end Set_Display;
   
   ----------------------------------------------------------------------------
   procedure Set_Header (Switch : On_Off) is
   begin
      Msg_Mutex.Lock;
      Header_On := (Switch = On);
      Msg_Mutex.Unlock;
   end Set_Header;

   ----------------------------------------------------------------------------
   procedure Set_Dir (Dir_In : String) is
   begin
      Msg_Mutex.Lock;
      Log_Dir_Store := Dir_In;
      Msg_Mutex.Unlock;
   end Set_Dir;

   function Get_Dir return String is
   begin
      return Log_Dir_Store;
   end Get_Dir;
   
   ----------------------------------------------------------------------------
   procedure Set_Task (New_Task : String) is
   begin
      Msg_Mutex.Lock;
      Task_State := New_Task;
      Msg_Mutex.Unlock;
   end Set_Task;
   
   ----------------------------------------------------------------------------
   procedure Title (Message : String) is
   begin
      Msg_Mutex.Lock;
      Put (To_Upper (Message), "MSG", True);
      Msg_Mutex.Unlock;
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
   
      if Display_On and Tio.Get_Ansi then
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
            Tio.Put_Line (Prg.Date_Time_Milli_Stamp & " - " &
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
         Tio.Put_Line (Handle, Prg.Date_Time_Milli_Stamp & " - " &
                               Line_Task & " - " &
                               Line_Level & " - " &
                               Line_Disk);
         Tio.Flush (Handle); -- Flush to disk for an allways up to date log file
      end if;

   end Put;
   
------------------------------------------------------------------------------
end v22.Msg;
------------------------------------------------------------------------------
