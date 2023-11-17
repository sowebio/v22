-------------------------------------------------------------------------------
-- 
--  _|      _|    _|_|      _|_|    
--  _|      _|  _|    _|  _|    _| 
--  _|      _|      _|        _|    
--    _|  _|      _|        _|      
--      _|      _|_|_|_|  _|_|_|_|  
--
--  @file      v22-log.ads
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

with v22.Tio;
with v22.Prg;
with v22.Uxs; use v22.Uxs;

package v22.Msg is

   procedure Debug (Message : String);
   --  Log a debug message.

   procedure Error (Message : String);
   --  Log a error message.
      
   function Get_Debug return On_Off;
   --  Return true if debug status is on.
   
   function Get_Dir return String;
   --  Returns log file directory.
   
   procedure New_Line;
   --  Log a blank line.
      
   procedure Info (Message : Boolean);
   procedure Info (Message : On_Off);
   procedure Info (Message : ASCII_Character);
   procedure Info (Message : String);
   procedure Info (Message : Integer);
   procedure Info (Message : Long_Integer);
   procedure Info (Message : Long_Long_Integer);
   procedure Info (Message : Money);
   --  Log an information message.

   procedure Set_Debug (Switch : On_Off);
   --  Set debug messages status on/[off].
   
   procedure Set_Display (Switch : On_Off);
   --  Log to display on/[off].

   procedure Set_Disk (Switch : On_Off);
   --  Log to disk on/[off].
      
   procedure Set_Header (Switch : On_Off);
   --  Line header on/[off].
   
   procedure Set_Dir (Dir_In : String);
   --  Set log file directory.
   
   procedure Set_Task (New_Task : String);
   --  Set new current log task.

   procedure Title (Message : String);
   --  Log a title.

-------------------------------------------------------------------------------
private

   Task_State : String := "INIT";

   --  0         1         2         3  3
   --  0123456789012345678901234567890123
   --  20210327 160010 - STEP 3  - MSG -
   --  \-----15-----/  3 \--7--/ 3 \3/ 3 = Header_Length
   --   Timestamp        Task    Class
   --  Line_Max_Length : constant Natural := 79; < Declared in v20.ads

   Task_Max_Length : constant Natural := 7;
   Header_Length : constant Natural := 34;

   Header_On : Boolean := False;
   --  Line header on/[off]

   Debug_On : On_Off := Off;
   --  Debug messages on/[off]
   
   Display_On : Boolean := False;
   --  Log to display on/[off]

   Disk_On : Boolean := False;
   --  Log to disk on/[off]

   Handle : Tio.File;

   Log_Dir_Store : String := Prg.Start_Dir & "/";

   procedure Put (Line_In : String; Line_Level : String; Title_On : Boolean := False);

-------------------------------------------------------------------------------
end v22.Msg;
-------------------------------------------------------------------------------
