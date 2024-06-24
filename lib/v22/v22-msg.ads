-------------------------------------------------------------------------------
-- 
--  _|      _|    _|_|      _|_|    
--  _|      _|  _|    _|  _|    _| 
--  _|      _|      _|        _|    
--    _|  _|      _|        _|      
--      _|      _|_|_|_|  _|_|_|_|  
--
--  @file      v22-log.ads
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

with v22.Tio;
with v22.Prg;
with v22.Uxs; use v22.Uxs;

package v22.Msg is

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   procedure Debug (Message : Boolean);
   procedure Debug (Message : On_Off);
   procedure Debug (Message : String);
   procedure Debug_Latin_1 (Message : Standard.String);
   --  Log a debug message.

   procedure Error (Message : String);
   procedure Error_Latin_1 (Message : Standard.String);
   --  Log a error message.
      
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
   procedure Info (Message : Float);
   procedure Info (Message : Money);
   procedure Info_Latin_1 (Message : Standard.String);
   --  Log an information message.
   
   function Is_Debug return On_Off;
   --  Return debug status.
   
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

   --  0         1         2         3      3
   --  01234567890123456789012345678901234567
   --  20210327-160010.NNN - STEP 3  - MSG -
   --  \-------19-------/  3 \--7--/ 3 \3/ 3 = Header_Length
   --      Timestamp           Task   Class
   
   --  Line_Max_Length declared in v22.ads

   Task_Max_Length : constant Natural := 7;
   Header_Length : constant Natural := 37;

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
