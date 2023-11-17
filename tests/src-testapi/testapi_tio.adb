-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_tio.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - API test program
--
--  @description
--  Build application and documentation
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

package body TestApi_Tio is

   type Animation is array (1 .. 7) of Character;
   Progress : constant Animation := ('/', '-', '\', '|', '/', '-', '|');

   procedure Run is
   begin

      -------------------------------------------------------------------------
      Msg.Set_Task ("TIO T1");
      Msg.Title ("Console demo - nothing will be logged");
      Msg.New_Line;

      Tio.Put_Line ("Put a String at current cursor position");
      Tio.Put_Line ("Put a Char, a String then put a new line");
      Tio.Put ("C"); Tio.Put (" String"); Tio.New_Line;
      Tio.Put_Line ("Move cursor forward, backward, then put a new line");
      Tio.Put_Line ("     0    1         2         ");
      Tio.Put_Line ("012345678901234567890123456789");
      Tio.Cursor_Line_Forward (10); Tio.Put ("|");
      Tio.Cursor_Line_Forward (9); Tio.Put ("|");
      Tio.Cursor_Line_Backward (16); Tio.Put ("|");
      Tio.New_Line;
      Tio.Put_Line ("(-16)3<  >1(+10)   >2(+9)");
      Tio.Put_Line ("Save cursor pos, print first line, restore pos, print");
      Tio.Put_Line ("current line, then put a new line. Using 'clear'");
      Tio.Put_Line ("command before running this test is recommended to");
      Tio.Put_Line ("reset the console coordinates.");
      Tio.Cursor_Save;
      Tio.Cursor_Move (0, 0);
      Tio.Put ("Command 1 : Cursor at the first line, after saving cursor");
      Tio.Put ("coordinates and move cursor...");
      Tio.Cursor_Restore;
      Tio.Put_Line ("Command 2 : Cursor at the current line, after restoring");
      Tio.Put_Line ("cursor coordinates...");
      Tio.New_Line;

      -------------------------------------------------------------------------
      Msg.Set_Task ("TIO T2");
      Msg.Title ("Animation for batch processing, style 1");
      Tio.New_Line;

      for I in 1 .. 10 loop
         Tio.Put (".*");
         Tio.Cursor_Line_Backward (1);
         delay 0.1;
      end loop;
      for I in 1 .. 11 loop
         Tio.Put ("-");
         Tio.Cursor_Line_Backward (2);
         delay 0.1;
      end loop;

      Tio.New_Line;
      Tio.New_Line;

      -------------------------------------------------------------------------
      Msg.Set_Task ("TIO T3");
      Msg.Title ("Animation for batch processing, style 2");
      Tio.New_Line;

      for I in 1 .. 3 loop
         Tio.Put (I * 2 * ".");
         for J in 1 .. 7 loop
            Tio.Put (Progress (J));
            Tio.Cursor_Line_Backward (1);
            delay 0.2;
         end loop;
         Tio.Cursor_Line_Backward (3);
         delay 0.2;
      end loop;

      Tio.New_Line;
      Tio.New_Line;

      -------------------------------------------------------------------------
      Msg.Set_Task ("TIO T4");
      Msg.Title ("Animation for batch processing, style 3");
      Msg.New_Line;

      Tio.Animated_Delay (8);

      Tio.New_Line;
      Tio.Put_Line ("Finally send a beep");
      Tio.Beep;

   end Run;

-------------------------------------------------------------------------------
end TestApi_Tio;
-------------------------------------------------------------------------------
