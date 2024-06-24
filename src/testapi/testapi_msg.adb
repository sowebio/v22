-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_msg.ads
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

package body TestApi_Msg is

   procedure Run is
      Set_Debug_State : On_Off := Msg.Is_Debug;
   begin

      -------------------------------------------------------------------------
      Msg.Set_Task ("LOG T1");
      Msg.Title ("Log demo");
      Msg.New_Line;

      Msg.Info ("This is an information message");
      Msg.Debug ("This first debug message should not appears");
      Msg.Set_Debug (On);
      Msg.Debug ("This is a debug message");
      Msg.Set_Debug (Off);
      Msg.Debug ("This last debug message should not appears");
       Msg.Error ("This is an error message");
      Msg.Set_Disk (Off);
      Msg.Info ("This message should not be file logged (but displayed)");
      Msg.Set_Disk (On);
      Msg.Info ("This message should be truncated because it is really" &
                 "too long !");
      Msg.Set_Task ("TASKTRUNCATED");
      Msg.Title ("Task above and this title should be truncated it is" &
                   "really too long !");
      Msg.New_Line;

      Msg.Set_Debug (Set_Debug_State);

   end Run;

-------------------------------------------------------------------------------
end TestApi_Msg;
-------------------------------------------------------------------------------
