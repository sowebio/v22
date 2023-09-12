-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-crl.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - curl interface
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io (low level sources amalgation and high level functions)
--
--  @versions
--  See git log
------------------------------------------------------------------------------

package body v22.Crl is

   package ICS renames Interfaces.C.Strings;

   --
   function Get_Version return String is
      Curl_Full_Version : Standard.String := ICS.Value (Curl_Version);
   begin
      return "cURL components versions: " & From_Latin_1 (Curl_Full_Version);
   end Get_Version;

-------------------------------------------------------------------------------
end v22.Crl;
-------------------------------------------------------------------------------
