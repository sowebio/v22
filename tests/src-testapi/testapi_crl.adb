-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testapi_crl.ads
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

package body TestApi_Crl is

   procedure Run is

      package ICS renames Interfaces.C.Strings;

      function Fopen (Name : ICS.chars_ptr; Mode : ICS.chars_ptr) return File_P;
      pragma Import(C, Fopen, "fopen");

      function Fclose (Stream : File_P) return int;
      pragma Import (C, Fclose, "fclose");

      Curl : CURL_P;
      Res  : Curlcode;

      Host_Name        : ICS.chars_ptr := ICS.New_String ("https://www.soweb.io");
      Header_File_Name : ICS.chars_ptr := ICS.New_String ("www-soweb-io-header.txt");
      Body_File_Name   : ICS.chars_ptr := ICS.New_String ("www-soweb-io-homepage.html");
      Header_Handle    : File_P;
      Body_Handle      : File_P;
      Rc               : int;

   begin

      -------------------------------------------------------------------------
      Msg.Set_Task ("CRL T1");
      Msg.Title ("cURL demo");
      Msg.New_Line;

      Curl := Crl.Curl_Easy_Init;

      if Curl /= null then

         Res := Curl_Easy_Setopt (Curl, Crl.CURLOPT_URL, Host_Name);
         Res := Curl_Easy_Setopt (Curl, Crl.CURLOPT_NOPROGRESS, 1);
         Res := Curl_Easy_Setopt (Curl, Crl.CURLOPT_WRITEFUNCTION, Write_Data'Access);

         Header_Handle := Fopen (Header_File_Name, ICS.New_String ("w"));
         if not (Header_Handle = System.Null_Address) then

            Body_Handle := Fopen (Body_File_Name, ICS.New_String ("w"));
            if not (Body_Handle = System.Null_Address) then

               Res := Curl_Easy_Setopt (Curl, Crl.CURLOPT_WRITEHEADER, Header_Handle);
               Res := Curl_Easy_Setopt (Curl, Crl.CURLOPT_WRITEDATA, Body_Handle);
               Res := Curl_Easy_Perform (Curl);

               Msg.Info (Crl.Get_Version);
               Msg.Info ("cURL : www-soweb-io-header.txt and www-soweb-io-homepage.html files processed successfully");

               --  www-soweb-io-header.txt
               --
               --  HTTP/2 200
               --  server: nginx/1.21.3
               --  date: Mon, 21 Aug 2023 08:44:52 GMT
               --  content-type: text/html; charset=UTF-8
               --  vary: Accept-Encoding
               --  link: <https://www.soweb.io/wp-json/>; rel="https://api.w.org/"
               --  link: <https://www.soweb.io/wp-json/wp/v2/pages/26>; rel="alternate"; type="application/json"
               --  link: <https://www.soweb.io/>; rel=shortlink
               --  x-cache-status: HIT
               --  strict-transport-security: max-age=63072000; includeSubdomains
               --  x-frame-options: SAMEORIGIN
               --  x-content-type-options: nosniff
               --  x-xss-protection: 1; mode=block

               --  www-soweb-io-homepage.html
               --
               --  Full UTF-8 home page with net links for ressources (images, graphics, etc.)

               Rc := Fclose (Body_Handle);
            end if;

            Rc := Fclose (Header_Handle);
         end if;

         Curl_Easy_Cleanup (Curl);

      end if;

      Msg.New_Line;

   end Run;

-------------------------------------------------------------------------------
end TestApi_Crl;
-------------------------------------------------------------------------------
