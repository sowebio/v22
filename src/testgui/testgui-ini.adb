-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      testgui-ini.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - GUI test program
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with GNAT.SHA512;

separate (TestGui) package body Ini is

   ----------------------------------------------------------------------------
   --  API
   ----------------------------------------------------------------------------

   ----------------------------------------------------------------------------
   function App return Boolean is
      App_Init_Success : Boolean := True;
   begin

      --  Settings
      Prg.Set_Version (0, 2);

      Prg.Set_Handler_Ctrl_C (On);
      Sys.Set_Memory_Monitor (On);

      Msg.Set_Header (On);
      Msg.Set_Disk (On);
      Msg.Set_Display (On);

      Msg.Set_Debug (Off); --  v22 debug messages activation
      Gui.Set_Debug (Off); --  Gnoga debug messages activation

      Tio.Set_Cursor (Off);

      --  Console banner
      Msg.New_Line;
      Msg.Info ("v22 Framework - GUI test program");
      Msg.Info ("Copyright (C) Sowebio SARL 2020-" & From_Latin_1 (GCT.Image (AC.Clock, "%Y")) & ", under LGPLv3");
      Msg.Info (Prg.Get_Version & " - " & v22.Get_Version & " - " & v22.Get_Build);
      Msg.New_Line;

      ----------------------------------------------------------------------------
      --  Command line parameters handling
      ----------------------------------------------------------------------------

      GCL.Set_Usage (Config, Usage => "[switches] [arguments] overview", Help =>  "This is the short help text");
      GCL.Define_Switch (Config, Exception_Test'Access, Switch => "-e", Help => "Enable exception test");
      GCL.Getopt (Config); --  Command line processing

      if Exception_Test then
         Msg.Info ("TestGui.Init.App > Exception test trigered by a raise exception");
         Raise_Exception;
      end if;

      ----------------------------------------------------------------------------
      --  Configuration file management
      ----------------------------------------------------------------------------

      if not Fls.Exists (Prg.Start_Dir & "/" & App_Config_File) then
         if Cfg.Open (Prg.Start_Dir & "/" & App_Config_File) then
            Cfg.Comment ("-----------------------------------------------------------------------------");
            Cfg.Comment (" " & Prg.Name & ".cfg - Configuration file");
            Cfg.Comment ("-----------------------------------------------------------------------------");
            Cfg.Comment ("");
            Cfg.Comment (" " & Prg.Date_Time_Stamp & " - First release");
            Cfg.Comment ("");
            Cfg.Comment ("-----------------------------------------------------------------------------");
            Cfg.New_Line;
            --  Reverse order in code as insert is allways done at the beginning of a section
            Cfg.Set ("Connection", "Tls_Private_Key", "A relative path to .key file");
            Cfg.Set ("Connection", "Tls_Certificate", "A relative path to .cer file");
            Cfg.Set ("Connection", "Port", "8080");
            Cfg.Set ("Connection", "Domain", "localhost");
            Cfg.New_Line;
            Cfg.Set ("Database", "Password", "User password");
            Cfg.Set ("Database", "User", "User name");
            Cfg.Set ("Database", "Name", "testgui");
            Cfg.Set ("Database", "Port", "3306");
            Cfg.Set ("Database", "Host", "Host name");
            Cfg.Set ("Database", "Type", "SQLite");
            Cfg.New_Line;
            Cfg.Comment ("-----------------------------------------------------------------------------");
            Cfg.Comment (" EOF");
            Cfg.Comment ("-----------------------------------------------------------------------------");
            Cfg.Close;
            Msg.Info ("TestGui.Init.App > Configuration file " & App_Config_File & " has been created");
         else
             Msg.Error ("TestGui.Init.App > Can't create " & App_Config_File);
             App_Init_Success := False;
         end if;
      end if;

      if Cfg.Open (Prg.Start_Dir & "/" & App_Config_File) then
         App_Config_Datas.Connection_Domain := Cfg.Get ("Connection", "Domain");
         App_Config_Datas.Connection_Port := To_Integer (Cfg.Get ("Connection", "Port"));
         App_Config_Datas.Connection_Tls_Certificate := Cfg.Get ("Connection", "Tls_Certificate");
         App_Config_Datas.Connection_Tls_Private_Key := Cfg.Get ("Connection", "Tls_Private_Key");
         App_Config_Datas.Database_Type := Cfg.Get ("Database", "Type");
         App_Config_Datas.Database_Host := Cfg.Get ("Database", "Host");
         App_Config_Datas.Database_Port := To_Integer (Cfg.Get ("Database", "Port"));
         App_Config_Datas.Database_Name := Cfg.Get ("Database", "Name");
         App_Config_Datas.Database_User := Cfg.Get ("Database", "User");
         App_Config_Datas.Database_Password := Cfg.Get ("Database", "Password");
         Msg.Info ("TestGui.Init.App > Configuration file " & App_Config_File & " loaded");

         --  Place holder for parameters validation
         --
         --

         Sql.Set_Database_Main (App_Config_Datas.Database_Name);

      else
         Msg.Error ("TestGui.Init.App > Can't open " & App_Config_File);
         App_Init_Success := False;
      end if;

      ----------------------------------------------------------------------------
      --  Database management
      ----------------------------------------------------------------------------

      if Sql.Get_Database_Brand (DB) = MySQL then
         --  db:db_name?host=host_name&port=port_number&user=user_name&password=user_password (conforms to RFC 3986)
         if Sql.Open (DB,"db:" & App_Config_Datas.Database_Name & "?" &
                        "host=" & App_Config_Datas.Database_Host & "&" &
                        "port=" & To_String_Unsigned (App_Config_Datas.Database_Port) & "&" &
                        "user=" & App_Config_Datas.Database_User & "&" &
                        "password=" & App_Config_Datas.Database_Password,
                        "1.0") = Sql.Open_Need_Update then

            --  Dealers table
            Sql.Schema_Load (Sql.Table_Name, "Tbl_Dealers", Comment => "Dealers table");
            Sql.Schema_Load (Sql.Column_Name, "Number", "INTEGER", "Dealer number");
            Sql.Schema_Load (Sql.Column_Constraint, "Number", "NOT NULL");
            Sql.Schema_Load (Sql.Column_Constraint, "Number", "AUTO_INCREMENT");
            Sql.Schema_Load (Sql.Table_Constraint, "Number", "PRIMARY KEY");

            Sql.Schema_Load (Sql.Column_Name, "Name", "TEXT", "Dealer name");
            Sql.Schema_Load (Sql.Column_Name, "Phone", "TEXT", "Dealer phone");
            Sql.Schema_Load (Sql.Column_Name, "Email", "TEXT", "Dealer email");
            Sql.Schema_Load (Sql.Column_Name, "Grants", "TEXT", "Login1,Login2...Login_N");
            Sql.Schema_Load (Sql.Column_Name, "Properties", "TEXT", "Property_1:Value,Property_2:value...Property_N");
            Sql.Schema_Load (Sql.Column_Name, "Notes", "TEXT", "Notes");
            Sql.Schema_Load (Sql.Column_Name, "Created_On", "VARCHAR(15)", "Dealer creation datetime stamp");
            Sql.Schema_Load (Sql.Column_Name, "Updated_On", "VARCHAR(15)", "Dealer update datetime stamp");

            Sql.Schema_Load (Sql.Column_Name, "Sales_Day", "BIGINT", "Sales of the current day");
            Sql.Schema_Load (Sql.Column_Name, "Sales_Day_Previous", "BIGINT", "Sales of the current previous day");
            Sql.Schema_Load (Sql.Column_Name, "Sales_Week", "BIGINT", "Sales of the current Week");
            Sql.Schema_Load (Sql.Column_Name, "Sales_Month", "BIGINT", "Sales of the current month");
            Sql.Schema_Load (Sql.Column_Name, "Sales_Year", "BIGINT", "Sales of the current year");
            Sql.Schema_Load (Sql.Column_Name, "Pieces_Day", "INTEGER", "Pieces sold for the current day");
            Sql.Schema_Load (Sql.Column_Name, "Pieces_Day_Previous", "INTEGER", "Pieces sold for the current previousday");
            Sql.Schema_Load (Sql.Column_Name, "Pieces_Week", "INTEGER", "Pieces sold for the current week");
            Sql.Schema_Load (Sql.Column_Name, "Pieces_Month", "INTEGER", "Pieces sold for the current month");
            Sql.Schema_Load (Sql.Column_Name, "Pieces_Year", "INTEGER", "Pieces sold for the current Year");

            Sql.Schema_Load (Sql.Index_Name, "Idx_User_Number", "Number");
            Sql.Schema_Load (Sql.Index_Constraint, "Idx_User_Number", "UNIQUE");

            Sql.Schema_Update (DB);
         end if;

      elsif Sql.Get_Database_Brand (DB) = SQLite then
         --  Always use SQLite in demo mode
         if Sql.Open (DB,"file:" & App_Config_Datas.Database_Name & ".db", "1.0") = Sql.Open_Need_Update then
            Sql.Schema_Update (DB);

            --  Populate Sys_Users for demo purposes
            declare
               Table : String := "Sys_Users";
               Query_Tail : String := "Phone~06 66 66 66 66" & "^" &
                                      "Email~contact@mail.fr" & "^" &
                                      "Password~" & From_Latin_1 (GNAT.SHA512.Digest ("password")) & "^" &
                                      "Grants~admin:ACUDPE" & "^" &
                                      "Notes~Default administrator" & "^" &
                                      "DTS_Creation~" & Prg.Date_Time_Stamp;
            begin
               Sql.Insert (DB, Table, "Login~alpha^First_Name~Alphonse^Last_Name~Arthémis^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~bravo^First_Name~Babar^Last_Name~Bombarde^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~charlie^First_Name~Charles^Last_Name~Charlot^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~delta^First_Name~Didi^Last_Name~Dédale^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~echo^First_Name~Ernest^Last_Name~Écailler^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~foxtrot^First_Name~Fabrice^Last_Name~Fabulous^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~golf^First_Name~Gérard^Last_Name~Guimbarde^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~hotel^First_Name~Henri^Last_Name~Hannibal^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~india^First_Name~Isidore^Last_Name~Ingénieux^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~juliet^First_Name~Jules^Last_Name~Jambon^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~kilo^First_Name~Katerine^Last_Name~Kilomètre^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~lima^First_Name~Limace^Last_Name~Lagaff^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~mike^First_Name~Mastar^Last_Name~Magritte^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~november^First_Name~Norbert^Last_Name~Nunuche^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~oscar^First_Name~Original^Last_Name~Opossum^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~papa^First_Name~Philibert^Last_Name~Philistin^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~quebec^First_Name~Quentin^Last_Name~Quintilus^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~romeo^First_Name~Riton^Last_Name~Ravi^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~sierra^First_Name~Samuel^Last_Name~Sisyphe^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~tango^First_Name~Tara^Last_Name~Totor^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~uniform^First_Name~Ursule^Last_Name~Urtiquant^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~victor^First_Name~Vénus^Last_Name~Victoria^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~whisky^First_Name~Walter^Last_Name~Whombat^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~xray^First_Name~Xantia^Last_Name~Xanadu^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~yankee^First_Name~Yoman^Last_Name~Yellow^" &  Query_Tail);
               Sql.Insert (DB, Table, "Login~zulu^First_Name~Zoé^Last_Name~Zadig^" &  Query_Tail);
            end;

         end if;

      end if;

      return App_Init_Success;

   end App;

-------------------------------------------------------------------------------
end Ini;
-------------------------------------------------------------------------------
