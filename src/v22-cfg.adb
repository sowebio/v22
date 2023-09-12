-------------------------------------------------------------------------------
--  ▖▖▄▖▄▖
--  ▌▌▄▌▄▌
--  ▚▘▙▖▙▖
--
--  @file      v22-cfg.adb
--  @copyright See authors list below and v22.copyrights file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - config file manager
--
--  @description
--
--  @authors
--  Stéphane Rivière - sr - sriviere@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

with Ada.Directories;

with v22.Fls;
with v22.Msg;

package body v22.Cfg is

   --  Service functions

   ----------------------------------------------------------------------------
   function Table_Write (Line : String) return Boolean is
      Result : Boolean := True;
   begin
      if Cfg_Last = Table_Max then
         Msg.Err ("v22.Cfg.Table_Write > Too large to load: " & Cfg_File_Read);
         Cfg_Last := 0;
         Result := False;
      else
         Cfg_Last := Cfg_Last + 1;
         Cfg_Table (Cfg_Last) := Line;
      end if;
      return Result;
   end Table_Write;

   ----------------------------------------------------------------------------
   function Cfg_Read return Boolean is
      Line_Buffer : String := "";
      Result : Boolean := True;
   begin
      Cfg_Last := 0;
      Tio.Reset (Handle_Read);
      while (not Tio.End_Of_File (Handle_Read)) and Result loop
         Tio.Get_Line (Handle_Read, Line_Buffer);
         Result := Table_Write (Line_Buffer);
      end loop;
      return Result;
   end Cfg_Read;

   ----------------------------------------------------------------------------
   function Check_Parameters (Section : String;
                              Parameter : String;
                              Value : String)
                              return Boolean is
      String_To_Test : constant String := Section & Parameter & Value;
   begin
      return (Index (String_To_Test, Cfg_Open_Section) +
              Index (String_To_Test, Cfg_Close_Section) +
              Index (String_To_Test, Cfg_Assignment) +
              Index (String_To_Test, Cfg_Comment) = 0);
   end Check_Parameters;

   procedure Cfg_Search (Section : String; Parameter : String) is
      Pos_Open_Section : Natural := 0;
      Pos_Close_Section : Natural := 0;
      Pos_Assignment : Natural := 0;
   begin
      Cfg_Section := 0;
      Cfg_Parameter := 0;
      for I in 1 .. Cfg_Last loop
         --  Section found, search for parameter
         if Cfg_Section > 0 then
            --  Section break, parameter not found
            if Index (Cfg_Table (I), Cfg_Open_Section) > 0 and
               Index (Cfg_Table (I), Cfg_Close_Section) > 0
            then
               exit;
            else
               Pos_Assignment := Index (Trim_Left (Cfg_Table (I)),
                                        Cfg_Assignment);
               --  At least a parameter name of one char
               if Pos_Assignment > 2 then
                  --  Parameter found
                  if Parameter = Trim_Both (Slice (Cfg_Table (I), 1,
                                                  Pos_Assignment - 1))
                  then
                     Cfg_Parameter := I;
                     exit;
                  end if;
               end if;
            end if;
            --  Search for section
         else
            Pos_Open_Section := Index (Cfg_Table (I), Cfg_Open_Section);
            if Pos_Open_Section > 0 then
               Pos_Close_Section := Index (Cfg_Table (I), Cfg_Close_Section);
               --  At least a section name of one char
               if Pos_Close_Section > 0 and
                  (Pos_Close_Section - Pos_Open_Section > 1)
               then
                  if Section = Trim_Both (Slice (Cfg_Table (I),
                             Pos_Open_Section + 1, Pos_Close_Section - 1))
                  then
                     Cfg_Section := I;
                  end if;
               end if;
            end if;
         end if;
      end loop;
   end Cfg_Search;

   ----------------------------------------------------------------------------
   function Cfg_Write (Section : String := "";
                       Parameter : String := "";
                       Value : String := "";
                       Trailing_Comment : String := "")
                       return Boolean is
      Result : Boolean := True;
   begin

      Tio.Create (Handle_Write, Cfg_File_Write);
      if Tio.Is_Open (Handle_Write) then
         for I in 1 .. Cfg_Last loop

            --  Create Parameter
            if Index (Cfg_Table (I), Cfg_Command_Add) > 0 then
               if Index (Cfg_Table (I), Cfg_Comment) > 0 then
                  Cfg_Table (I) := Cfg_Open_Section & Section &
                                   Cfg_Close_Section &  " " &
                  Slice (Cfg_Table (I), Index (Cfg_Table (I),
                         Cfg_Comment),
                         Length (Cfg_Table (I)));
               else
                  Cfg_Table (I) := Cfg_Open_Section & Section &
                                   Cfg_Close_Section;
               end if;
               Tio.Put_Line (Handle_Write, Cfg_Table (I));
               Tio.Put_Line (Handle_Write, Parameter & " "  &
                                           Cfg_Assignment & " " &
                                           Value &
                                           Trailing_Comment);
            --  Delete Line
            elsif Index (Cfg_Table (I), Cfg_Command_Delete) > 0 then
               null;

            --  Copy Line
            else
               Tio.Put_Line (Handle_Write, Cfg_Table (I));
            end if;

         end loop;

         --  Replacing old cfg file by the updated new one
         Tio.Close (Handle_Write);
         Tio.Close (Handle_Read);
         Fls.Delete_File (Cfg_File_Read);
         Fls.Rename (Cfg_File_Write, Cfg_File_Read);
         Tio.Open_Read (Handle_Read, Cfg_File_Read);

         if Tio.Is_Open (Handle_Read) then
            Result := Cfg_Read;
         else
            Msg.Err ("v22.Cfg.Cfg_Write > Can't open new: " & Cfg_File_Read);
            Result := False;
         end if;
      else
         Msg.Err ("v22.Cfg.Cfg_Write > Can't create: " & Cfg_File_Write);
         Result := False;
      end if;
      return Result;
   end Cfg_Write;

   ----------------------------------------------------------------------------
   procedure Set (Section : String;
                  Parameter : String;
                  Value : String;
                  Comment : String := "") is

      Trailing_Comment : String := "";
   begin
      --  Check open/close section, assignment and comment reserved characters
      --  in Section, Parameter and Value
      if Check_Parameters (Section, Parameter, Value) then

         Cfg_Search (Section, Parameter);

         if Comment /= "" then
            Trailing_Comment := "  # " & Comment;
         end if;

         --  Update parameter
         if Cfg_Section > 0 and Cfg_Parameter > 0 then

            --  Preserve parameter comment when updating
            if Index (Cfg_Table (Cfg_Parameter), Cfg_Comment) > 0 then
               Cfg_Table (Cfg_Parameter) := Parameter & " " & Cfg_Assignment &
                                            " " & Value & " " &
               Slice (Cfg_Table (Cfg_Parameter),
                      Index (Cfg_Table (Cfg_Parameter), Cfg_Comment),
                      Length (Cfg_Table (Cfg_Parameter)));
            else
               Cfg_Table (Cfg_Parameter) := Parameter & " " &
                                            Cfg_Assignment & " " &
                                            Value &
                                            Trailing_Comment;
            end if;

         --  Create Parameter => tag the section for further just in time
         --  write in file
         elsif Cfg_Section > 0 then

            --  Preserve section comment when tagging
            if Index (Cfg_Table (Cfg_Section), Cfg_Comment) > 0 then
               Cfg_Table (Cfg_Section) := Cfg_Command_Add & " " &
                      Slice (Cfg_Table (Cfg_Section),
                      Index (Cfg_Table (Cfg_Section), Cfg_Comment),
                      Length (Cfg_Table (Cfg_Section)));
            else
               Cfg_Table (Cfg_Section) := Cfg_Command_Add;
            end if;

            --  Create Section and Parameter
         else
            if Table_Max >= Cfg_Last + 2 then
               Cfg_Table (Cfg_Last + 1) := Cfg_Open_Section &
                                           Section &
                                           Cfg_Close_Section;
               Cfg_Table (Cfg_Last + 2) := Parameter & " " &
                                           Cfg_Assignment & " " &
                                           Value &
                                           Trailing_Comment;
               Cfg_Last := Cfg_Last + 2;
            else
               Msg.Err ("v22.Cfg.Set > Too large: " & Cfg_File_Read);
               Msg.Err ("v22.Cfg.Set > For further writes to: " & Cfg_File_Write);
            end if;
         end if;

         if not Cfg_Write (Section, Parameter, Value, Trailing_Comment) then
            Msg.Err ("v22.Cfg.Set > Cfg_Write operation failed");
         end if;
      else
         Msg.Err ("v22.Cfg.Set > Invalid character in Section, Parameter or Value");
      end if;
   end Set;

   ----------------------------------------------------------------------------
   function Get (Section : String; Parameter : String) return String is
      Result : String := "";
   begin

      Cfg_Search (Section, Parameter);

      --  Get parameter
      if Cfg_Section > 0 and Cfg_Parameter > 0 then
         Result := Cfg_Table (Cfg_Parameter);
         --  Suppress comment
         if Index (Result, Cfg_Comment) > 0 then
            Result := Slice (Result, 1, Index (Result, Cfg_Comment) - 1);
         end if;
         Result := Trim_Both (Slice (Result,
                                     Index (Result, Cfg_Assignment) + 1,
                                     Length (Result)));
      end if;

      return Result;

   end Get;

   procedure Delete (Section : String; Parameter : String) is
      Count_Parameters : Natural := 0;
   begin

      Cfg_Search (Section, Parameter);

      --  Get parameter
      if Cfg_Section > 0 and Cfg_Parameter > 0 then
         Cfg_Table (Cfg_Parameter) := Cfg_Command_Delete;

         --  Does Section owns at least one parameter ?
         for I in (Cfg_Section + 1) .. Cfg_Last loop
            --  Section break
            if Index (Cfg_Table (I), Cfg_Open_Section) > 0 and
               Index (Cfg_Table (I), Cfg_Close_Section) > 0
            then
               exit;
            end if;
            --  Not the deleted parameter, not and empty line nor a
            --  commented line
            if (
               (I /= Cfg_Parameter) or
               (Index (Cfg_Table (I), Cfg_Assignment) > 0)) and
               not (Index (Cfg_Table (I), Cfg_Comment) > 0)
            then
               Count_Parameters := Count_Parameters + 1;
            end if;
         end loop;
         --  Delete section too
         if Count_Parameters = 0 then
            Cfg_Table (Cfg_Section) := Cfg_Open_Section &
                                       Cfg_Open_Section & "D";
         end if;

         if not Cfg_Write then
            Msg.Err ("v22.Cfg.Delete > Cfg_Write operation failed");
         end if;
      end if;
   end Delete;

   ----------------------------------------------------------------------------
   procedure Comment (Text : String) is
      Dummy : Boolean;
   begin
      if Table_Write ("# " & Text) then
         Dummy := Cfg_Write;
      end if;
   end Comment;

   ----------------------------------------------------------------------------
   function Open (Cfg_File_Read_In : String := "") return Boolean is
      Result : Boolean := False;
   begin
      --  Custom name
      if Cfg_File_Read_In /= "" then
         Cfg_File_Read := Cfg_File_Read_In;
      end if;
      --  Create if non-existent
      if not Ada.Directories.Exists (To_Latin_1 (Cfg_File_Read)) then
         Tio.Create (Handle_Read, Cfg_File_Read);
         if Tio.Is_Open (Handle_Read) then
            Close;
         else
            Msg.Err ("v22.Cfg.Open > Can't create Cnf file");
         end if;
      end if;
      --  Open in read mode
      Tio.Open_Read (Handle_Read, Cfg_File_Read);
      if Tio.Is_Open (Handle_Read) then
         Result :=  Cfg_Read;
      end if;
      return Result;
   end Open;

   ----------------------------------------------------------------------------
   procedure Close is
   begin
      if Tio.Is_Open (Handle_Read) then
         Tio.Close (Handle_Read);
      else
         Msg.Err ("v22.Cfg.close > Should be opened before closing: " & Cfg_File_Read);
      end if;
   end Close;

-------------------------------------------------------------------------------
end v22.Cfg;
-------------------------------------------------------------------------------
