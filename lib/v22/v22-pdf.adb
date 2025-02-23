-------------------------------------------------------------------------------
--
--  _|      _|    _|_|      _|_|
--  _|      _|  _|    _|  _|    _|
--  _|      _|      _|        _|
--    _|  _|      _|        _|
--      _|      _|_|_|_|  _|_|_|_|
--
--  @file      v22-pdf.adb
--  @copyright See authors list below and README.md file
--  @licence   LGPL v3
--  @encoding  UTF-8
-------------------------------------------------------------------------------
--  @summary
--  V22 framework - PDF package
--
--  @description
--
--  @authors
--  Lounès Souakri - ls - developpement@soweb.io
--
--  @versions
--  See git log
-------------------------------------------------------------------------------

package body v22.Pdf is
   
   -- TYPES

   ----------------------------------------------------------------------------
   function Get_Scale (Object : in out PDF_Out.PDF_Out_File) return Real is
      Scale : Real;
   begin
      Set_Font (Object);
      --Object.Font (Courier);
      case Integer (Object.Get_Font_Size) is
         when 9  => Scale := 110.1;
         when 10 => Scale := 99.1;
         when 11 => Scale := 90.2;
         when 12 => Scale := 82.6;
         when 13 => Scale := 76.2;
         when 14 => Scale := 70.9;
         when 15 => Scale := 66.1;
         when 16 => Scale := 62.0;
         when 17 => Scale := 58.3;
         when 18 => Scale := 55.1;
         when 19 => Scale := 52.1;
         when 20 => Scale := 49.8; --- à vérifier
         when 21 => Scale := 47.2;
         when others => Scale := 82.6;
      end case;
      return Scale;
   end Get_Scale;
     
   ----------------------------------------------------------------------------
   function Get_Size (Object : in out PDF_Out.PDF_Out_File; Str : String) return Real is
      Char_Width  : Real;
      Result : Real;
      Text : Standard.String := To_Latin_1 (Str);
   begin
      Char_Width := Full_X / Get_Scale (Object);
      Result := Real (Text'Length) * Char_Width;
      return Result;
   end Get_Size;

   ----------------------------------------------------------------------------
   function Get_X_Align (Object : in out PDF_Out.PDF_Out_File; X : Real ; Str : String) return Real is
      Result : Real;
   begin
      Result := X - Get_Size (Object,Str) / 2.0 ;
      return Result;
   end Get_X_Align;
      
   ----------------------------------------------------------------------------
   procedure Put_X_Align (Object : in out PDF_Out.PDF_Out_File; X, Y : Real; Str : String) is
      Text : Standard.String := To_Latin_1 (Str);
   begin
      Object.Put_XY (Get_X_Align (Object,X,Str),Y,Text);
   end Put_X_Align;   
      
   ----------------------------------------------------------------------------
   function Get_X_Right (Object : in out PDF_Out.PDF_Out_File; Right : Real ; Str : String) return Real is
      Result : Real;
   begin
      Result := Full_X - (Get_Size (Object,Str) + Right);
      return Result;
   end Get_X_Right;
      
   ----------------------------------------------------------------------------
   procedure Put_X_Right (Object : in out PDF_Out.PDF_Out_File; Right, Y : Real; Str : String) is
      Text : Standard.String := To_Latin_1 (Str);
   begin
      Object.Put_XY (Get_X_Right (Object,Right, Str), Y ,Text);
   end Put_X_Right;   
      
   ----------------------------------------------------------------------------
   function Get_X_Center (Object : in out PDF_Out.PDF_Out_File; Str : String) return Real is
      Midle_Size : Real;
      Result : Real;
   begin
      Midle_Size := Get_Size (Object, Str) / 2.0;
      Result := Full_X / 2.0 - Midle_Size ;
      return Result;
   end Get_X_Center;
      
   ----------------------------------------------------------------------------
   procedure Put_X_Center (Object : in out PDF_Out.PDF_Out_File; Y : Real; Str : String) is
      Text : Standard.String := To_Latin_1 (Str);
   begin
      Object.Put_XY (Get_X_Center (Object, Str),Y,Text);
   end Put_X_Center;

   ----------------------------------------------------------------------------
   procedure Put_Link (Object : in out PDF_Out.PDF_Out_File; 
                           X, Y : Real ; 
                           Url : String ; 
                           Pre : Reference ; 
                           Str : String := "" ; 
                           Color : Color_Type := (0.0,0.0,0.4)) is
                           
      Text : Standard.String := To_Latin_1 (Str);
      Link : Standard.String := To_Latin_1 (Url);
      Y_Under : Real;
      Height : Real;
      Font_Size : Integer := Integer (Object.Get_Font_Size);
      Real_Text : String;
      Before : String;
   begin
      if Text = "" then
         Real_Text := From_Latin_1 (Link);
      else
         Real_Text := From_Latin_1 (Text);
      end if;
      case Font_Size is
         when 12 =>
            Y_Under := 0.1 * one_cm ;
            Height := 0.4 * one_cm;
         when 16 =>
            Y_Under := 0.15 * one_cm ;
            Height := 0.55 * one_cm;
         when 15 =>
            Y_Under := 0.2 * one_cm ;
            Height := 0.65 * one_cm;
         when others =>
            Y_Under := 0.1 * one_cm ;
            Height := 0.4 * one_cm;
      end case;
      Filling_Color (Object,Color);
      Object.Put_XY (X, Y, To_Latin_1 (Real_Text));
      Filling_Color (Object,black); 
      case Pre is
         when Go_To => Object.Hyperlink ((X, Y - Y_Under, Get_Size (Object, Real_Text), Height), False, Uxs.To_Integer (From_Latin_1 (Link)) + 1);
         when Mail_To => Object.Hyperlink ((X, Y - Y_Under, Get_Size (Object, Real_Text), Height), False, "mailto:" & Link);
         when URI => Object.Hyperlink ((X, Y - Y_Under, Get_Size (Object, Real_Text), Height), False, "https://" & Link);
      end case;
   end Put_Link;   

   ----------------------------------------------------------------------------
   procedure Put_Pie_Chart (Object : in out PDF_Out.PDF_Out_File; Center : Point; Radius : Real; Data : Pie_Content) is
      Total_Value : Pie_Value := 0.0;
      First_Angle , Second_Angle, Middle_Angle : Real := 180.0;
      Radian : constant := Pi / 180.0 ;
      Label_X , Label_Y : Real;
      Percentage : Integer;
   begin
      for Slice in Data'Range loop
         Total_Value := Total_Value + Data(Slice).Value ;
      end loop;
      if Total_Value = 0.0 then
         Put_X_Align (Object, Center.x,Center.y,"Pie chart not available");
      else
         for Slice in Data'Range loop
            Object.Font_Size (11.0);
            Percentage := Integer (Data(Slice).Value * 100.0 / Total_Value);
            Second_Angle := Second_Angle + Real (Data(Slice).Value / Total_Value) * 360.0 ;
            Move (Object, Center);
            Filling_Color (Object, Data(Slice).Color);
            Arc (Object, Center, Radius, First_Angle, Second_Angle, True);
            Middle_Angle := (First_Angle + Second_Angle) / 2.0;
            Label_X := (Center.x + (Radius * 0.75) * Cos(Middle_Angle * Radian));
            Label_Y := (Center.y + (Radius * 0.75) * Sin(Middle_Angle * Radian)) - one_cm * 0.125;
            Label_X := Get_X_Align (Object, Label_X, Uxs.To_String (Percentage) & "%");
            First_Angle := Second_Angle;
            Line (Object, Center);
            Finish_Path (Object, False, fill_then_stroke, nonzero_winding_number);
            Filling_Color (Object, black);
            Object.Put_XY (Label_X,Label_Y,To_UTF_8 (Uxs.To_String (Percentage) & "%"));
         end loop;
         Filling_Color (Object, black);
      end if;
   end Put_Pie_Chart;
   
   ----------------------------------------------------------------------------
   procedure Set_Header (Logo_Path, Line_One, Line_Two : String; Logo_Url : String := "") is
   begin
      Object_Header.Path_To_Logo := Logo_Path ;
      Object_Header.First_Line := Line_One ;
      Object_Header.Second_Line := Line_Two ;
      Object_Header.Logo_Redirect := Logo_Url ;
   end Set_Header;
   
   ----------------------------------------------------------------------------
   procedure Put_Header (Object : in out PDF_Out.PDF_Out_File; Title : String) is
      Logo_Path : String := Object_Header.Path_To_Logo ;
      Line_One : String := Object_Header.First_Line ;
      Line_Two : String := Object_Header.Second_Line ;
      Logo_Url : String := Object_Header.Logo_Redirect ;
   begin
      Set_Font (Object);
      Object.Font_Size (12.0);
      Object.Image (To_Latin_1 (Logo_Path),(2.0 * one_cm, 27.0 * one_cm, 2.675 * one_cm, 1.68 * one_cm));
      Object.Hyperlink ((2.0 * one_cm, 27.0 * one_cm, 2.675 * one_cm, 1.68 * one_cm), False, To_Latin_1 (Logo_Url));
      if Starts_With (Line_One,"www.") then
         Put_Link (Object, Get_X_Right (Object ,2.0 * one_cm, Line_One), 28.0 * one_cm, Line_One, URI);
      elsif Index (Line_One, "@") > 0 then
         Put_Link (Object, Get_X_Right (Object ,2.0 * one_cm, Line_One), 28.0 * one_cm, Line_One, Mail_To);
      else 
         Put_X_Right (Object, 2.0 * one_cm, 28.0 * one_cm, Line_One);
      end if;
      if Starts_With (Line_Two,"www.") then
         Put_Link (Object, Get_X_Right (Object ,2.0 * one_cm, Line_Two), 27.2 * one_cm, Line_Two, URI);
      elsif Index (Line_Two, "@") > 0 then
         Put_Link (Object, Get_X_Right (Object ,2.0 * one_cm, Line_Two), 27.2 * one_cm, Line_Two, Mail_To);
      else 
         Put_X_Right (Object, 2.0 * one_cm, 27.2 * one_cm, Line_Two);
      end if;
      Object.Font_Size (15.0);
      Object.Put_XY (2.0 * one_cm,24.5*one_cm,To_Latin_1 (Title));
      Object.Draw ((1.0 * one_cm, 23.8 * one_cm, 19.0 * one_cm, 0.01 * one_cm), fill);
      Object.Font_Size (12.0);
   end Put_Header;
   
   ----------------------------------------------------------------------------
   procedure Set_Footer (Website, Contact, Item : String := ""; Paginate : Integer := 0) is
   begin
      Object_Footer.Website := Website;
      Object_Footer.Contact := Contact;
      Object_Footer.Item := Item;
      Object_Footer.Paginate := Paginate;
   end Set_Footer;
    
   ----------------------------------------------------------------------------
   procedure Put_Footer (Object : in out PDF_Out.PDF_Out_File) is
    
   begin
      Set_Font (Object);
      Object.Font_Size (12.0);
      if Object_Footer.Website /= "" then
         Put_Link (Object, 2.0 * one_cm, 2.0 * one_cm, Object_Footer.Website, URI);
         if Object_Footer.Contact /= "" then 
            Object.Put_XY (Get_Size(Object,Object_Footer.Website) + (2.0 * one_cm) + (0.2 * one_cm), 2.0 * one_cm, "|");
            Put_Link (Object, 
                      Get_Size(Object,Object_Footer.Website) + (0.4 * one_cm) + (2.0 * one_cm) + Get_Size(Object,"|"), 
                      2.0 * one_cm, 
                      Object_Footer.Contact, 
                      Mail_To);
         end if;
      elsif Object_Footer.Contact /= "" then
         Put_Link (Object, 2.0 * one_cm, 2.0 * one_cm, Object_Footer.Contact, Mail_To);
      end if;
      if Object_Footer.Item /= "" then
         if Starts_With (Object_Footer.Item,"www.") then
            Put_Link (Object, Get_X_Right (Object ,4.0 * one_cm, Object_Footer.Item), 2.0 * one_cm, Object_Footer.Item, URI);
         elsif Index (Object_Footer.Item, "@") > 0 then
            Put_Link (Object, Get_X_Right (Object ,4.0 * one_cm, Object_Footer.Item), 2.0 * one_cm, Object_Footer.Item, Mail_To);
         else 
            Put_X_Right (Object, 4.0 * one_cm, 2.0 * one_cm, Object_Footer.Item);
         end if;
      end if;      
      if Object_Footer.Paginate > 0 then
         Put_X_Right (Object, 2.0 * one_cm,2.0 * one_cm,Uxs.To_String (Object.Page) & "/" & Uxs.Trim_Left (Uxs.To_String (Object_Footer.Paginate)));
      end if;
    
      
      
      
      
   end Put_Footer;
    
   ----------------------------------------------------------------------------
   procedure Set_Font (Object : in out PDF_Out.PDF_Out_File) is
   begin
      Object.Font (Courier);
   end Set_Font;
   
   ----------------------------------------------------------------------------
     
   
-------------------------------------------------------------------------------
end v22.Pdf;
-------------------------------------------------------------------------------
