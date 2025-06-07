with Ada.Calendar.Conversions;
with Ada.Characters.Handling;
with Ada.Command_Line;
with Ada.Containers.Hashed_Sets;
with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Containers.Vectors;
with Ada.Directories;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Text_IO;
with GNATCOLL.JSON;
with Libre_Frame.Edit;

package body Libre_Frame.UI.Server is
   use type Containers.Count_Type;

   package Client_Datas_By_ID_Maps is new
     Containers.Ordered_Maps (Key_Type => Client_ID_Type, Element_Type => Client_Data);

   package Connections_By_ID_Maps is new
     Containers.Ordered_Maps (Key_Type => Natural, Element_Type => Connection_Access);

   package Connections_By_Client_ID_Maps is new
     Containers.Ordered_Maps
       (Key_Type     => Client_ID_Type,
        Element_Type => Connections_By_ID_Maps.Map,
        "="          => Connections_By_ID_Maps."=");

   Client_Datas_By_ID       : Client_Datas_By_ID_Maps.Map;
   Connections_By_Client_ID : Connections_By_Client_ID_Maps.Map;

   Current_Connection_ID : Natural := 0;

   Initialized_View_Names : View_Labels := View_Names;
   Initialized_View_Paths : View_Labels := View_Paths;

   Views_JSON_Object : Unbounded_String;

   function Format_Label (Source : Unbounded_String) return Unbounded_String is
      I : constant Natural := Index (Source, "##");
   begin
      return (if I = 0 then Source else Unbounded_Slice (Source, 1, I - 1));
   end Format_Label;

   procedure Append_JSON (Target : in out JSON.JSON_Value; Source : Widget_Access) is
   begin
      if Source.Kind in Text then
         return;
      end if;

      declare
         Obj : constant JSON.JSON_Value := JSON.Create_Object;
      begin
         if Source.Kind in Interactive_Widget then
            Obj.Set_Field ("enabled", Source.Enabled);
         end if;

         case Source.Kind is
            when Text =>
               raise Program_Error with "unreachable";

            when Bar =>
               Obj.Set_Field ("value", Source.Bar_Value);

            when Button =>
               Obj.Set_Field ("size", Source.Size'Image);

            when Checkbox =>
               Obj.Set_Field ("checked", Source.Checkbox_Value);

            when Integer_Field =>
               Obj.Set_Field ("min", Source.Min);
               Obj.Set_Field ("max", Source.Max);
               Obj.Set_Field ("value", Source.Integer_Value);

            when Text_Field | Password_Field =>
               Obj.Set_Field ("value", Source.Text_Value);

            when Date_Field =>
               Obj.Set_Field ("min", Long_Integer (Calendar.Conversions.To_Unix_Time (Source.Date_Min)));
               Obj.Set_Field ("date", Long_Integer (Calendar.Conversions.To_Unix_Time (Source.Date_Value)));

            when Option_Field =>
               declare
                  Labels : JSON.JSON_Array;
               begin
                  Obj.Set_Field ("index", Source.Index);
                  for I in Source.Labels.Iterate (Source.Labels.First) loop
                     JSON.Append (Labels, JSON.Create (Source.Labels (I)));
                  end loop;
                  Obj.Set_Field ("labels", Labels);
               end;

            when Breadcrumb =>
               Obj.Set_Field ("index", Source.Index);

            when View =>
               Obj.Set_Field ("index", Source.Index);
               Obj.Set_Field_If_Not_Empty ("description", Source.Description);

            when Box =>
               Obj.Set_Field ("axis", Source.Axis'Image);
               Obj.Set_Field ("content_alignment", Source.Content_Alignment'Image);
               Obj.Set_Field ("fill", Source.Fill);
               Obj.Set_Field ("bordered", Source.Bordered);
               Obj.Set_Field ("spacing", Source.Spacing);

            when Separator =>
               Obj.Set_Field ("visible", Source.Visible);
               Obj.Set_Field ("size", Source.Separator_Size'Image);

            when Rich =>
               declare
                  use type Calendar.Time;
                  Segments    : JSON.JSON_Array;
                  Segment_Obj : JSON.JSON_Value;
                  Segment     : Rich_Text_Segment;
               begin
                  for I in Source.Rich.Segments.Iterate (Source.Rich.Segments.First) loop
                     Segment := Source.Rich.Segments (I);
                     Segment_Obj := JSON.Create_Object;
                     Segment_Obj.Set_Field ("text", Segment.Text);
                     Segment_Obj.Set_Field ("highlighted", Segment.Highlighted);
                     Segment_Obj.Set_Field ("new_line", Segment.New_Line);

                     if Segment.Date /= Unset_Time then
                        Segment_Obj.Set_Field
                          ("date", Long_Integer (Calendar.Conversions.To_Unix_Time (Segment.Date)));
                     end if;

                     JSON.Append (Segments, Segment_Obj);
                  end loop;
                  Obj.Set_Field ("fixed_width", Source.Rich.Fixed_Width);
                  Obj.Set_Field ("segments", Segments);
               end;
         end case;
         Target.Set_Field ("data", Obj);
      end;
   end Append_JSON;

   overriding
   function Create
     (Factory  : access Factory_Type;
      Listener : access Sockets.Server.Connections_Server'Class;
      From     : Sockets.Sock_Addr_Type) return Sockets.Server.Connection_Ptr
   is
      Result : constant Connection_Access :=
        new HTTP_Connection
              (Listener => Listener,
               Request_Length => Factory.Request_Length,
               Input_Size => Factory.Input_Size,
               Output_Size => Factory.Output_Size);
   begin
      Result.Self := Result;
      return Sockets.Server.Connection_Ptr (Result);
   end Create;

   overriding
   procedure Trace_Error
     (Factory : in out Factory_Type; Context : String; Occurrence : Exceptions.Exception_Occurrence) is
   begin
      Text_IO.Put_Line (Context & ": " & Exceptions.Exception_Information (Occurrence));
   end Trace_Error;

   overriding
   procedure Do_Get (Connection : in out HTTP_Connection) is
      use type HTTP.Status_Line_Type;
      Status : constant HTTP.Status_Line := Connection.Get_Status_Line;
   begin
      pragma Assert (Status.Kind = HTTP.File);
      Connection.Send_Status_Line (200, "OK");
      Connection.Send_Date;
      Connection.Send
        ("Cache-Control: no-cache, no-store, max-age=0, must-revalidate, proxy-revalidate"
         & Sockets.Connection_State_Machine.HTTP_Server.CRLF);
      Connection.Send_Content_Type ("text/html");
      Connection.Send_Body
        (Read_File (Directories.Compose (Directories.Containing_Directory (Command_Line.Command_Name), "index.html")));
   end Do_Get;

   overriding
   function WebSocket_Open (Connection : access HTTP_Connection) return HTTP.WebSocket_Accept is
      use type HTTP.Status_Line_Type;
      Status : constant HTTP.Status_Line := Connection.Get_Status_Line;
   begin
      pragma Assert (Status.Kind = HTTP.File);
      if Status.File (1 .. Status.Path_Length) /= "ws" then
         return (Accepted => False, Length => 0, Code => 404, Reason => "");
      else
         return (Accepted => True, Length => 0, Size => 1e6, Duplex => True, Chunked => False, Protocols => "");
      end if;
   end WebSocket_Open;

   overriding
   procedure WebSocket_Initialize (Connection : in out HTTP_Connection) is
   begin
      Connection.ID := Current_Connection_ID;
      Current_Connection_ID := @ + 1;

      Connection.Window.View_Names := Initialized_View_Names;
      Connection.Window.View_Paths := Initialized_View_Paths;
      Text_IO.Put_Line ("New window");

      Connection.WebSocket_Send (To_String (Views_JSON_Object));

      pragma Assert (Connection.State = Init);
      Connection.State := ID_Exchange;
   end WebSocket_Initialize;

   overriding
   procedure WebSocket_Finalize (Connection : in out HTTP_Connection) is
   begin
      for Item of Connection.Window.New_Widgets loop
         Free (Item);
      end loop;
      Connections_By_Client_ID (Connection.Client_ID).Delete (Connection.ID);
      Text_IO.Put_Line ("Window closed");
   end WebSocket_Finalize;

   not overriding
   procedure WebSocket_Send (Connection : in out HTTP_Connection; Data : Bytes) is
   begin
      Connection.WebSocket_Send (To_Stream_Element_Array (Data));
   end WebSocket_Send;

   overriding
   procedure WebSocket_Received (Connection : in out HTTP_Connection; Data : String) is
   begin
      pragma Assert (Data'Length > 0);
      raise Program_Error with "unreachable";
   end WebSocket_Received;

   procedure Redraw_Window_And_Send_Updates (Connection : in out HTTP_Connection) is
      Frame : Container := (Window => Connection.Window'Unchecked_Access, others => null);

      Previous_View       : constant Views := Connection.Window.Current_View;
      Event_Received_Copy : constant Natural := Connection.Window.Event_ID;

      Next_View : Views;
   begin
      for I in 1 .. 10 loop
         Connection.New_Widgets_Copy.Move (Connection.Window.New_Widgets);
         Next_View :=
           Redraw_Window
             (Current_View => Connection.Window.Current_View,
              Frame        => Frame,
              Client       => Connection.Client_Data_Access.all,
              Window       => Connection.Current_Window_Data);
         Connection.Window.Event_ID := 0;

         if Next_View /= Connection.Window.Current_View then
            Connection.Window.Widgets_By_ID.Clear;
            Connection.Window.Current_View := Next_View;
            goto Continue;
         end if;

         declare
            -- Make sure the view has distinct widget paths  TODO: Also check the widget kind hierarchy
            package String_Set is new Containers.Indefinite_Hashed_Sets (String, Strings.Hash, "=");
            Path_Set : String_Set.Set := String_Set.Empty (Connection.Window.New_Widgets.Length);
         begin
            for Item of Connection.Window.New_Widgets loop
               if Item.Kind in Labeled_Widget then
                  Path_Set.Insert (Get_Path (Item));
               end if;
            end loop;
         end;

         exit when
           Connection.Window.New_Widgets.Length = Connection.New_Widgets_Copy.Length
           and then (for all I in Connection.Window.New_Widgets.First_Index .. Connection.Window.New_Widgets.Last_Index
                     => Equivalent_Widgets (Connection.Window.New_Widgets (I), Connection.New_Widgets_Copy (I)));

         if I > 2 then
            Text_IO.Put_Line ("LOOP: " & I'Image);
         end if;

         pragma Assert (I < 5, "TODO");
         <<Continue>>
      end loop;

      declare
         package Widget_Edit is new Edit (Widget_Vectors, Equivalent_Widgets);
         use all type Widget_Edit.Kind_T;
         Edits : constant Widget_Edit.List :=
           Widget_Edit.Minimal_Sequence (Connection.Old_Widgets, Connection.Window.New_Widgets);

         package ID_Vectors is new Containers.Vectors (Positive, Natural);
         IDs_To_Delete : ID_Vectors.Vector;

         Updates    : JSON.JSON_Array;
         Update_Obj : JSON.JSON_Value;
      begin
         if Previous_View /= Connection.Window.Current_View then
            Update_Obj := JSON.Create_Object;
            Update_Obj.Set_Field ("type", "navigation");
            Update_Obj.Set_Field ("index", Integer (Views'Pos (Connection.Window.Current_View)));
            JSON.Append (Updates, Update_Obj);
         end if;

         if Edits'Length = 0 then
            for I in Connection.Old_Widgets.First_Index .. Connection.Old_Widgets.Last_Index loop
               Connection.Window.New_Widgets (I).ID := Connection.Old_Widgets (I).ID;
            end loop;
         else
            for Edit of Edits loop
               case Edit.Kind is
                  when Delete =>
                     Connection.Window.Widgets_By_ID.Exclude (Edit.Item.ID);
                     IDs_To_Delete.Append (Edit.Item.ID);

                  when Create =>
                     Edit.Item.ID := Connection.Current_Widget_ID;
                     Connection.Current_Widget_ID := @ + 1;
                     Connection.Window.Widgets_By_ID.Insert (Edit.Item.ID, Edit.Item);
                     Update_Obj := JSON.Create_Object;
                     Update_Obj.Set_Field ("type", "create");
                     Update_Obj.Set_Field ("id", Edit.Item.ID);
                     Update_Obj.Set_Field ("kind", Characters.Handling.To_Lower (Edit.Item.Kind'Image));

                     if Edit.Item.Kind in Labeled_Widget then
                        Update_Obj.Set_Field ("text", Format_Label (Edit.Item.Label));
                     end if;

                     Update_Obj.Set_Field ("parent_id", (if Edit.Item.Parent = null then 4 else Edit.Item.Parent.ID));
                     Update_Obj.Set_Field
                       ("preceding_id", (if Edit.Item.Previous = null then 0 else Edit.Item.Previous.ID));
                     Append_JSON (Update_Obj, Edit.Item);
                     JSON.Append (Updates, Update_Obj);

                  when Update | Keep =>
                     Edit.New_Item.ID := Edit.Old_Item.ID;
                     Connection.Window.Widgets_By_ID.Include (Edit.New_Item.ID, Edit.New_Item);
                     Update_Obj := JSON.Create_Object;
                     Update_Obj.Set_Field ("type", (if Edit.Kind = Update then "update" else "keep"));
                     Update_Obj.Set_Field ("id", Edit.New_Item.ID);
                     Update_Obj.Set_Field
                       ("parent_id", (if Edit.New_Item.Parent = null then 4 else Edit.New_Item.Parent.ID));
                     Update_Obj.Set_Field
                       ("preceding_id", (if Edit.New_Item.Previous = null then 0 else Edit.New_Item.Previous.ID));

                     if Edit.Kind = Update then
                        Update_Obj.Set_Field ("kind", Characters.Handling.To_Lower (Edit.New_Item.Kind'Image));

                        if Edit.New_Item.Kind in Labeled_Widget then
                           Update_Obj.Set_Field ("text", Format_Label (Edit.New_Item.Label));
                        end if;

                        Append_JSON (Update_Obj, Edit.New_Item);
                     end if;

                     JSON.Append (Updates, Update_Obj);
               end case;
            end loop;
         end if;

         if not IDs_To_Delete.Is_Empty then
            declare
               IDs : JSON.JSON_Array;
            begin
               for ID of IDs_To_Delete loop
                  JSON.Append (IDs, JSON.Create (ID));
               end loop;

               Update_Obj := JSON.Create_Object;
               Update_Obj.Set_Field ("type", "delete");
               Update_Obj.Set_Field ("ids", IDs);
               JSON.Append (Updates, Update_Obj);
            end;
         end if;

         Connection.Old_Widgets.Move (Connection.Window.New_Widgets);

         if Event_Received_Copy /= 0 then
            Update_Obj := JSON.Create_Object;
            Update_Obj.Set_Field ("type", "ack");
            Update_Obj.Set_Field ("id", Event_Received_Copy);
            JSON.Append (Updates, Update_Obj);
         end if;

         if JSON.Length (Updates) > 0 then
            Connection.WebSocket_Send (JSON.Create (Updates).Write);
         end if;
      end;
   end Redraw_Window_And_Send_Updates;

   overriding
   procedure WebSocket_Received (Connection : in out HTTP_Connection; Data_Array : Streams.Stream_Element_Array) is
      Data : constant Bytes := To_Bytes (Data_Array);
   begin
      pragma Assert (Data'Length > 0);

      case Connection.State is
         when Init =>
            raise Program_Error with "unreachable";

         when ID_Exchange =>
            declare
               Create_Client : Boolean := False;
               Path_Index    : Natural_64 := 17;
            begin
               pragma Assert (Hexadecimal (Data (0 .. 15)) = "F465D8E5127044C5876658817829CCC8");

               if Boolean'Val (Data (16)) then
                  Path_Index := @ + 16;
                  Connection.Client_ID := Data (17 .. 32);
                  Create_Client := not Client_Datas_By_ID.Contains (Connection.Client_ID);
               else
                  Create_Client := True;
                  Put_Random_Bytes (Connection.Client_ID);
                  Connection.WebSocket_Send (Connection.Client_ID);
               end if;

               if Create_Client then
                  declare
                     Default_Client : Client_Data;
                  begin
                     Client_Datas_By_ID.Insert (Connection.Client_ID, Default_Client);
                  end;
               end if;

               Connection.Client_Data_Access := Client_Datas_By_ID.Reference (Connection.Client_ID).Element;

               for View in Views loop
                  if To_String (Data (Path_Index .. Data'Last)) = Connection.Window.View_Paths (View) then
                     Connection.Window.Current_View := View;
                     exit;
                  end if;
               end loop;
            end;

            if Connections_By_Client_ID.Contains (Connection.Client_ID) then
               Connections_By_Client_ID (Connection.Client_ID).Insert (Connection.ID, Connection.Self);
            else
               declare
                  Map : Connections_By_ID_Maps.Map;
               begin
                  Map.Insert (Connection.ID, Connection.Self);
                  Connections_By_Client_ID.Insert (Connection.Client_ID, Map);
               end;
            end if;

            Connection.State := Ready;

         when Ready =>
            pragma Assert (Data = [24]);
            Redraw_Window_And_Send_Updates (Connection);
            Connection.State := Event_Loop;

         when Event_Loop =>
            declare
               Event_Obj : constant JSON.JSON_Value := JSON.Read (To_String (Data)).Value;
            begin
               Connection.Window.Event_ID := Event_Obj.Get ("id");
               Connection.Window.Event_Data := Event_Obj.Get ("data");
            end;

            Redraw_Window_And_Send_Updates (Connection);

            -- Redraw the client's other windows
            for Same_Client_Connection of Connections_By_Client_ID (Connection.Client_ID) loop
               if Same_Client_Connection /= Connection.Self then
                  Redraw_Window_And_Send_Updates (Same_Client_Connection.all);
               end if;
            end loop;
      end case;
   exception
      when E : others =>
         Text_IO.Put_Line (Exceptions.Exception_Information (E));
         raise;
   end WebSocket_Received;

   procedure Run is
      Factory :
        aliased Factory_Type (Request_Length => 4096, Input_Size => 4096, Output_Size => 1e6, Max_Connections => 100);
      Server  : Sockets.Server.Connections_Server (Factory'Access, Port => Sockets.Port_Type (Port))
      with Unreferenced;
   begin
      if Debug then
         Factory.Trace_On (Sockets.Server.Trace_Decoded, Sockets.Server.Trace_Decoded);
      end if;

      Text_IO.Put_Line ("HTTP server started");
      delay Duration'Last;
   end Run;
begin
   if Initialized_View_Names = View_Labels'[others => <>] then
      for View in Views loop
         declare
            Result : String := Characters.Handling.To_Lower (View'Image);
            Index  : Natural := 0;
         begin
            loop
               Result (Index + 1) := Characters.Handling.To_Upper (Result (Index + 1));
               Index := Strings.Fixed.Index (Result, "_", From => Index + 1);
               exit when Index = 0;
            end loop;
            Initialized_View_Names (View) := To_Unbounded_String (Result);
         end;
      end loop;
   end if;

   if Initialized_View_Paths = View_Labels'[others => <>] then
      for View in Views loop
         if View /= Views'First then
            declare
               Result : String := Characters.Handling.To_Lower (View'Image);
               Index  : Natural := Strings.Fixed.Index (Result, "_");
            begin
               while Index /= 0 loop
                  Result (Index) := '/';
                  Index := Strings.Fixed.Index (Result, "_", From => Index + 1);
               end loop;
               Initialized_View_Paths (View) := To_Unbounded_String (Result);
            end;
         end if;
      end loop;
   end if;

   declare
      function Hash (Item : Unbounded_String) return Containers.Hash_Type
      is (Strings.Hash (To_String (Item)));
      package Unbounded_String_Set is new Containers.Hashed_Sets (Unbounded_String, Hash, "=");
      Path_Set : Unbounded_String_Set.Set := Unbounded_String_Set.Empty (Initialized_View_Paths'Length);
   begin
      for Path of Initialized_View_Paths loop
         Path_Set.Insert (Path); -- Make sure the paths are unique
      end loop;
   end;

   declare
      List : JSON.JSON_Array;
      Obj  : JSON.JSON_Value;
   begin
      for View in Views loop
         Obj := JSON.Create_Object;
         Obj.Set_Field ("name", Initialized_View_Names (View));
         Obj.Set_Field ("path", Initialized_View_Paths (View));
         JSON.Append (List, Obj);
      end loop;
      Views_JSON_Object := JSON.Create (List).Write;
   end;
end Libre_Frame.UI.Server;
