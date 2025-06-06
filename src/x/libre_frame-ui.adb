with Ada.Calendar.Conversions;
with Interfaces.C;

package body Libre_Frame.UI is
   use Interfaces;

   function "&" (Left, Right : Widget_Type) return Widget_List
   is [Left, Right];

   function "&" (Left : Widget_List; Right : Widget_Type) return Widget_List is
   begin
      return Result : Widget_List (1 .. Left'Length + 1) do
         Result (Left'Range) := Left;
         Result (Result'Last) := Right;
      end return;
   end "&";

   function Equivalent_Widgets (Left, Right : not null Widget_Access) return Boolean is
      use type Calendar.Time, String_Vectors.Vector;
   begin
      if Left = Right then
         return True;
      elsif Left.Kind /= Right.Kind or else (Left.Kind in Labeled_Widget and Left.Label /= Right.Label) then
         return False;
      end if;

      if Left.Kind in Interactive_Widget and (Left.Enabled /= Right.Enabled) then
         return False;
      end if;

      case Left.Kind is
         when Text =>
            return True;

         when Bar =>
            return Left.Bar_Value = Right.Bar_Value;

         when Button =>
            return Left.Size = Right.Size;

         when Checkbox =>
            return Left.Checkbox_Value = Right.Checkbox_Value;

         when Integer_Field =>
            return Left.Min = Right.Min and Left.Max = Right.Max and Left.Integer_Value = Right.Integer_Value;

         when Text_Field | Password_Field =>
            return Left.Text_Value = Right.Text_Value;

         when Date_Field =>
            return Left.Date_Min = Right.Date_Min and Left.Date_Value = Right.Date_Value;

         when Option_Field =>
            return Left.Index = Right.Index and Left.Labels = Right.Labels;

         when Breadcrumb =>
            return Left.Index = Right.Index;

         when View =>
            return Left.Index = Right.Index and Left.Description = Right.Description;

         when Box =>
            return
              Left.Axis = Right.Axis
              and Left.Content_Alignment = Right.Content_Alignment
              and Left.Fill = Right.Fill
              and Left.Bordered = Right.Bordered
              and Left.Spacing = Right.Spacing;

         when Separator =>
            return Left.Visible = Right.Visible and Left.Separator_Size = Right.Separator_Size;

         when Rich =>
            return Left.Rich = Right.Rich;
      end case;
   end Equivalent_Widgets;

   function Get_Path (Widget : Widget_Access) return String is
      Widgets_Count  : Natural := 0;
      Path_Length    : Natural := 0;
      Current_Widget : access Widget_Type := Widget;
   begin
      loop
         Widgets_Count := Widgets_Count + 1;
         Path_Length := Path_Length + Length (Current_Widget.Label);
         Current_Widget := Current_Widget.Parent;
         exit when Current_Widget = null;
      end loop;
      Current_Widget := Widget;
      declare
         Path         : String (1 .. Path_Length + Widgets_Count - 1);
         I            : Positive := Path'Last;
         Label_Length : Natural;
      begin
         loop
            Label_Length := Length (Current_Widget.Label);
            Path (I - Label_Length + 1 .. I) := To_String (Current_Widget.Label);
            Current_Widget := Current_Widget.Parent;
            exit when Current_Widget = null;
            I := I - Label_Length;
            Path (I) := '/';
            I := I - 1;
         end loop;
         return Path;
      end;
   end Get_Path;

   function Same_Path (Left, Right : Widget_Access) return Boolean is
      Left_Access  : access Widget_Type := Left;
      Right_Access : access Widget_Type := Right;
   begin
      loop
         if Left_Access.Label /= Right_Access.Label then
            -- TODO: Also check the widget kind hierarchy
            return False;
         end if;

         Left_Access := Left_Access.Parent;
         Right_Access := Right_Access.Parent;

         if Left_Access = null or Right_Access = null then
            -- returns True if both are null
            return Left_Access = Right_Access;
         end if;
      end loop;
   end Same_Path;

   function New_Line return Rich_Text is
      Result : Rich_Text;
   begin
      Result.Segments.Append (Rich_Text_Segment'(New_Line => True, others => <>));
      return Result;
   end New_Line;

   function Highlighted (Text : String) return Rich_Text is
      Result : Rich_Text;
   begin
      Result.Segments.Append
        (Rich_Text_Segment'(Text => To_Unbounded_String (Text), Highlighted => True, others => <>));
      return Result;
   end Highlighted;

   function Highlighted (Item : Rich_Text) return Rich_Text is
      Copy : Rich_Text := Item;
   begin
      for Element of Copy.Segments loop
         Element.Highlighted := True;
      end loop;
      return Copy;
   end Highlighted;

   function Date (Date : Calendar.Time) return Rich_Text is
      Result : Rich_Text;
   begin
      Result.Segments.Append (Rich_Text_Segment'(Date => Date, others => <>));
      return Result;
   end Date;

   function "&" (Left, Right : Rich_Text) return Rich_Text
   is (Segments => Left.Segments & Right.Segments);

   function "&" (Left : String; Right : Rich_Text) return Rich_Text
   is (Segments => Rich_Text_Segment'(Text => To_Unbounded_String (Left), others => <>) & Right.Segments);

   function "&" (Left : Rich_Text; Right : String) return Rich_Text
   is (Segments => Left.Segments & Rich_Text_Segment'(Text => To_Unbounded_String (Right), others => <>));

   procedure Append (Target : in out Rich_Text; Item : String) is
   begin
      Target.Segments.Append (Rich_Text_Segment'(Text => To_Unbounded_String (Item), others => <>));
   end Append;

   procedure Append (Target : in out Rich_Text; Item : Rich_Text) is
   begin
      Target.Segments.Append_Vector (Item.Segments);
   end Append;

   function Text (Label : String; Fixed_Width : Boolean := False) return Widget_Type is
   begin
      if not Fixed_Width then
         return Widget_Type'(Kind => Text, Label => To_Unbounded_String (Label), others => <>);
      end if;

      declare
         Segments : Rich_Text_Vector;
      begin
         Segments.Append (Rich_Text_Segment'(Text => To_Unbounded_String (Label), others => <>));
         return Widget_Type'(Kind => Rich, Rich => (Segments => Segments, Fixed_Width => True), others => <>);
      end;
   end Text;

   function Text (Value : Rich_Text; Fixed_Width : Boolean := False) return Widget_Type
   is (Widget_Type'(Kind => Rich, Rich => (Segments => Value.Segments, Fixed_Width => Fixed_Width), others => <>));

   function Bar (Label : String; Proportion : Proportion_Type) return Widget_Type
   is (Widget_Type'
         (Kind      => Bar,
          Label     => To_Unbounded_String (Label),
          Bar_Value => (if Proportion.Valid then Proportion.Value else -1.0),
          others    => <>));

   function Checkbox (Label : String; Value : not null access Boolean) return Widget_Type
   is (Widget_Type'(Kind => Checkbox, Label => To_Unbounded_String (Label), Checkbox_Access => Value, others => <>));

   function Integer_Field (Label : String; Value : not null access Integer; Min, Max : Integer) return Widget_Type
   is (Widget_Type'
         (Kind           => Integer_Field,
          Label          => To_Unbounded_String (Label),
          Min            => Min,
          Max            => Max,
          Integer_Value  => Value.all,
          Integer_Access => Value,
          others         => <>));

   function Text_Field (Label : String; Value : not null access Unbounded_String) return Widget_Type
   is (Widget_Type'
         (Kind        => Text_Field,
          Label       => To_Unbounded_String (Label),
          Text_Value  => Value.all,
          Text_Access => Value,
          others      => <>));

   function Password_Field (Label : String; Value : not null access Unbounded_String) return Widget_Type
   is (Widget_Type'
         (Kind        => Password_Field,
          Label       => To_Unbounded_String (Label),
          Text_Value  => Value.all,
          Text_Access => Value,
          others      => <>));

   function Date_Field
     (Label   : String;
      Value   : not null access Calendar.Time;
      From    : Calendar.Time := Unset_Time;
      Enabled : Boolean := True) return Widget_Type
   is (Widget_Type'
         (Kind        => Date_Field,
          Label       => To_Unbounded_String (Label),
          Enabled     => Enabled,
          Date_Min    => From,
          Date_Value  => Value.all,
          Date_Access => Value,
          others      => <>));

   function Separator (Size : Size_Type := Default; Visible : Boolean := False) return Widget_Type
   is (Widget_Type'(Kind => Separator, Visible => Visible, Separator_Size => Size, others => <>));

   function Make_Widget (Target : in out Container; Kind : Widget_Kind; Label : String) return Widget_Access is
   begin
      return Widget : constant Widget_Access := new Widget_Type (Kind) do
         Widget.Label := To_Unbounded_String (Label);
         Widget.Parent := Target.Widget;
         Widget.Previous := Target.Current;
         Target.Current := Widget;
         Target.Window.New_Widgets.Append (Widget);
      end return;
   end Make_Widget;

   procedure Make_Widget (Target : in out Container; Kind : Widget_Kind; Label : String) is
      Ignored : constant Widget_Access := Make_Widget (Target, Kind, Label)
      with Unreferenced;
   begin
      null;
   end Make_Widget;

   procedure Append (Target : in out Container; Widget : Widget_Type) is
      Item : constant Widget_Access := new Widget_Type'(Widget);
   begin
      Item.Parent := Target.Widget;
      Item.Previous := Target.Current;
      Target.Current := Item;
      Target.Window.New_Widgets.Append (Item);

      if Target.Window.Event_ID > 0 then
         declare
            Existing_Widget : constant Widget_Access := Target.Window.Widgets_By_ID (Target.Window.Event_ID);
         begin
            if Same_Path (Item, Existing_Widget) then
               pragma Assert (Item.Kind = Existing_Widget.Kind and Item.Kind in Interactive_Widget);
               case Item.Kind is
                  when Checkbox =>
                     Item.Checkbox_Value := Item.Checkbox_Access.all;
                     Item.Checkbox_Access.all := not Item.Checkbox_Access.all;
                     Existing_Widget.Checkbox_Value := Item.Checkbox_Access.all;

                  when Integer_Field =>
                     Item.Integer_Value := Target.Window.Event_Data.Get;

                     if Item.Integer_Value < Item.Min then
                        Item.Integer_Value := Item.Min;
                     elsif Item.Integer_Value > Item.Max then
                        Item.Integer_Value := Item.Max;
                     end if;

                     Item.Integer_Access.all := Item.Integer_Value;
                     Existing_Widget.Integer_Value := Item.Integer_Value;
                     Existing_Widget.Min := Item.Min;
                     Existing_Widget.Max := Item.Max;

                  when Text_Field | Password_Field =>
                     Item.Text_Value := Target.Window.Event_Data.Get;
                     Item.Text_Access.all := Item.Text_Value;
                     Existing_Widget.Text_Value := Item.Text_Value;

                  when Date_Field =>
                     Item.Date_Value :=
                       Calendar.Conversions.To_Ada_Time (C.Long (Long_Integer'(Target.Window.Event_Data.Get)));
                     Item.Date_Access.all := Item.Date_Value;
                     Existing_Widget.Enabled := Item.Enabled;
                     Existing_Widget.Date_Min := Item.Date_Min;
                     Existing_Widget.Date_Value := Item.Date_Value;

                  when others =>
                     raise Program_Error with "unreachable";
               end case;
            end if;
         end;
      end if;
   end Append;

   procedure Append (Target : in out Container; Widgets : Widget_List) is
      use type Containers.Count_Type;
   begin
      Target.Window.New_Widgets.Reserve_Capacity (Target.Window.New_Widgets.Capacity + Widgets'Length);
      for Widget of Widgets loop
         Target.Append (Widget);
      end loop;
   end Append;

   procedure Text (Target : in out Container; Label : String; Fixed_Width : Boolean := False) is
   begin
      if not Fixed_Width then
         Make_Widget (Target, Text, Label);
      else
         declare
            Widget : constant Widget_Access := Make_Widget (Target, Rich, "");
         begin
            Widget.Rich.Segments.Append (Rich_Text_Segment'(Text => To_Unbounded_String (Label), others => <>));
            Widget.Rich.Fixed_Width := True;
         end;
      end if;
   end Text;

   procedure Text (Target : in out Container; Value : Rich_Text; Fixed_Width : Boolean := False) is
      Widget : constant Widget_Access := Make_Widget (Target, Rich, "");
   begin
      Widget.Rich.Segments := Value.Segments;
      Widget.Rich.Fixed_Width := Fixed_Width;
   end Text;

   procedure Bar (Target : in out Container; Label : String; Proportion : Proportion_Type) is
      Widget : constant Widget_Access := Make_Widget (Target, Bar, Label);
   begin
      Widget.Bar_Value := (if Proportion.Valid then Proportion.Value else -1.0);
   end Bar;

   procedure Separator (Target : in out Container; Size : Size_Type := Default; Visible : Boolean := False) is
      Widget : constant Widget_Access := Make_Widget (Target, Separator, "");
   begin
      Widget.Visible := Visible;
      Widget.Separator_Size := Size;
   end Separator;

   function Button
     (Target : in out Container; Label : String; Size : Visible_Size := Default; Enabled : Boolean := True)
      return Boolean
   is
      Widget : constant Widget_Access := Make_Widget (Target, Button, Label);
   begin
      Widget.Enabled := Enabled;
      Widget.Size := Size;
      return
        Target.Window.Event_ID > 0 and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID));
   end Button;

   function Checkbox (Target : in out Container; Label : String; Value : in out Boolean) return Boolean is
      Widget : constant Widget_Access := Make_Widget (Target, Checkbox, Label);
   begin
      Widget.Checkbox_Value := Value;

      return Interacted_With : Boolean := False do
         if Target.Window.Event_ID > 0
           and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID))
         then
            Interacted_With := True;
            Value := not Value;
            Target.Window.Widgets_By_ID (Target.Window.Event_ID).Checkbox_Value := Value;
         end if;
      end return;
   end Checkbox;

   procedure Checkbox (Target : in out Container; Label : String; Value : in out Boolean) is
      Ignored : constant Boolean := Target.Checkbox (Label, Value)
      with Unreferenced;
   begin
      null;
   end Checkbox;

   function Integer_Field
     (Target : in out Container; Label : String; Value : in out Integer; Min, Max : Integer) return Boolean
   is
      Widget : constant Widget_Access := Make_Widget (Target, Integer_Field, Label);
   begin
      Widget.Min := Min;
      Widget.Max := Max;

      if Value < Min then
         Value := Min;
      elsif Value > Max then
         Value := Max;
      end if;

      Widget.Integer_Value := Value;

      return Interacted_With : Boolean := False do
         if Target.Window.Event_ID > 0
           and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID))
         then
            declare
               Existing_Widget : constant Widget_Access := Target.Window.Widgets_By_ID (Target.Window.Event_ID);
            begin
               Interacted_With := True;
               Value := Target.Window.Event_Data.Get;
               Existing_Widget.Min := Min;
               Existing_Widget.Max := Max;
               Existing_Widget.Integer_Value := Value;
            end;
         end if;
      end return;
   end Integer_Field;

   procedure Integer_Field (Target : in out Container; Label : String; Value : in out Integer; Min, Max : Integer) is
      Ignored : constant Boolean := Target.Integer_Field (Label, Value, Min, Max)
      with Unreferenced;
   begin
      null;
   end Integer_Field;

   function Text_Field (Target : in out Container; Label : String; Value : in out Unbounded_String) return Boolean is
      Widget : constant Widget_Access := Make_Widget (Target, Text_Field, Label);
   begin
      Widget.Text_Value := Value;

      return Interacted_With : Boolean := False do
         if Target.Window.Event_ID > 0
           and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID))
         then
            Interacted_With := True;
            Value := Target.Window.Event_Data.Get;
            Target.Window.Widgets_By_ID (Target.Window.Event_ID).Text_Value := Value;
         end if;
      end return;
   end Text_Field;

   procedure Text_Field (Target : in out Container; Label : String; Value : in out Unbounded_String) is
      Ignored : constant Boolean := Target.Text_Field (Label, Value)
      with Unreferenced;
   begin
      null;
   end Text_Field;

   function Password_Field (Target : in out Container; Label : String; Value : in out Unbounded_String) return Boolean
   is
      Widget : constant Widget_Access := Make_Widget (Target, Password_Field, Label);
   begin
      Widget.Text_Value := Value;

      return Interacted_With : Boolean := False do
         if Target.Window.Event_ID > 0
           and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID))
         then
            Interacted_With := True;
            Value := Target.Window.Event_Data.Get;
            Target.Window.Widgets_By_ID (Target.Window.Event_ID).Text_Value := Value;
         end if;
      end return;
   end Password_Field;

   procedure Password_Field (Target : in out Container; Label : String; Value : in out Unbounded_String) is
      Ignored : constant Boolean := Target.Password_Field (Label, Value)
      with Unreferenced;
   begin
      null;
   end Password_Field;

   function Date_Field
     (Target  : in out Container;
      Label   : String;
      Value   : in out Calendar.Time;
      From    : Calendar.Time := Unset_Time;
      Enabled : Boolean := True) return Boolean
   is
      use type Calendar.Time;
      Widget : constant Widget_Access := Make_Widget (Target, Date_Field, Label);
   begin
      if Value /= Unset_Time and Value < From then
         Value := From;
      end if;

      Widget.Enabled := Enabled;
      Widget.Date_Min := From;
      Widget.Date_Value := Value;

      return Interacted_With : Boolean := False do
         if Target.Window.Event_ID > 0
           and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID))
         then
            Interacted_With := True;
            Value := Calendar.Conversions.To_Ada_Time (C.Long (Long_Integer'(Target.Window.Event_Data.Get)));
            Target.Window.Widgets_By_ID (Target.Window.Event_ID).Date_Value := Value;
         end if;
      end return;
   end Date_Field;

   procedure Date_Field
     (Target  : in out Container;
      Label   : String;
      Value   : in out Calendar.Time;
      From    : Calendar.Time := Unset_Time;
      Enabled : Boolean := True)
   is
      Ignored : constant Boolean := Target.Date_Field (Label, Value, From, Enabled)
      with Unreferenced;
   begin
      null;
   end Date_Field;

   function Make_Parent (Target : in out Container; Widget : not null Widget_Access) return Container is
   begin
      return Result : constant Container := (Window => Target.Window, Widget => Widget, Current => null) do
         Widget.Parent := Target.Widget;
         Widget.Previous := Target.Current;
         Target.Window.New_Widgets.Append (Widget);
         Target.Current := Widget;
      end return;
   end Make_Parent;

   package body Choice is
      Saved_Labels : String_Vectors.Vector;

      function Option_Field
        (Target : in out Container; Label : String; Value : in out Options; Custom_Labels : Labels := [others => <>])
         return Boolean
      is
         Widget : constant Widget_Access := Make_Widget (Target, Option_Field, Label);
      begin
         Widget.Index := Options'Pos (Value);

         if Custom_Labels = Labels'[others => <>] then
            Widget.Labels := Saved_Labels;
         else
            Saved_Labels.Clear;
            for Option in Options loop
               Saved_Labels.Append (Custom_Labels (Option));
            end loop;
            Widget.Labels.Move (Saved_Labels);
         end if;

         return Interacted_With : Boolean := False do
            if Target.Window.Event_ID > 0
              and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID))
            then
               declare
                  Index : constant Integer := Target.Window.Event_Data.Get;
               begin
                  Interacted_With := True;
                  Value := Options'Val (Index);
                  Target.Window.Widgets_By_ID (Target.Window.Event_ID).Index := Index;
               end;
            end if;
         end return;
      end Option_Field;

      procedure Option_Field
        (Target : in out Container; Label : String; Value : in out Options; Custom_Labels : Labels := [others => <>])
      is
         Ignored : constant Boolean := Option_Field (Target, Label, Value, Custom_Labels)
         with Unreferenced;
      begin
         null;
      end Option_Field;
   begin
      for Option in Options loop
         Saved_Labels.Append (To_Unbounded_String (Option'Image));
      end loop;
   end Choice;

   function Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False) return Container
   is (Make_Parent
         (Target,
          new Widget_Type'
            (Kind              => Box,
             Label             => To_Unbounded_String (Label),
             Axis              => Axis,
             Content_Alignment => Content_Alignment,
             Fill              => Fill,
             Bordered          => Bordered,
             Spacing           => Spacing,
             others            => <>)));

   function Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widget            : Widget_Type) return Container
   is
      Box : Container :=
        Target.Box (Label, Axis, Content_Alignment, Fill => Fill, Bordered => Bordered, Spacing => Spacing);
   begin
      Box.Append (Widget);
      return Box;
   end Box;

   function Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widgets           : Widget_List) return Container
   is
      Box : Container :=
        Target.Box (Label, Axis, Content_Alignment, Fill => Fill, Bordered => Bordered, Spacing => Spacing);
   begin
      Box.Append (Widgets);
      return Box;
   end Box;

   procedure Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widget            : Widget_Type)
   is
      Box : Container :=
        Target.Box (Label, Axis, Content_Alignment, Fill => Fill, Bordered => Bordered, Spacing => Spacing);
   begin
      Box.Append (Widget);
   end Box;

   procedure Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widgets           : Widget_List)
   is
      Box : Container :=
        Target.Box (Label, Axis, Content_Alignment, Fill => Fill, Bordered => Bordered, Spacing => Spacing);
   begin
      Box.Append (Widgets);
   end Box;

   function Breadcrumb (Target : in out Container) return Views is
      Widget : constant Widget_Access := Make_Widget (Target, Breadcrumb, "SjeQ1S0QrQAT_breadcrumb");
   begin
      Widget.Index := Views'Pos (Target.Window.Current_View);

      if Target.Window.Event_ID > 0 and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID))
      then
         declare
            Path : constant String := Target.Window.Event_Data.Get;
         begin
            for View in Views loop
               if View /= Target.Window.Current_View and then Path = Target.Window.View_Paths (View) then
                  return View;
               end if;
            end loop;
         end;
      end if;
      return Target.Window.Current_View;
   end Breadcrumb;

   function View (Target : in out Container; View : Views; Description : String := "") return Boolean is
      Widget : constant Widget_Access :=
        Make_Widget (Target, UI.View, "SjeQ1S0QrQAT_" & To_String (Target.Window.View_Names (View)) & Description);
   begin
      Widget.Index := Views'Pos (View);
      Widget.Description := To_Unbounded_String (Description);
      return
        Target.Window.Event_ID > 0 and then Same_Path (Widget, Target.Window.Widgets_By_ID (Target.Window.Event_ID));
   end View;

   function Group
     (Target : in out Container; Label : String; Content_Alignment : Alignment_Type := Top_Left) return Container
   is
      Column    : UI.Container := Target.Box ("Column", Vertical);
      Label_Box : UI.Container := Column.Box ("Label_Box", Horizontal);
   begin
      Label_Box.Separator;
      Label_Box.Text (Label);
      return Column.Box ("group", Vertical, Content_Alignment, Bordered => True, Spacing => True);
   end Group;

end Libre_Frame.UI;
