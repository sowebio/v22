with Ada.Calendar;
with Ada.Strings.Unbounded;
with Libre_Frame.UI.Server;

package body Widgets_Pkg is
   use Ada.Strings.Unbounded;

   type Views is (Main);

   package UI is new Libre_Frame.UI (Views => Views);

   type Null_Record is null record;

   type Options is (First_Option, The_Second_One, Another);

   package Choice is new UI.Choice (Options);

   type Window_Data is record
      Text_Value : Unbounded_String := To_Unbounded_String ("Hello, World!");

      Checkbox_Label   : Unbounded_String := To_Unbounded_String ("Label");
      Checkbox_Value   : Boolean := False;
      Checkbox_Enabled : Boolean := True;

      Integer_Field_Label   : Unbounded_String := To_Unbounded_String ("Label:");
      Integer_Field_Value   : Integer := 6;
      Integer_Field_Min     : Integer := 0;
      Integer_Field_Max     : Integer := 10;
      Integer_Field_Enabled : Boolean := True;

      Text_Field_Label   : Unbounded_String := To_Unbounded_String ("Label:");
      Text_Field_Value   : Unbounded_String := To_Unbounded_String ("Hello, World!");
      Text_Field_Enabled : Boolean := True;

      Password_Field_Label   : Unbounded_String := To_Unbounded_String ("Label:");
      Password_Field_Value   : Unbounded_String := To_Unbounded_String ("SECRET");
      Password_Field_Enabled : Boolean := True;

      Date_Field_Label   : Unbounded_String := To_Unbounded_String ("Label:");
      Date_Field_Value   : Ada.Calendar.Time := Ada.Calendar.Clock;
      Date_Field_From    : Ada.Calendar.Time := Ada.Calendar.Clock;
      Date_Field_Enabled : Boolean := True;

      Option_Field_Label          : Unbounded_String := To_Unbounded_String ("Label");
      Option_Field_Value          : Options := First_Option;
      Option_Field_Custom_Labels  : Boolean := False;
      Option_Field_Custom_Label_1 : Unbounded_String := Choice.Default_Labels (First_Option);
      Option_Field_Custom_Label_2 : Unbounded_String := Choice.Default_Labels (The_Second_One);
      Option_Field_Custom_Label_3 : Unbounded_String := Choice.Default_Labels (Another);
      Option_Field_Enabled        : Boolean := True;
   end record;

   function Redraw_Window
     (Current_View : Views; Frame : in out UI.Container; Client : in out Null_Record; Window : in out Window_Data)
      return Views
   is
      Row_Index : Natural := 0;

      Left, Right : UI.Container;

      procedure Create_New_Row is
         Row       : UI.Container := Frame.Box ("row" & Row_Index'Image, UI.Horizontal, Spacing => True);
         Left_Row  : UI.Container := Row.Box ("left", UI.Horizontal, Fill => True);
         Right_Row : UI.Container := Row.Box ("right", UI.Horizontal, Fill => True);
      begin
         Left := Left_Row.Box ("left", UI.Vertical);
         Right := Right_Row.Box ("right", UI.Vertical, UI.Middle_Left, Fill => True);
         Row_Index := @ + 1;
         Frame.Separator (Visible => True);
      end Create_New_Row;
   begin
      Create_New_Row;
      declare
         Group : UI.Container := Left.Group ("Text");
      begin
         Group.Text_Field ("Value:", Window.Text_Value);
         Right.Text (To_String (Window.Text_Value));
      end;

      Create_New_Row;
      declare
         Group : UI.Container := Left.Group ("Checkbox");
      begin
         Group.Text_Field ("Label:", Window.Checkbox_Label);
         Group.Checkbox ("Value", Window.Checkbox_Value);
         Group.Checkbox ("Enabled", Window.Checkbox_Enabled);
         Right.Checkbox (To_String (Window.Checkbox_Label), Window.Checkbox_Value, Window.Checkbox_Enabled);
      end;

      Create_New_Row;
      declare
         Group : UI.Container := Left.Group ("Integer_Field");
      begin
         Group.Text_Field ("Label:", Window.Integer_Field_Label);
         Group.Integer_Field
           ("Value:", Window.Integer_Field_Value, Window.Integer_Field_Min, Window.Integer_Field_Max);
         Group.Integer_Field ("Min:", Window.Integer_Field_Min, Min => -10_000, Max => Window.Integer_Field_Max);
         Group.Integer_Field ("Max:", Window.Integer_Field_Max, Min => Window.Integer_Field_Min, Max => 10_000);
         Group.Checkbox ("Enabled", Window.Integer_Field_Enabled);
         Right.Integer_Field
           (To_String (Window.Integer_Field_Label),
            Window.Integer_Field_Value,
            Window.Integer_Field_Min,
            Window.Integer_Field_Max,
            Window.Integer_Field_Enabled);
      end;

      Create_New_Row;
      declare
         Group : UI.Container := Left.Group ("Text_Field");
      begin
         Group.Text_Field ("Label:", Window.Text_Field_Label);
         Group.Text_Field ("Value:", Window.Text_Field_Value);
         Group.Checkbox ("Enabled", Window.Text_Field_Enabled);
         Right.Text_Field (To_String (Window.Text_Field_Label), Window.Text_Field_Value, Window.Text_Field_Enabled);
      end;

      Create_New_Row;
      declare
         Group : UI.Container := Left.Group ("Password_Field");
      begin
         Group.Text_Field ("Label:", Window.Password_Field_Label);
         Group.Text_Field ("Value:", Window.Password_Field_Value);
         Group.Checkbox ("Enabled", Window.Password_Field_Enabled);
         Right.Password_Field
           (To_String (Window.Password_Field_Label), Window.Password_Field_Value, Window.Password_Field_Enabled);
      end;

      Create_New_Row;
      declare
         Group : UI.Container := Left.Group ("Date_Field");
      begin
         Group.Text_Field ("Label:", Window.Date_Field_Label);
         Group.Date_Field ("Value:", Window.Date_Field_Value);
         Group.Date_Field ("From:", Window.Date_Field_From);
         Group.Checkbox ("Enabled", Window.Date_Field_Enabled);
         Right.Date_Field
           (To_String (Window.Date_Field_Label),
            Window.Date_Field_Value,
            Window.Date_Field_From,
            Window.Date_Field_Enabled);
      end;

      Create_New_Row;
      declare
         use type UI.Rich_Text;
         Group : UI.Container := Left.Group ("Option_Field");
      begin
         Group.Text
           (UI.Highlighted ("type")
            & " Options "
            & UI.Highlighted ("is")
            & " (First_Option, The_Second_One, Another);",
            Fixed_Width => True);
         Group.Separator (Visible => True);
         Group.Text_Field ("Label:", Window.Option_Field_Label);
         Choice.Option_Field
           (Target        => Group,
            Label         => "Value",
            Value         => Window.Option_Field_Value,
            Custom_Labels =>
              Choice.Labels'
                [To_Unbounded_String ("Options'(First_Option)"),
                 To_Unbounded_String ("Options'(The_Second_One)"),
                 To_Unbounded_String ("Options'(Another)")]);
         Group.Checkbox ("Enable custom labels", Window.Option_Field_Custom_Labels);
         Group.Text_Field ("Custom label 1:", Window.Option_Field_Custom_Label_1);
         Group.Text_Field ("Custom label 2:", Window.Option_Field_Custom_Label_2);
         Group.Text_Field ("Custom label 3:", Window.Option_Field_Custom_Label_3);
         Group.Checkbox ("Enabled", Window.Option_Field_Enabled);
         Choice.Option_Field
           (Target        => Right,
            Label         => To_String (Window.Option_Field_Label),
            Value         => Window.Option_Field_Value,
            Custom_Labels =>
              (if Window.Option_Field_Custom_Labels
               then
                 Choice.Labels'
                   [Window.Option_Field_Custom_Label_1,
                    Window.Option_Field_Custom_Label_2,
                    Window.Option_Field_Custom_Label_3]
               else [others => <>]),
            Enabled       => Window.Option_Field_Enabled);
      end;

      return Current_View;
   end Redraw_Window;

   package Server is new
     UI.Server (Client_Data => Null_Record, Window_Data => Window_Data, Redraw_Window => Redraw_Window);

   procedure Run renames Server.Run;

end Widgets_Pkg;
