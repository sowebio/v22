with Ada.Calendar;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;

private with Ada.Containers.Ordered_Maps;
private with Ada.Containers.Vectors;
private with Ada.Unchecked_Deallocation;
private with GNATCOLL.JSON;

generic
   type Views is (<>);

package Libre_Frame.UI
is
   use Ada, Ada.Strings.Unbounded;

   function Valid_Name (Source : String) return Boolean
   is (Strings.Fixed.Index_Non_Blank (Source) > 0);

   subtype Non_Blank_String is String with Dynamic_Predicate => Valid_Name (Non_Blank_String);

   type View_Labels is array (Views) of Unbounded_String;

   type Widget_Type is limited private;
   type Widget_List is array (Positive range <>) of Widget_Type;

   function "&" (Left, Right : Widget_Type) return Widget_List;
   function "&" (Left : Widget_List; Right : Widget_Type) return Widget_List;

   type Proportion_Type (Valid : Boolean) is record
      case Valid is
         when True =>
            Value : Float range 0.0 .. 1.0;

         when False =>
            null;
      end case;
   end record;

   type Axis_Type is (Horizontal, Vertical);

   type Alignment_Type is
     (Top_Left,
      Top_Center,
      Top_Right,
      Middle_Left,
      Middle_Center,
      Middle_Right,
      Bottom_Left,
      Bottom_Center,
      Bottom_Right);

   type Size_Type is (Zero, Default, Fill);
   subtype Visible_Size is Size_Type range Default .. Fill;

   type Rich_Text is private;

   function New_Line return Rich_Text;

   function Highlighted (Text : String) return Rich_Text;
   function Highlighted (Item : Rich_Text) return Rich_Text;

   function Date (Date : Calendar.Time) return Rich_Text; -- TODO: Add formatting options

   function "&" (Left, Right : Rich_Text) return Rich_Text;
   function "&" (Left : String; Right : Rich_Text) return Rich_Text;
   function "&" (Left : Rich_Text; Right : String) return Rich_Text;

   procedure Append (Target : in out Rich_Text; Item : String);
   procedure Append (Target : in out Rich_Text; Item : Rich_Text);

   Unset_Time : constant Calendar.Time := Time_First;

   function Text (Value : String; Fixed_Width : Boolean := False) return Widget_Type
   with Pre => Value /= "";

   function Text (Value : Rich_Text; Fixed_Width : Boolean := False) return Widget_Type;

   function Bar (Label : String; Proportion : Proportion_Type) return Widget_Type
   with Pre => Label /= "";

   function Checkbox (Label : String; Value : not null access Boolean; Enabled : Boolean := True) return Widget_Type
   with Pre => Label /= "";

   function Integer_Field
     (Label : String; Value : not null access Integer; Min, Max : Integer; Enabled : Boolean := True)
      return Widget_Type
   with Pre => Label /= "" and Min <= Max;

   function Text_Field
     (Label : String; Value : not null access Unbounded_String; Enabled : Boolean := True) return Widget_Type
   with Pre => Label /= "";

   function Password_Field
     (Label : String; Value : not null access Unbounded_String; Enabled : Boolean := True) return Widget_Type
   with Pre => Label /= "";

   function Date_Field
     (Label   : String;
      Value   : not null access Calendar.Time;
      From    : Calendar.Time := Unset_Time;
      Enabled : Boolean := True) return Widget_Type
   with Pre => Label /= "";

   function Separator (Size : Size_Type := Default; Visible : Boolean := False) return Widget_Type;

   type Container is tagged private;

   procedure Append (Target : in out Container; Widget : Widget_Type);
   procedure Append (Target : in out Container; Widgets : Widget_List);

   procedure Text (Target : in out Container; Value : String; Fixed_Width : Boolean := False)
   with Pre => Value /= "";

   procedure Text (Target : in out Container; Value : Rich_Text; Fixed_Width : Boolean := False);

   procedure Bar (Target : in out Container; Label : String; Proportion : Proportion_Type)
   with Pre => Label /= "";

   procedure Separator (Target : in out Container; Size : Size_Type := Default; Visible : Boolean := False);

   function Button
     (Target : in out Container; Label : String; Size : Visible_Size := Default; Enabled : Boolean := True)
      return Boolean
   with Pre => Label /= "";

   function Checkbox
     (Target : in out Container; Label : String; Value : in out Boolean; Enabled : Boolean := True) return Boolean
   with Pre => Label /= "";

   procedure Checkbox (Target : in out Container; Label : String; Value : in out Boolean; Enabled : Boolean := True)
   with Pre => Label /= "";

   function Integer_Field
     (Target : in out Container; Label : String; Value : in out Integer; Min, Max : Integer; Enabled : Boolean := True)
      return Boolean
   with Pre => Label /= "" and Min <= Max;

   procedure Integer_Field
     (Target : in out Container; Label : String; Value : in out Integer; Min, Max : Integer; Enabled : Boolean := True)
   with Pre => Label /= "" and Min <= Max;

   function Text_Field
     (Target : in out Container; Label : String; Value : in out Unbounded_String; Enabled : Boolean := True)
      return Boolean
   with Pre => Label /= "";

   procedure Text_Field
     (Target : in out Container; Label : String; Value : in out Unbounded_String; Enabled : Boolean := True)
   with Pre => Label /= "";

   function Password_Field
     (Target : in out Container; Label : String; Value : in out Unbounded_String; Enabled : Boolean := True)
      return Boolean
   with Pre => Label /= "";

   procedure Password_Field
     (Target : in out Container; Label : String; Value : in out Unbounded_String; Enabled : Boolean := True)
   with Pre => Label /= "";

   function Date_Field
     (Target  : in out Container;
      Label   : String;
      Value   : in out Calendar.Time;
      From    : Calendar.Time := Unset_Time;
      Enabled : Boolean := True) return Boolean
   with Pre => Label /= "";

   procedure Date_Field
     (Target  : in out Container;
      Label   : String;
      Value   : in out Calendar.Time;
      From    : Calendar.Time := Unset_Time;
      Enabled : Boolean := True)
   with Pre => Label /= "";

   generic
      type Options is (<>);

   package Choice
   is
      type Labels is array (Options) of Unbounded_String;

      function Option_Field
        (Target        : in out Container;
         Label         : String;
         Value         : in out Options;
         Custom_Labels : Labels := [others => <>];
         Enabled       : Boolean := True) return Boolean
      with Pre => Label /= "";

      procedure Option_Field
        (Target        : in out Container;
         Label         : String;
         Value         : in out Options;
         Custom_Labels : Labels := [others => <>];
         Enabled       : Boolean := True)
      with Pre => Label /= "";
   end Choice;

   function Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False) return Container
   with Pre => Label /= "";

   function Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widget            : Widget_Type) return Container
   with Pre => Label /= "";

   function Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widgets           : Widget_List) return Container
   with Pre => Label /= "";

   procedure Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widget            : Widget_Type);

   procedure Box
     (Target            : in out Container;
      Label             : String;
      Axis              : Axis_Type;
      Content_Alignment : Alignment_Type := Top_Left;
      Fill              : Boolean := False;
      Bordered          : Boolean := False;
      Spacing           : Boolean := False;
      Widgets           : Widget_List);

   function Breadcrumb (Target : in out Container) return Views;

   function View (Target : in out Container; View : Views; Description : String := "") return Boolean;

   -- Helpers
   function Group
     (Target : in out Container; Label : String; Content_Alignment : Alignment_Type := Top_Left) return Container
   with Pre => Label /= "";

private
   use GNATCOLL;

   Debug : constant Boolean := False;

   type Widget_Kind is
     (Text,
      Bar,
      Button,
      Checkbox,
      Integer_Field,
      Text_Field,
      Password_Field,
      Date_Field,
      Option_Field,
      Breadcrumb,
      View,
      Box,
      Separator,
      Rich);

   subtype Interactive_Widget is Widget_Kind range Button .. View;
   subtype Labeled_Widget is Widget_Kind range Text .. Box;

   subtype Extended_Proportion is Float range -1.0 .. 1.0;

   type Widget_Access is access Widget_Type;

   type Rich_Text_Segment is record
      Text        : Unbounded_String;
      Date        : Calendar.Time := Unset_Time;
      Highlighted : Boolean := False;
      New_Line    : Boolean := False;
   end record;

   package String_Vectors is new Containers.Vectors (Positive, Unbounded_String);
   package Rich_Text_Segment_Vectors is new Containers.Vectors (Positive, Rich_Text_Segment);
   subtype Rich_Text_Vector is Rich_Text_Segment_Vectors.Vector;
   use type Rich_Text_Vector;

   type Rich_Text is record
      Segments : Rich_Text_Vector;
   end record;

   type Complete_Rich_Text is record
      Segments    : Rich_Text_Vector;
      Fixed_Width : Boolean;
   end record;

   type Widget_Type (Kind : Widget_Kind := Text) is record
      ID       : Natural := 0;
      Label    : Unbounded_String;
      Parent   : Widget_Access;
      Previous : Widget_Access;

      Enabled : Boolean := True;

      case Kind is
         when Text =>
            null;

         when Bar =>
            Bar_Value : Extended_Proportion;

         when Button =>
            Size : Visible_Size;

         when Checkbox =>
            Checkbox_Value  : Boolean;
            Checkbox_Access : access Boolean;

         when Integer_Field =>
            Min            : Integer;
            Max            : Integer;
            Integer_Value  : Integer;
            Integer_Access : access Integer;

         when Text_Field | Password_Field =>
            Text_Value  : Unbounded_String;
            Text_Access : access Unbounded_String;

         when Date_Field =>
            Date_Min    : Calendar.Time;
            Date_Value  : Calendar.Time;
            Date_Access : access Calendar.Time;

         when Option_Field | Breadcrumb | View =>
            Index : Integer;

            case Kind is
               when Option_Field =>
                  Labels : String_Vectors.Vector;

               when View =>
                  Description : Unbounded_String;

               when others =>
                  null;
            end case;

         when Box =>
            Axis              : Axis_Type;
            Content_Alignment : Alignment_Type;
            Fill              : Boolean;
            Bordered          : Boolean;
            Spacing           : Boolean;

         when Separator =>
            Visible        : Boolean;
            Separator_Size : Size_Type;

         when Rich =>
            Rich : Complete_Rich_Text;
      end case;
   end record;

   procedure Free is new Unchecked_Deallocation (Widget_Type, Widget_Access);

   function Equivalent_Widgets (Left, Right : not null Widget_Access) return Boolean;

   function Get_Path (Widget : Widget_Access) return String
   with Pre => Widget /= null;

   package Widget_Vectors is new Containers.Vectors (Index_Type => Positive, Element_Type => Widget_Access);
   package Widget_Maps is new Containers.Ordered_Maps (Key_Type => Natural, Element_Type => Widget_Access);

   type Window_Type is record
      Event_ID      : Natural := 0;
      Event_Data    : JSON.JSON_Value;
      Current_View  : Views := Views'First;
      View_Names    : View_Labels;
      View_Paths    : View_Labels;
      New_Widgets   : Widget_Vectors.Vector;
      Widgets_By_ID : Widget_Maps.Map;
   end record;

   type Container is tagged record
      Window  : access Window_Type;
      Widget  : Widget_Access;
      Current : Widget_Access;
   end record;

end Libre_Frame.UI;
