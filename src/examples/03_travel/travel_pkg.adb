with Ada.Calendar;
with Ada.Strings.Unbounded;
with Libre_Frame.UI.Server;

package body Travel_Pkg is

   type Views is (Main);

   package UI is new Libre_Frame.UI (Views => Views);

   type Null_Record is null record;

   type Travel_Type is (One_Way, Round_Trip);
   package Travel_Choice is new UI.Choice (Travel_Type);
   Travel_Choice_Labels : constant Travel_Choice.Labels :=
     [One_Way    => Ada.Strings.Unbounded.To_Unbounded_String ("One-way"),
      Round_Trip => Ada.Strings.Unbounded.To_Unbounded_String ("Round-trip")];

   Now   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
   Today : constant Ada.Calendar.Time :=
     Ada.Calendar.Time_Of
       (Year    => Ada.Calendar.Year (Now),
        Month   => Ada.Calendar.Month (Now),
        Day     => Ada.Calendar.Day (Now),
        Seconds => 0.0);

   Travel      : Travel_Type := One_Way;
   Departure   : Ada.Calendar.Time := Today;
   Return_Date : Ada.Calendar.Time := Today;

   function Redraw_Window
     (Current_View : Views; Frame : in out UI.Container; Client, Window : in out Null_Record) return Views
   is
      use type Ada.Calendar.Time;
   begin
      Travel_Choice.Option_Field (Frame, "Travel type:", Travel, Custom_Labels => Travel_Choice_Labels);
      Frame.Separator;

      declare
         Group : UI.Container := Frame.Group ("Travel dates", Content_Alignment => UI.Top_Right);
      begin
         Group.Date_Field ("Departure:", Departure, From => Today);
         Group.Date_Field
           ("Return:",
            Return_Date,
            From    => (if Departure = UI.Unset_Time then Today else Departure),
            Enabled => Travel = Round_Trip);

         if Departure = UI.Unset_Time then
            Group.Text ("Please set a valid departure date.");
         end if;

         if Travel = Round_Trip and Return_Date = UI.Unset_Time then
            Group.Text ("Please set a valid return date.");
         end if;

         if Group.Button
              ("Book",
               Size    => UI.Fill,
               Enabled => Departure /= UI.Unset_Time and (Travel = One_Way or Return_Date /= UI.Unset_Time))
         then
            null;
         end if;
      end;

      return Current_View;
   end Redraw_Window;

   package Server is new
     UI.Server (Client_Data => Null_Record, Window_Data => Null_Record, Redraw_Window => Redraw_Window);

   procedure Run renames Server.Run;

end Travel_Pkg;
