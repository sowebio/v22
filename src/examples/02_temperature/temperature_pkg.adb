with Libre_Frame.UI.Server;

package body Temperature_Pkg is

   type Views is (Main);

   package UI is new Libre_Frame.UI (Views => Views);

   type Null_Record is null record;

   Celcius    : Integer := 20;
   Fahrenheit : Integer := 68;

   function Redraw_Window
     (Current_View : Views; Frame : in out UI.Container; Client, Window : in out Null_Record) return Views
   is
      Group : UI.Container := Frame.Group ("Temperature converter", Content_Alignment => UI.Top_Right);
   begin
      if Group.Integer_Field ("Celcius:", Celcius, Min => -20, Max => 60) then
         Fahrenheit := Integer (Float'Rounding (Float (Celcius) * 9.0 / 5.0)) + 32;

      elsif Group.Integer_Field ("Fahrenheit:", Fahrenheit, Min => -4, Max => 140) then
         Celcius := Integer (Float'Rounding (Float (Fahrenheit - 32) * 5.0 / 9.0));
      end if;

      return Current_View;
   end Redraw_Window;

   package Server is new
     UI.Server (Client_Data => Null_Record, Window_Data => Null_Record, Redraw_Window => Redraw_Window);

   procedure Run renames Server.Run;

end Temperature_Pkg;
