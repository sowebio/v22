with Libre_Frame.UI.Server;

package body State_Pkg is

   type Views is (Main);

   package UI is new Libre_Frame.UI (Views => Views);

   Global_Clicked : Natural := 0;

   type Client_Data is record
      Clicked : Natural := 0;
   end record;

   type Window_Data is record
      Clicked : Natural := 0;
   end record;

   function Redraw_Window
     (Current_View : Views; Frame : in out UI.Container; Client : in out Client_Data; Window : in out Window_Data)
      return Views
   is
      function Format_Message (I : Natural) return String
      is (case I is
            when 0 => "The button has not been clicked yet",
            when 1 => "The button has been clicked 1 time",
            when others => "The button has been clicked" & I'Image & " times");

      Box : UI.Container := Frame.Box ("box", UI.Vertical, Spacing => True);
   begin
      Box.Text (Format_Message (Global_Clicked) & " globally.");
      Box.Text (Format_Message (Client.Clicked) & " by the client.");
      Box.Text (Format_Message (Window.Clicked) & " on this window.");

      if Box.Button ("Click me") then
         Global_Clicked := @ + 1;
         Client.Clicked := @ + 1;
         Window.Clicked := @ + 1;
      end if;

      return Current_View;
   end Redraw_Window;

   package Server is new
     UI.Server (Client_Data => Client_Data, Window_Data => Window_Data, Redraw_Window => Redraw_Window);

   procedure Run renames Server.Run;

end State_Pkg;
