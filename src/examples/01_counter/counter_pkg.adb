with Libre_Frame.UI.Server;

package body Counter_Pkg is

   type Views is (Main);

   package UI is new Libre_Frame.UI (Views => Views);

   type Null_Record is null record;

   Counter : Integer := 0;

   function Redraw_Window
     (Current_View : Views; Frame : in out UI.Container; Client, Window : in out Null_Record) return Views is
   begin
      Frame.Text ("Counter:" & Counter'Image);

      if Frame.Button ("Increment") then
         Counter := Counter + 1;
      end if;

      return Current_View;
   end Redraw_Window;

   package Server is new
     UI.Server (Client_Data => Null_Record, Window_Data => Null_Record, Redraw_Window => Redraw_Window);

   procedure Run renames Server.Run;

end Counter_Pkg;
