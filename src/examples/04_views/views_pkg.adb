with Libre_Frame.UI.Server;

package body Views_Pkg is

   type Views is (First, Second, Third);

   package UI is new Libre_Frame.UI (Views => Views);

   type Null_Record is null record;

   function Redraw_Window
     (Current_View : Views; Frame : in out UI.Container; Client, Window : in out Null_Record) return Views
   is
      Header : UI.Container := Frame.Box ("header", UI.Horizontal, Spacing => True);
   begin
      Header.Text ("View:");
      if Header.Navigation_Button (First) then
         return First;
      elsif Header.Navigation_Button (Second) then
         return Second;
      elsif Header.Navigation_Button (Third) then
         return Third;
      end if;

      case Current_View is
         when First =>
            Frame.Bar ("One-third:", (Valid => True, Value => 0.3333));

         when Second =>
            Frame.Text ("This is the second view");

         when Third =>
            declare
               use type UI.Rich_Text;
               Group : UI.Container := Frame.Group ("Snippet");
            begin
               Group.Text
                 (UI.Highlighted ("case")
                  & " Current_View "
                  & UI.Highlighted ("is")
                  & UI.New_Line
                  & UI.Highlighted ("   when")
                  & " Third "
                  & UI.Highlighted ("=>"),
                  Fixed_Width => True);
            end;
      end case;

      return Current_View;
   end Redraw_Window;

   package Server is new
     UI.Server
       (Client_Data      => Null_Record,
        Window_Data      => Null_Record,
        Redraw_Window    => Redraw_Window,
        Application_Name => "Views");

   procedure Run renames Server.Run;

end Views_Pkg;
