with Ada.Containers.Ordered_Maps;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with Libre_Frame.UI.Server;

package body Complete_Pkg is
   use Ada, Ada.Strings.Unbounded, Libre_Frame;

   package String_To_String_Maps is new Containers.Ordered_Maps (Unbounded_String, Unbounded_String);

   type Global_Data is record
      Username : Unbounded_String := To_Unbounded_String ("xp");
      Password : Unbounded_String := To_Unbounded_String ("xp");
   end record;

   Global : Global_Data;

   type Bits_Type is array (Integer range <>) of Boolean with Pack;

   type Client_Data is record
      Logged_In : Boolean := False;

      Bytes : Integer := 2;
      Bits  : Bits_Type (1 .. 32) := [others => False];

      Passwords : String_To_String_Maps.Map;

      Line_Count     : Integer := 5;
      Max_Line_Count : Integer := 99;
   end record;

   type Login_Data is record
      Username, Password : Unbounded_String;
      Wrong_Credentials  : Boolean := False;
   end record;

   type Profile_Data is record
      Username : Unbounded_String := Global.Username;
      Password : Unbounded_String := Global.Password;
   end record;

   type Passwords_Data is record
      Key      : Unbounded_String;
      Password : Unbounded_String;
   end record;

   type Null_Record is null record;

   type Examples_Counter_Data is record
      Counter : Natural := 0;
      Checked : Boolean := True;
   end record;

   type Views is (Main, Login, Profile, Passwords, Examples, Examples_Hello, Examples_Counter, Examples_Slider);

   type View_Data (Current : Views := Main) is record
      case Current is
         when Main =>
            Main : Null_Record;

         when Login =>
            Login : Login_Data;

         when Profile =>
            Profile : Profile_Data;

         when Passwords =>
            Passwords : Passwords_Data;

         when Examples =>
            Examples : Null_Record;

         when Examples_Hello =>
            Examples_Hello : Null_Record;

         when Examples_Counter =>
            Examples_Counter : Examples_Counter_Data;

         when Examples_Slider =>
            Examples_Slider : Null_Record;
      end case;
   end record;

   type Window_Data is record
      Redirect    : Boolean := False;
      Redirect_To : Views;

      View : View_Data;
   end record;

   package UI is new Libre_Frame.UI (Views => Views);
   use UI;

   View_Paths : constant View_Labels :=
     [Main             => To_Unbounded_String (""),
      Login            => To_Unbounded_String ("login"),
      Profile          => To_Unbounded_String ("profile"),
      Passwords        => To_Unbounded_String ("passwords"),
      Examples         => To_Unbounded_String ("examples"),
      Examples_Hello   => To_Unbounded_String ("examples/hello-world"),
      Examples_Counter => To_Unbounded_String ("examples/counter"),
      Examples_Slider  => To_Unbounded_String ("examples/slider")];

   View_Names : constant View_Labels :=
     [Main             => To_Unbounded_String (""),
      Login            => To_Unbounded_String ("Login"),
      Profile          => To_Unbounded_String ("Profile"),
      Passwords        => To_Unbounded_String ("Passwords"),
      Examples         => To_Unbounded_String ("Examples"),
      Examples_Hello   => To_Unbounded_String ("Hello, World!"),
      Examples_Counter => To_Unbounded_String ("Counter"),
      Examples_Slider  => To_Unbounded_String ("Slider")];

   function Main (Frame : in out Container; Client : in out Client_Data; View : in out Null_Record) return Views is
      Row : Container := Frame.Box ("row", Horizontal, Middle_Left);
   begin
      if Row.Button ("Reset") then
         Client.Bits := [others => False];
      end if;

      Row.Separator;
      Row.Integer_Field ("Number of bytes", Client.Bytes, Min => 1, Max => 4);

      declare
         subtype My_Bits is Bits_Type (1 .. Client.Bytes * 8);
         subtype My_Bytes is Bytes (0 .. Natural_64 (Client.Bytes) - 1);
         function To_Bytes is new Ada.Unchecked_Conversion (Source => My_Bits, Target => My_Bytes);

         Current_Bytes : constant My_Bytes := To_Bytes (Client.Bits (1 .. Client.Bytes * 8));

         Column : Container := Frame.Box ("Column", Vertical, Bordered => True, Spacing => True);
         Row    : Container := Column.Box ("Row", Horizontal);
      begin
         for I in 1 .. Client.Bytes * 8 loop
            if I > 1 and then (I - 1) mod 8 = 0 then
               Row.Separator;
            end if;
            declare
               Column : Container := Row.Box ("Column_" & I'Image, Vertical, Top_Center);
            begin
               Column.Text
                 (Strings.Fixed.Trim (Boolean'Pos (Client.Bits (I))'Image, Strings.Left), Fixed_Width => True);
               Column.Checkbox ("##", Client.Bits (I));
            end;
         end loop;

         Column.Text ("Hex: " & Hexadecimal (Current_Bytes), Fixed_Width => True);
         --  Column.Text ("Hex: " & Hexadecimal (To_Bytes (Window.Bits)) & " Number:" & To_Unsigned_64 (To_Bytes (Window.Bits))'Image);
         return Main;
      end;
   end Main;

   function Profile (Frame : in out Container; Client : in out Client_Data; View : in out Profile_Data) return Views is
      Group : Container := Frame.Group ("Change credentials", Top_Right);
   begin
      Group.Text_Field ("Username:", View.Username);
      Group.Password_Field ("Password:", View.Password);

      if Group.Button ("Save", Size => Fill) then
         Global.Username := View.Username;
         Global.Password := View.Password;
      end if;

      return Profile;
   end Profile;

   function Passwords
     (Frame : in out Container; Client : in out Client_Data; View : in out Passwords_Data) return Views
   is
      use String_To_String_Maps;
   begin
      for Pos in Client.Passwords.Iterate loop
         Frame.Text (To_String ("key: " & Key (Pos) & " password: " & Element (Pos)));
      end loop;

      declare
         Group : Container := Frame.Group ("New password", Top_Right);
      begin
         Group.Text_Field ("Key:", View.Key);
         Group.Password_Field ("Password:", View.Password);

         if Group.Button ("Add", Fill) and Length (View.Key) > 0 and Length (View.Password) > 0 then
            Client.Passwords.Include (View.Key, View.Password);
         end if;
      end;

      return Passwords;
   end Passwords;

   function Examples (Frame : in out Container; Client : in out Client_Data; View : in out Null_Record) return Views is
   begin
      if Frame.Navigation_Button (Examples_Hello) then
         return Examples_Hello;
      elsif Frame.Navigation_Button (Examples_Counter) then
         return Examples_Counter;
      elsif Frame.Navigation_Button (Examples_Slider) then
         return Examples_Slider;
      end if;

      return Examples;
   end Examples;

   function Examples_Hello
     (Frame : in out Container; Client : in out Client_Data; View : in out Null_Record) return Views is
   begin
      Frame.Append (Text ("Hello, World!") & Text ("Deuxième ligne"));
      return Examples_Hello;
   end Examples_Hello;

   function Examples_Counter
     (Frame : in out Container; Client : in out Client_Data; View : in out Examples_Counter_Data) return Views
   is
      Counter : Container := Frame.Group ("Counter");
   begin
      Counter.Checkbox ("Display counter", View.Checked);

      if View.Checked then
         Counter.Text ("Counter:" & View.Counter'Image);

         if Counter.Button ("Increment", Fill) then
            View.Counter := @ + 1;
         end if;
      end if;

      return Examples_Counter;
   end Examples_Counter;

   function Examples_Slider
     (Frame : in out Container; Client : in out Client_Data; View : in out Null_Record) return Views
   is
      Fields : Container := Frame.Box ("fields", Vertical, Top_Right, Bordered => True, Spacing => True);
      Inner  : Container := Frame.Box ("inner", Vertical);
   begin
      Fields.Integer_Field ("Maximum number of lines:", Client.Max_Line_Count, Min => 0, Max => 100);
      if Client.Line_Count > Client.Max_Line_Count then
         Client.Line_Count := Client.Max_Line_Count;
      end if;
      Fields.Integer_Field ("Number of lines:", Client.Line_Count, Min => 0, Max => Client.Max_Line_Count);
      Inner.Bar
        ("Ratio:",
         (if Client.Max_Line_Count > 0
          then (Valid => True, Value => Float (Client.Line_Count) / Float (Client.Max_Line_Count))
          else (Valid => False)));

      for I in 1 .. Client.Line_Count loop
         Inner.Text ("Line" & I'Image);
      end loop;

      return Examples_Slider;
   end Examples_Slider;

   function Redraw_Window
     (Current_View : Views; Frame : in out Container; Client : in out Client_Data; Window : in out Window_Data)
      return Views is
   begin
      if Window.View.Current /= Current_View then
         declare
            New_View_Data : View_Data (Current_View);
         begin
            Window.View := New_View_Data;
         end;
      end if;

      if Current_View = Login then
         if Client.Logged_In then
            return Main;
         end if;

         declare
            Box         : Container := Frame.Box ("box", Horizontal, Middle_Center, Fill => True);
            Credentials : Container := Box.Group ("Credentials", Top_Right);
         begin
            if Credentials.Text_Field ("Username:", Window.View.Login.Username) then
               Window.View.Login.Wrong_Credentials := False;
            end if;

            if Credentials.Password_Field ("Password:", Window.View.Login.Password) then
               Window.View.Login.Wrong_Credentials := False;
            end if;

            if Window.View.Login.Wrong_Credentials then
               Credentials.Box ("row", Horizontal, Fill => True, Widget => Text ("Wrong credentials"));
            end if;

            if Credentials.Button ("Login", Fill) then
               if Window.View.Login.Username = Global.Username and Window.View.Login.Password = Global.Password then
                  Client.Logged_In := True;

                  if Window.Redirect then
                     Window.Redirect := False;
                     return Window.Redirect_To;
                  end if;

                  return Main;
               end if;

               Window.View.Login.Wrong_Credentials := True;
            end if;
         end;

         return Login;
      end if;

      if Window.Redirect then
         Window.Redirect := False;

         if Current_View /= Window.Redirect_To then
            return Window.Redirect_To;
         end if;
      end if;

      if not Client.Logged_In then
         pragma Assert (Current_View /= Login); -- This case was handled before

         if not Window.Redirect then
            -- We are not in the Login window, so we save the view so we can come back to it later
            Window.Redirect := True;
            Window.Redirect_To := Current_View;
         end if;

         return Login;
      end if;

      declare
         Lower, Sidebar, Main_Area, Content : Container;

         Header : Container := Frame.Box ("header", Horizontal, Middle_Left, Widget => Separator);

         Next_View : constant Views := Header.Breadcrumb;
      begin
         if Next_View /= Current_View then
            return Next_View;
         end if;

         Header.Separator (Size => Fill);
         if Header.Navigation_Button (Profile, " (" & To_String (Global.Username) & ")") then
            return Profile;
         end if;

         Header.Separator;
         if Header.Button ("Logout") then
            Client.Logged_In := False;
            Window.Redirect := True;
            Window.Redirect_To := Current_View;
            return Login;
         end if;

         Header.Separator;

         Frame.Separator (Size => Zero, Visible => True);
         Lower := Frame.Box ("lower", Horizontal, Fill => True);
         Sidebar := Lower.Box ("sidebar", Vertical, Spacing => True);
         Lower.Separator (Size => Zero, Visible => True);
         Main_Area := Lower.Box ("content", Vertical, Fill => True);
         Content := Main_Area.Box ("content", Vertical, Spacing => True);

         if Sidebar.Navigation_Button (Examples) then
            return Examples;
         elsif Sidebar.Navigation_Button (Passwords) then
            return Passwords;
         end if;

         case Current_View is
            when Login =>
               raise Program_Error with "unreachable";

            when Main =>
               return Main (Content, Client, Window.View.Main);

            when Profile =>
               return Profile (Content, Client, Window.View.Profile);

            when Passwords =>
               return Passwords (Content, Client, Window.View.Passwords);

            when Examples =>
               return Examples (Content, Client, Window.View.Examples);

            when Examples_Hello =>
               return Examples_Hello (Content, Client, Window.View.Examples_Hello);

            when Examples_Counter =>
               return Examples_Counter (Content, Client, Window.View.Examples_Counter);

            when Examples_Slider =>
               return Examples_Slider (Content, Client, Window.View.Examples_Slider);
         end case;
      end;
   end Redraw_Window;

   package Server is new
     UI.Server
       (Client_Data      => Client_Data,
        Window_Data      => Window_Data,
        Redraw_Window    => Redraw_Window,
        View_Names       => View_Names,
        View_Paths       => View_Paths,
        Application_Name => "SaaS");

   procedure Run renames Server.Run;

end Complete_Pkg;
