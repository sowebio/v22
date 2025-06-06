private with Ada.Exceptions;
private with GNAT.Sockets.Connection_State_Machine.HTTP_Server;
private with GNAT.Sockets.Server;

generic
   type Client_Data is private;
   type Window_Data is private;

   with
     function Redraw_Window
       (Current_View : Views; Frame : in out Container; Client : in out Client_Data; Window : in out Window_Data)
        return Views;

   View_Names : View_Labels := [others => <>];
   View_Paths : View_Labels := [others => <>];

   Port : Positive := 8080;

package Libre_Frame.UI.Server
is
   procedure Run;
private
   use Ada, GNAT, GNATCOLL;

   package HTTP renames Sockets.Connection_State_Machine.HTTP_Server;

   subtype Client_ID_Type is Bytes (0 .. 15);

   type Factory_Type
     (Request_Length  : Positive;
      Input_Size      : Sockets.Server.Buffer_Length;
      Output_Size     : Sockets.Server.Buffer_Length;
      Max_Connections : Positive)
   is new Sockets.Server.Connections_Factory with null record;

   overriding
   function Create
     (Factory  : access Factory_Type;
      Listener : access Sockets.Server.Connections_Server'Class;
      From     : Sockets.Sock_Addr_Type) return Sockets.Server.Connection_Ptr;

   overriding
   procedure Trace_Error
     (Factory : in out Factory_Type; Context : String; Occurrence : Exceptions.Exception_Occurrence);

   type Connection_State is (Init, ID_Exchange, Ready, Event_Loop);

   type HTTP_Connection;

   type Connection_Access is access HTTP_Connection;

   type HTTP_Connection
     (Listener       : access Sockets.Server.Connections_Server'Class;
      Request_Length : Positive;
      Input_Size     : Sockets.Server.Buffer_Length;
      Output_Size    : Sockets.Server.Buffer_Length)
   is
     new HTTP.HTTP_Client
          (Listener => Listener,
           Request_Length => Request_Length,
           Input_Size => Input_Size,
           Output_Size => Output_Size)
   with record
      Self                : Connection_Access;
      ID                  : Natural;
      Client_ID           : Client_ID_Type;
      State               : Connection_State := Init;
      Client_Data_Access  : access Client_Data;
      Current_Widget_ID   : Natural := 5;
      Current_Window_Data : Window_Data;
      Old_Widgets         : Widget_Vectors.Vector;
      New_Widgets_Copy    : Widget_Vectors.Vector;
      Window              : aliased Window_Type;
   end record;

   overriding
   procedure Do_Get (Connection : in out HTTP_Connection);

   overriding
   function WebSocket_Open (Connection : access HTTP_Connection) return HTTP.WebSocket_Accept;

   overriding
   procedure WebSocket_Initialize (Connection : in out HTTP_Connection);

   overriding
   procedure WebSocket_Finalize (Connection : in out HTTP_Connection);

   not overriding
   procedure WebSocket_Send (Connection : in out HTTP_Connection; Data : Bytes);

   overriding
   procedure WebSocket_Received (Connection : in out HTTP_Connection; Data : String);

   overriding
   procedure WebSocket_Received (Connection : in out HTTP_Connection; Data_Array : Streams.Stream_Element_Array);

end Libre_Frame.UI.Server;
