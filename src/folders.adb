with Gnoga.Gui.Element;
with Gnoga.Gui.Element.Common;
with UXStrings; use UXStrings;

package body Folders is

   procedure Create_Folder
     (View   : in out Folder_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "")
   is
   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, From_UTF_8 (ID));
   end Create_Folder;

   --------------------
   -- Create_Section --
   --------------------

   --  procedure Create_Section
   --    (View    : in out Folder_Type;
   --     Heading :        String)
   --  is
   --  begin
   --     null;
   --     View.Put_HTML ("<h3>" & Gnoga.Escape_Quotes (From_UTF_8(Heading)) & "</h3>");
   --  end Create_Section;

   procedure Create_Section
     (View    : in out Folder_Type;
      Content :        String;
      Name    :        String;
      IMG_URL :        String := "")
   is
      Button : constant Gnoga.Gui.Element.Pointer_To_Element_Class := new Gnoga.Gui.Element.Common.Button_Type;
   begin
      Gnoga.Gui.Element.Common.Button_Access (Button).Create (View, From_Latin_1 (Content));
      View.Add_Element (From_Latin_1 (Name), Button);
      Button.Dynamic;
      if IMG_URL /= "" then
         Button.Background_Image (Value => From_Latin_1 (IMG_URL));
         --Button.Background("linear-gradient(green, blue)");
         Button.Background_Position (Value => "10px 10px");
         Button.Background_Repeat (Value => "no-repeat");
         Button.Background_Size (Value => "30px 30px");
      end if;
   end Create_Section;

   ----------------------
   -- Render_Folder --
   ----------------------

   procedure Render_Folder
     (View           : in out Folder_Type;
      Allow_Collapse : in     Boolean := False)
   is
      function params return String;

      function params return String is
      begin
         if Allow_Collapse then
            return "{ collapsible: true }";
         end if;
         return "";
      end params;
   begin
      View.jQuery_Execute (From_UTF_8 ("folder(" & params & ")"));
   end Render_Folder;

end Folders;
