with Gnoga.Gui.Base;
with Gnoga.Gui.View;

package Folders is

   type Folder_Type is new Gnoga.Gui.View.View_Type with private;
   type Folder_Access is access all Folder_Type;
   type Pointer_To_Folder_Class is access all Folder_Type'Class;

   procedure Create_Folder
     (View   : in out Folder_Type;
      Parent : in out Gnoga.Gui.Base.Base_Type'Class;
      ID     : in     String := "");

      --------------------
      -- Create_Section --
      --------------------

   procedure Create_Section
     (View    : in out Folder_Type;
      Content :        String;
      Name    :        String;
      IMG_URL :        String := "");

      ----------------------
      -- Render_Folder --
      ----------------------

   procedure Render_Folder
     (View           : in out Folder_Type;
      Allow_Collapse : in     Boolean := False);

private
   type Folder_Type is new Gnoga.Gui.View.View_Type with null record;

end Folders;
