------------------------------------------------------------------------------
--                                                                          --
--                   GNOGA - The GNU Omnificent GUI for Ada                 --
--                                                                          --
--                G N O G A . G U I . E L E M E N T . S T Y L E             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--                     Copyright (C) 2014 David Botton                      --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file might be   --
--  covered by the  GNU Public License.                                     --
--                                                                          --
--  For more information please go to http://www.gnoga.com                  --
------------------------------------------------------------------------------

package body Gnoga.Gui.Element.Style_Block is

   ------------
   -- Create --
   ------------

   procedure Create
     (Style   : in out Style_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      Content : in     String := "";
      ID      : in     String := "")
   is
   begin
      Style.Create_From_HTML (Parent, "<style>" & Escape_Quotes (Content) & "</style>", ID);
   end Create;

   ---------------
   -- Add_Style --
   ---------------

   procedure Add_Style
     (Style : in out Style_Type;
      Text  : in     String)
   is
   begin
      Style.Text (Style.Text & " " & Text);
   end Add_Style;

   ---------------------------
   -- Add_Style_for_Element --
   ---------------------------

   procedure Add_Style_for_Element
     (Style : in out Style_Type;
      Name  : in     String;
      Text  : in     String)
   is
   begin
      Style.Add_Style (Name & " {" & Text & "}");
   end Add_Style_for_Element;

   ----------------------
   -- Add_Style_for_ID --
   ----------------------

   procedure Add_Style_for_ID
     (Style : in out Style_Type;
      ID    : in     String;
      Text  : in     String)
   is
   begin
      Style.Add_Style ("#" & ID & " {" & Text & "}");
   end Add_Style_for_ID;

   -------------------------
   -- Add_Style_for_Class --
   -------------------------

   procedure Add_Style_for_Class
     (Style : in out Style_Type;
      Name  : in     String;
      Text  : in     String)
   is
   begin
      Style.Add_Style ("." & Name & " {" & Text & "}");
   end Add_Style_for_Class;

end Gnoga.Gui.Element.Style_Block;
