with Gnoga.Gui.Element.Table;

package body Simple_Form is

   function Field_By_Index
     (Source    : in UXString;
      Index_Num : in Positive;
      Separator :    UXString)
      return UXString
   is
      Source_Copy : UXString := Source;
      Field       : UXString;
      Field_Num   : Integer  := 0;
      Cursor      : Integer;
   begin
      while Field_Num < Index_Num loop
         Cursor      := Integer'Min (Index (Source_Copy, Separator), Source_Copy.Length);
         Field       := Head (Source_Copy, Cursor - 1);
         Source_Copy := Tail (Source_Copy, Source_Copy.Length - Cursor);
         Field_Num   := Field_Num + 1;
      end loop;
      return Field;
   end Field_By_Index;

   function All_Fields
     (Source    : in UXString;
      Separator :    UXString)
      return String_Array
   is
      Source_Copy     : UXString          := Source;
      Separator_Count : constant Positive := Count (Source, Separator);
      Fields          : String_Array (1 .. Separator_Count + 1);
      Field_Num       : Positive          := 1;
      Cursor          : Positive;
   begin
      while Field_Num <= Separator_Count loop
         Cursor             := Index (Source_Copy, Separator);
         Fields (Field_Num) := Head (Source_Copy, Cursor - 1);
         Source_Copy        := Tail (Source_Copy, Source_Copy.Length - Cursor);
         Field_Num          := Field_Num + 1;
      end loop;
      Cursor             := Source_Copy.Length + 1;
      Fields (Field_Num) := Head (Source_Copy, Cursor - 1);
      return Fields;
   end All_Fields;

   function Remove_First_Occurence
     (Source : in UXString;
      Letter :    UXString)
      return UXString
   is
      Result      : UXString := Source;
      Tail_Source : UXString;
      Cursor      : Integer;
   begin
      Cursor := Index (Result, Letter);
      if Cursor > 0 then
         Tail_Source := Tail (Result, Result.Length - Cursor);
         Result      := Head (Result, Cursor - 1) & Tail_Source;
      end if;
      return Result;
   end Remove_First_Occurence;

   function Remove_All_Occurences
     (Source : in UXString;
      Letter :    UXString)
      return UXString
   is
      Letter_Count : constant Positive := Count (Source, Letter);
      Result       : UXString          := Source;
      Tail_Source  : UXString;
      Cursor       : Integer;
      Field_Num    : Positive          := 1;
   begin
      while Field_Num <= Letter_Count loop
         Cursor := Index (Result, Letter);
         if Cursor > 0 then
            Tail_Source := Tail (Result, Result.Length - Cursor);
            Result      := Head (Result, Cursor - 1) & Tail_Source;
         end if;
         Field_Num := Field_Num + 1;
      end loop;
      return Result;
   end Remove_All_Occurences;

   procedure Create
     (View    : in out Form_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     Gnoga.String := "";
      Strings :        String_Array)
   is
      use Gnoga.Gui.Element.Table;
      Layout_Table : constant Table_Access := new Table_Type;

   begin
      Gnoga.Gui.View.View_Type (View).Create (Parent, ID);

      View.Form.Create (View, "result", Gnoga.Gui.Element.Form.Post);
      Layout_Table.Dynamic;
      Layout_Table.Create (View);
      View.Row_Count := Strings'Length;

      declare
         Rows : constant Row_Set_Ptr := new Row_Set (1 .. View.Row_Count);
      begin
         for i in 1 .. View.Row_Count loop
            declare
               row  : constant Table_Row_Access    := new Table_Row_Type;
               col1 : constant Table_Column_Access := new Table_Column_Type;
               col2 : constant Table_Column_Access := new Table_Column_Type;
            begin
               row.Dynamic;
               col1.Dynamic;
               col2.Dynamic;

               row.Create (Layout_Table.all);

               Rows (i).Row_Name := Strings (i);

               Rows (i).Input.Create
                 (Form => View.Form, Size => 40, Name => Remove_First_Occurence (Rows (i).Row_Name, "'"));

               col1.Create (row.all, Rows (i).Row_Name);
               col2.Create (row.all);
               Rows (i).Input.Place_Inside_Top_Of (col2.all);
            end;
         end loop;
      end;
      View.Submit.Create (Form => View.Form, Value => "Envoyer");
      View.Submit.Place_After (Layout_Table.all);
      View.Submit.Add_Class ("submit");
   end Create;

end Simple_Form;
