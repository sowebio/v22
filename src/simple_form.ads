with Gnoga.Gui.Base;
with Gnoga.Gui.Element.Form;
with Gnoga.Gui.View;
with UXStrings; use UXStrings;

package Simple_Form is

   type String_Array is array (Positive range <>) of UXString;

   type Row_Type is new Gnoga.Gui.View.View_Type with record
      Input    : aliased Gnoga.Gui.Element.Form.Text_Type;
      Row_Name : aliased UXString;
   end record;

   type Row_Set is array (Positive range <>) of Row_Type;

   type Row_Set_Ptr is access all Row_Set;

   type Form_View_Type is new Gnoga.Gui.View.View_Type with record
      Form      : Gnoga.Gui.Element.Form.Form_Type;
      Message   : Gnoga.Gui.Element.Form.Text_Area_Type;
      Submit    : Gnoga.Gui.Element.Form.Submit_Button_Type;
      Row_Count : Positive;
   end record;

   function Field_By_Index
     (Source    : in UXString;
      Index_Num : in Positive;
      Separator :    UXString)
      return UXString;

   function All_Fields
     (Source    : in UXString;
      Separator :    UXString)
      return String_Array;

   function Remove_First_Occurence
     (Source : in UXString;
      Letter :    UXString)
      return UXString;

   function Remove_All_Occurences
     (Source : in UXString;
      Letter :    UXString)
      return UXString;

   procedure Create
     (View    : in out Form_View_Type;
      Parent  : in out Gnoga.Gui.Base.Base_Type'Class;
      ID      : in     Gnoga.String := "";
      Strings : in     String_Array);

end Simple_Form;
