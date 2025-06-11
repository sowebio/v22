with Ada.Integer_Text_IO;
with Ada.Strings.Fixed;
with Ada.Text_IO;

package body Libre_Frame.Edit is
   use Ada.Containers;

   package Int_IO renames Integer_Text_IO;

   Debug : constant Boolean := False;

   function Minimal_Sequence (A, B : Vector) return List is
   begin
      if A.Is_Empty and B.Is_Empty then
         return [];
      end if;

      if A.Is_Empty then
         return Result : List (B.First_Index .. B.Last_Index) do
            for I in Result'Range loop
               Result (I) := (Kind => Create, Item => B (I));
            end loop;
         end return;
      end if;

      if B.Is_Empty then
         return Result : List (A.First_Index .. A.Last_Index) do
            for I in Result'Range loop
               Result (I) := (Kind => Create, Item => A (I));
            end loop;
         end return;
      end if;

      if A = B then
         return Result : List (A.First_Index .. A.Last_Index) do
            for I in Result'Range loop
               Result (I) := (Kind => Keep, Old_Item => A (I), New_Item => B (I));
            end loop;
         end return;
      end if;

      declare
         First  : constant Index_Type := A.First_Index;
         A_Last : constant Index_Type := A.Last_Index;
         B_Last : constant Index_Type := B.Last_Index;

         subtype Max_Range is Index_Type range First .. Index_Type'Max (A_Last, B_Last);
         subtype A_Range is Index_Type range First .. A_Last;
         subtype B_Range is Index_Type range First .. B_Last;

         Costs : array (First .. A_Last + 1, First .. B_Last + 1) of Unsigned_32;
         Edits : array (First .. A_Last + 1, First .. B_Last + 1) of Kind_T;
      begin
         if Debug then
            Text_IO.Put_Line ("~~~ E D I T S ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~");
            declare
               Lengths : array (1 .. 2) of Natural := [0, 0];
               Spacing : constant String := "  ";
            begin
               for I in Max_Range loop
                  if I <= A_Last then
                     declare
                        Img : constant String := Element_Type'(A (I))'Image;
                     begin
                        if Img'Length > Lengths (1) then
                           Lengths (1) := Img'Length;
                        end if;
                     end;
                  end if;
                  if I <= B_Last then
                     declare
                        Img : constant String := Element_Type'(B (I))'Image;
                     begin
                        if Img'Length > Lengths (2) then
                           Lengths (2) := Img'Length;
                        end if;
                     end;
                  end if;
               end loop;
               for I in Max_Range loop
                  Int_IO.Put (Integer (I), Max_Range'Width);
                  Text_IO.Put (Spacing);
                  declare
                     S : String (1 .. Lengths (1)) := [others => ' '];
                  begin
                     if I <= A_Last then
                        Strings.Fixed.Move (Element_Type'(A (I))'Image, S);
                     end if;
                     Text_IO.Put (S);
                  end;
                  Text_IO.Put (Spacing);
                  declare
                     S : String (1 .. Lengths (2)) := [others => ' '];
                  begin
                     if I <= B_Last then
                        Strings.Fixed.Move (Element_Type'(B (I))'Image, S);
                     end if;
                     Text_IO.Put (S);
                  end;
                  Text_IO.New_Line;
               end loop;
               Text_IO.New_Line;
            end;
         end if;

         for X in Costs'Range (1) loop
            Costs (X, First) := Unsigned_32 (X - First);
            Edits (X, First) := Delete;
         end loop;
         for Y in Costs'Range (2) loop
            Costs (First, Y) := Unsigned_32 (Y - First);
            Edits (First, Y) := Create;
         end loop;

         for X in First + 1 .. Costs'Last (1) loop
            for Y in First + 1 .. Costs'Last (2) loop
               declare
                  Create_Cost, Delete_Cost, Update_Cost, Smallest_Cost : Unsigned_32 := 0;
               begin
                  if Equivalent (A (X - 1), B (Y - 1)) then
                     Costs (X, Y) := Costs (X - 1, Y - 1);
                     Edits (X, Y) := Keep;
                  else
                     Delete_Cost := Costs (X - 1, Y) + 1;
                     Create_Cost := Costs (X, Y - 1) + 1;
                     Update_Cost := Costs (X - 1, Y - 1) + 1;
                     Smallest_Cost := Unsigned_32'Min (Unsigned_32'Min (Delete_Cost, Create_Cost), Update_Cost);
                     Costs (X, Y) := Smallest_Cost;
                     if Smallest_Cost = Update_Cost then
                        Edits (X, Y) := Update;
                     elsif Smallest_Cost = Delete_Cost then
                        Edits (X, Y) := Delete;
                     elsif Smallest_Cost = Create_Cost then
                        Edits (X, Y) := Create;
                     end if;
                  end if;
               end;
            end loop;
         end loop;

         if Debug then
            Text_IO.New_Line;
            declare
               subtype Cost_Range is Unsigned_32 range 0 .. Unsigned_32 (Count_Type'Max (A.Length, B.Length));

               X_Width    : constant Natural := A_Range'Width;
               Y_Width    : constant Natural := B_Range'Width;
               Cost_Width : constant Natural := Natural'Max (Cost_Range'Width, X_Width);

               function "*" (Left : Natural; Right : Character) return String renames Strings.Fixed."*";

               procedure Put_Separator is
               begin
                  Text_IO.Put_Line
                    ("+-" & (Y_Width - 1) * '-' & "-+" & (Natural (A.Length) * X_Width + X_Width) * '-' & "-+");
               end;
               procedure Put_Header is
               begin
                  Put_Separator;
                  Text_IO.Put ("| " & (Y_Width - 1) * ' ' & " | " & (X_Width - 1) * ' ');
                  for I in A_Range loop
                     Int_IO.Put (Integer (I), X_Width);
                  end loop;
                  Text_IO.Put_Line (" |");
                  Put_Separator;
               end;
               procedure Put_Line_Header (Y : Index_Type) is
               begin
                  if Y = First then
                     Text_IO.Put ("| " & (Y_Width - 1) * ' ' & " |");
                  else
                     Text_IO.Put ("| ");
                     Int_IO.Put (Integer (Y) - 1, Y_Width - 1);
                     Text_IO.Put (" |");
                  end if;
               end;
            begin
               -- Put Costs
               Put_Header;
               for Y in Costs'Range (2) loop
                  Put_Line_Header (Y);
                  for X in Costs'Range (1) loop
                     Int_IO.Put (Integer (Costs (X, Y)), Cost_Width);
                  end loop;
                  Text_IO.Put_Line (" |");
               end loop;
               Put_Separator;
               Text_IO.New_Line;

               -- Put Edits
               Put_Header;
               for Y in Edits'Range (2) loop
                  Put_Line_Header (Y);
                  for X in Edits'Range (1) loop
                     Text_IO.Put ((X_Width - 1) * ' ');
                     if X = Edits'First (1) and Y = Edits'First (2) then
                        Text_IO.Put (" ");
                     else
                        Text_IO.Put
                          (case Edits (X, Y) is
                             when Create => "↑",
                             when Delete => "←",
                             when Update => "↖",
                             when Keep => "⇖");
                     end if;
                  end loop;
                  Text_IO.Put_Line (" |");
               end loop;
               Put_Separator;
               Text_IO.New_Line;
            end;
         end if;

         declare
            Result : List (First .. A_Last + B_Last);
            Last   : Index_Type := Result'First;
            X      : Index_Type := Edits'Last (1);
            Y      : Index_Type := Edits'Last (2);
            procedure Reverse_Edits is new Reverse_Array (Index => Index_Type, Item => T, List => List);
         begin
            while X > First or Y > First loop
               case Edits (X, Y) is
                  when Create =>
                     Y := Y - 1;
                     Result (Last) := (Kind => Create, Item => B (Y));

                  when Delete =>
                     X := X - 1;
                     Result (Last) := (Kind => Delete, Item => A (X));

                  when Update =>
                     X := X - 1;
                     Y := Y - 1;
                     Result (Last) := (Kind => Update, Old_Item => A (X), New_Item => B (Y));

                  when Keep =>
                     X := X - 1;
                     Y := Y - 1;
                     Result (Last) := (Kind => Keep, Old_Item => A (X), New_Item => B (Y));
               end case;
               Last := Last + 1;
            end loop;
            Last := Last - 1;

            Reverse_Edits (Result (Result'First .. Last));

            if Debug then
               for I in Result'First .. Last loop
                  Int_IO.Put (Integer (I), Max_Range'Width);
                  case Result (I).Kind is
                     when Create =>
                        Text_IO.Put_Line ("  Create: " & Result (I).Item'Image);

                     when Delete =>
                        Text_IO.Put_Line ("  Delete: " & Result (I).Item'Image);

                     when Update =>
                        Text_IO.Put_Line
                          ("  Update: " & Result (I).Old_Item'Image & " to: " & Result (I).New_Item'Image);

                     when Keep =>
                        Text_IO.Put_Line
                          ("  Keep:   " & Result (I).Old_Item'Image & " as: " & Result (I).New_Item'Image);
                  end case;
               end loop;
            end if;

            return Result (Result'First .. Last);
         end;
      end;
   end Minimal_Sequence;

end Libre_Frame.Edit;
