with Ada.Containers.Vectors;

-- TODO: make generic for arrays & iterators (if that's possible)
-- TODO: custom cost function
-- TODO: custom debug_image function
-- TODO: grid text table (with or without coords)
-- TODO: handle negative index in debug logs

generic
   with package Vectors is new Ada.Containers.Vectors (<>);

   with function Equivalent (Left, Right : Vectors.Element_Type) return Boolean is Vectors."=";

package Libre_Frame.Edit
is
   use Vectors;

   type Kind_T is (Create, Delete, Update, Keep);

   type T (Kind : Kind_T := Create) is record
      case Kind is
         when Create | Delete =>
            Item : Element_Type;

         when Update | Keep =>
            Old_Item, New_Item : Element_Type;
      end case;
   end record;

   type List is array (Index_Type range <>) of T;

   function Minimal_Sequence (A, B : Vector) return List;

end Libre_Frame.Edit;
