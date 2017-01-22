with Ada.Text_IO;

function Zero_Filled_Image (Item : in Num) return String is
   package Numeric_IO is new Ada.Text_IO.Integer_IO (Num => Num);
   Result : String (1 .. Width);
begin
   Numeric_IO.Put (To   => Result,
                   Item => Item);

   for Index in Result'Range loop
      if Result (Index) = ' ' then
         Result (Index) := '0';
      end if;
   end loop;

   return Result;
end Zero_Filled_Image;
