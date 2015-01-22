with Ada.Integer_Text_IO;

package body Date is
   procedure Get (File  : in     Ada.Text_IO.File_Type;
                  Valid : in     Character);

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Instance) is
      use Ada.Integer_Text_IO;
   begin
      Get (File => File, Item  => Item.Year);
      Get (File => File, Valid => '-');
      Get (File => File, Item  => Item.Month);
      Get (File => File, Valid => '-');
      Get (File => File, Item  => Item.Day);
   end Get;

   procedure Get (File  : in     Ada.Text_IO.File_Type;
                  Valid : in     Character) is
      Item : Character;
   begin
      Ada.Text_IO.Get (File => File,
                       Item => Item);
      if Item /= Valid then
         raise Ada.Text_IO.Data_Error
           with "'" & Valid & "' expected.  Got '" & Item & "'.";
      end if;
   end Get;

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Instance) is
      use Ada.Integer_Text_IO, Ada.Text_IO;
   begin
      Put (File, Item.Year,  Width => 4);
      Put (File, '-');
      Put (File, Item.Month, Width => 2);
      Put (File, '-');
      Put (File, Item.Day,   Width => 2);
   end Put;
end Date;
