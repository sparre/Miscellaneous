with Ada.Integer_Text_IO;

with Get_And_Check,
     Zero_Filled_2_Digit_Image;

package body Date is
   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Instance) is
      use Ada.Integer_Text_IO;
   begin
      Get           (File, Item.Year);
      Get_And_Check (File, '-');
      Get           (File, Item.Month);
      Get_And_Check (File, '-');
      Get           (File, Item.Day);
   end Get;

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Instance) is
      use Ada.Integer_Text_IO, Ada.Text_IO;
   begin
      Put (File, Item.Year,  Width => 4);
      Put (File, '-');
      Put (File, Zero_Filled_2_Digit_Image (Item.Month));
      Put (File, '-');
      Put (File, Zero_Filled_2_Digit_Image (Item.Day));
   end Put;
end Date;
