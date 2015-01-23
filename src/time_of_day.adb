with Get_And_Check,
     Zero_Filled_2_Digit_Image;

package body Time_Of_Day is
   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Instance) is
      use Ada.Text_IO;
      Hours   : String (1 .. 2);
      Minutes : String (1 .. 2);
   begin
      Get           (File => File, Item  => Hours);
      Get_And_Check (File => File, Valid => ':');
      Get           (File => File, Item  => Minutes);

      Item := (Hour   => Integer'Value (Hours),
               Minute => Integer'Value (Minutes));
   end Get;

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Instance) is
      use Ada.Text_IO;
   begin
      Put (File, Zero_Filled_2_Digit_Image (Item.Hour));
      Put (File, ':');
      Put (File, Zero_Filled_2_Digit_Image (Item.Minute));
   end Put;
end Time_Of_Day;
