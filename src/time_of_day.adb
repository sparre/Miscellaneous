with Get_And_Check,
     Zero_Filled_2_Digit_Image;

package body Time_Of_Day is
   function "-" (Left, Right : in Instance) return Duration is
   begin
      return (Left.Hour   - Right.Hour) * 3600.0 +
             (Left.Minute - Right.Minute) * 60.0;
   end "-";

   function "<" (Left, Right : in Instance) return Boolean is
   begin
      return (Left.Hour < Right.Hour) or
             (Left.Hour = Right.Hour and Left.Minute < Right.Minute);
   end "<";

   function ">=" (Left, Right : in Instance) return Boolean is
   begin
      return (Left.Hour >= Right.Hour) or
             (Left.Hour  = Right.Hour and Left.Minute >= Right.Minute);
   end ">=";

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
