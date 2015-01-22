with Ada.Calendar,
     Ada.Text_IO;

package Date is
   type Instance is
      record
         Year  : Ada.Calendar.Year_Number;
         Month : Ada.Calendar.Month_Number;
         Day   : Ada.Calendar.Day_Number;
      end record;

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Instance);
   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Instance);
end Date;
