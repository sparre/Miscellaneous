with Ada.Text_IO;

package Time_Of_Day is
   subtype Hour_Number is Integer range 0 .. 24;
   subtype Minute_Number is Integer range 0 .. 59;

   type Instance is
      record
         Hour   : Hour_Number;
         Minute : Minute_Number;
      end record;

   function "-" (Left, Right : in Instance) return Duration;

   function "<" (Left, Right : in Instance) return Boolean;

   function ">=" (Left, Right : in Instance) return Boolean;

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Instance);
   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Instance);
end Time_Of_Day;
