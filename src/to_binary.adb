with Ada.Command_Line;
with Ada.Long_Integer_Text_IO;
with Ada.Text_IO;

procedure To_Binary is
   use Ada.Command_Line;
   use Ada.Long_Integer_Text_IO;
   use Ada.Text_IO;
begin
   for Index in 1 .. Argument_Count loop
      Put (Item  => """" & Argument (Index) & """");
      Put (Item  => " = ");
      Put (Item  => Long_Integer'Value (Argument (Index)),
           Width => 0);
      Put (Item  => " = ");
      Put (Item  => Long_Integer'Value (Argument (Index)),
           Width => 0,
           Base  => 16);
      Put (Item  => " = ");
      Put (Item  => Long_Integer'Value (Argument (Index)),
           Width => 0,
           Base  => 8);
      Put (Item  => " = ");
      Put (Item  => Long_Integer'Value (Argument (Index)),
           Width => 0,
           Base  => 2);
      New_Line;
   end loop;
end To_Binary;
