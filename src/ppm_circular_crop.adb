with Ada.Command_Line,
     Ada.Integer_Text_IO,
     Ada.Text_IO;

procedure PPM_Circular_Crop is
   PPM_Key : constant String := "P3";
   Key     : String (1 .. 2);

   use Ada.Command_Line, Ada.Integer_Text_IO, Ada.Text_IO;

   Depth, Width, Height : Positive;
begin
   Get (Key);

   if Key /= PPM_Key then
      Put_Line (File => Standard_Error,
                Item => "Input not a PPM file.");
      Set_Exit_Status (Failure);
      return;
   end if;

   Get (Width);
   Get (Height);

   if Width /= Height then
      Put_Line (File => Standard_Error,
                Item => "Input not a square PPM file.");
      Set_Exit_Status (Failure);
      return;
   end if;

   Get (Depth);

   Put_Line (Key);

   Put (Width,  Width => 0);
   Put (" ");
   Put (Height, Width => 0);
   New_Line;

   Put (Depth, Width => 0);
   New_Line;

   declare
      Centre           : constant Float := Float (Height + 1) * 0.5;
      Radius_Squared   : constant Float := (Centre - 0.5) ** 2;
      Delta_X_Squared  : Float;
      Delta_Y_Squared  : Float;
      Red, Green, Blue : Natural;
   begin
      for Y in reverse 1 .. Height loop
         Delta_Y_Squared := (Float (Y) - Centre) ** 2;

         for X in 1 .. Width loop
            Delta_X_Squared := (Float (X) - Centre) ** 2;

            Get (Red);
            Get (Green);
            Get (Blue);

            if Delta_X_Squared + Delta_Y_Squared > Radius_Squared then
               Red   := Depth;
               Green := Depth;
               Blue  := Depth;
            end if;

            Put (Red, Width => 0);
            Put (" ");
            Put (Green, Width => 0);
            Put (" ");
            Put (Blue, Width => 0);
            New_Line;
         end loop;
      end loop;
   end;
end PPM_Circular_Crop;
