with
  Ada.Characters.Latin_1,
  Ada.Integer_Text_IO,
  Ada.Strings.Unbounded,
  Ada.Text_IO;

procedure Hex_Colours_To_PPM_Colour_Table is
   package Latin_1 renames Ada.Characters.Latin_1;
   use Ada.Integer_Text_IO, Ada.Strings.Unbounded, Ada.Text_IO;

   Colour_Code : String (1 .. 6);
   Table       : Ada.Strings.Unbounded.Unbounded_String;
   Buffer           : String := "### ### ###";
   Red, Green, Blue : Natural;
   Counter          : Natural := 0;
begin
   loop
      Get (Colour_Code);
      Skip_Line;

      Red   := Natural'Value ("16#" & Colour_Code (1 .. 2) & "#");
      Green := Natural'Value ("16#" & Colour_Code (3 .. 4) & "#");
      Blue  := Natural'Value ("16#" & Colour_Code (5 .. 6) & "#");

      Put (To => Buffer (1 ..  3), Item => Red);
      Put (To => Buffer (5 ..  7), Item => Green);
      Put (To => Buffer (9 .. 11), Item => Blue);

      Append (Table, Buffer & Latin_1.LF);
      Counter := Counter + 1;
   end loop;
exception
   when End_Error =>
      Put_Line ("P3");
      Put (Counter, Width => 0);
      Put_Line (" 1");
      Put_Line ("255");
      Put      (To_String (Table));
end Hex_Colours_To_PPM_Colour_Table;
