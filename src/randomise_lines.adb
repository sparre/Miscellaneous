with Ada.Characters.Latin_1,
     Ada.Command_Line,
     Ada.Numerics.Discrete_Random,
     Ada.Text_IO,
     System,
     System.Storage_Elements;

with POSIX,
     POSIX.IO,
     POSIX.Memory_Mapping;

procedure Randomise_Lines is
   use POSIX;
   use POSIX.IO;
   use POSIX.Memory_Mapping;
   use System.Storage_Elements;

   Text_File    : File_Descriptor;
   Text_Size    : System.Storage_Elements.Storage_Offset;
   Text_Address : System.Address;
begin
   Text_File := Open (Name => To_POSIX_String (Ada.Command_Line.Argument (1)),
                      Mode => Read_Only);
   Text_Size := Storage_Offset (File_Size (Text_File));
   Text_Address := Map_Memory (Length     => Text_Size,
                               Protection => Allow_Read,
                               Mapping    => Map_Shared,
                               File       => Text_File,
                               Offset     => 0);

   declare
      package Latin_1 renames Ada.Characters.Latin_1;
      Bit_Count       : constant Natural :=
                          Natural (Text_Size) * Storage_Element'Size;
      Character_Count : constant Natural :=
                          Bit_Count / Character'Size;

      Text : String (1 .. Character_Count);
      for Text'Address use Text_Address;
      Found_Line_Feeds : Natural := 0;
   begin
      for Index in Text'Range loop
         if Text (Index) = Latin_1.LF then
            Found_Line_Feeds := Found_Line_Feeds + 1;
         end if;
      end loop;

      declare
         subtype Line_Index is Natural range 1 .. Found_Line_Feeds;
         package Random_Lines is new Ada.Numerics.Discrete_Random (Line_Index);
         Order        : array (Line_Index) of Line_Index;
         Generator    : Random_Lines.Generator;
         Swap, Buffer : Line_Index;
         Line_Breaks  : array (0 .. Found_Line_Feeds) of Natural;
      begin
         for Line in Order'Range loop
            Order (Line) := Line;
         end loop;

         Random_Lines.Reset (Generator);

         for Line in Order'Range loop
            Swap := Random_Lines.Random (Generator);

            Buffer := Order (Line);
            Order (Line) := Order (Swap);
            Order (Swap) := Buffer;
         end loop;

         declare
            Line : Natural := 0;
         begin
            Line_Breaks (Line) := Text'First - 1;

            for Character in Text'Range loop
               if Text (Character) = Latin_1.LF then
                  Line := Line + 1;
                  Line_Breaks (Line) := Character;
               end if;
            end loop;
         end;

         for Line in Order'Range loop
            declare
               Selected : constant Line_Index := Order (Line);
               From     : constant Positive := Line_Breaks (Selected - 1) + 1;
               To       : constant Positive := Line_Breaks (Selected) - 1;
            begin
               Ada.Text_IO.Put_Line (Text (From .. To));
            end;
         end loop;
      end;
   end;

   Unmap_Memory (First  => Text_Address,
                 Length => Text_Size);
   Close (File => Text_File);
end Randomise_Lines;
