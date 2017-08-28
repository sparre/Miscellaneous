--  to encipher: ~$ ./bin/caesar --encipher 3 'some text to encipher'
--  to decipher: ~$ ./bin/caesar --decipher 3 'some text to decipher'

with Ada.Exceptions;
with Ada.Text_IO;
with Ada.Command_Line;

procedure Caesar is

   Bad_Arguments : exception;

   type Modes is (Encipher, Decipher);

   function Selected_Mode return Modes;

   function Offset return Positive;

   function Text return String;

   procedure Shift (Item     : in out Character;
                    Distance : in     Integer);

   procedure Shift (Item     : in out String;
                    Distance : in     Integer);

   function Shift (Item     : in     String;
                   Distance : in     Integer) return String;

   procedure Put_Help (File : in     Ada.Text_IO.File_Type);

   function Offset return Positive is
   begin
      if Ada.Command_Line.Argument_Count /= 3 then
         raise Bad_Arguments with "Missing arguments.";
      end if;

      return Positive'Value (Ada.Command_Line.Argument (2));
   exception
      when Constraint_Error =>
         raise Bad_Arguments with "Offset should be a positive integer.";
   end Offset;

   procedure Put_Help (File : in     Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
   begin
      Put_Line (File => File,
                Item => "Usage:");
      Put_Line (File => File,
                Item => "   " & Ada.Command_Line.Command_Name &
                        " --encipher 3 'some text to encipher'");
      Put_Line (File => File,
                Item => "   " & Ada.Command_Line.Command_Name &
                        " --decipher 3 'some text to decipher'");
   end Put_Help;

   function Selected_Mode return Modes is
   begin
      if Ada.Command_Line.Argument_Count /= 3 then
         raise Bad_Arguments with "Missing arguments.";
      end if;

      declare
         Mode_String : String renames Ada.Command_Line.Argument (1);
      begin
         if Mode_String'Length < 3 or else
            Mode_String (Mode_String'First .. Mode_String'First + 1) /= "--"
         then
            raise Bad_Arguments with "Bad mode argument.";
         end if;

         return Modes'Value
                  (Mode_String (Mode_String'First + 2 .. Mode_String'Last));
      exception
         when Constraint_Error =>
            raise Bad_Arguments
              with "'" & Mode_String & "' is not a valid mode.";
      end;
   end Selected_Mode;

   procedure Shift (Item     : in out Character;
                    Distance : in     Integer) is
   begin
      for Index in 1 .. Distance loop
         case Item is
            when 'z' =>                     Item := 'a';
            when 'Z' =>                     Item := 'A';
            when 'a' .. 'y' | 'A' .. 'Y' => Item := Character'Succ (Item);
            when others =>                  null;
         end case;
      end loop;

      for Index in Distance .. -1 loop
         case Item is
            when 'a' =>                     Item := 'z';
            when 'A' =>                     Item := 'Z';
            when 'b' .. 'z' | 'B' .. 'Z' => Item := Character'Pred (Item);
            when others =>                  null;
         end case;
      end loop;
   end Shift;

   procedure Shift (Item     : in out String;
                    Distance : in     Integer) is
   begin
      for Character of Item loop
         Shift (Item     => Character,
                Distance => Distance);
      end loop;
   end Shift;

   function Shift (Item     : in     String;
                   Distance : in     Integer) return String is
      Buffer : String := Item;
   begin
      Shift (Item     => Buffer,
             Distance => Distance);
      return Buffer;
   end Shift;

   function Text return String is
   begin
      if Ada.Command_Line.Argument_Count /= 3 then
         raise Bad_Arguments with "Missing arguments.";
      end if;

      return Ada.Command_Line.Argument (3);
   end Text;

   use Ada.Text_IO;
begin
   case Selected_Mode is
      when Encipher =>
         Put_Line ("Encoding");
         Put_Line (Shift (Item     => Text,
                          Distance => Offset));
      when Decipher =>
         Ada.Text_IO.Put_Line ("Decoding");
         Put_Line (Shift (Item     => Text,
                          Distance => -Offset));
   end case;
exception
   when E : Bad_Arguments =>
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => Ada.Exceptions.Exception_Message (E));
      Put_Help (File => Ada.Text_IO.Standard_Error);
end Caesar;
