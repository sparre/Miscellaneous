with Ada.Text_IO;

procedure Sum_Of_Amounts is
   type Amount is delta 0.01 digits 11 range -10_000_000.00 .. +10_000_000.00;
   package Amount_IO is new Ada.Text_IO.Decimal_IO (Amount);

   use Ada.Text_IO, Amount_IO;

   Sum   : Amount := 0.0;
   Value : Amount;
begin
   loop
      Get (Value);
      Sum := Sum + Value;
   end loop;
exception
   when End_Error =>
      Put (Sum);
      New_Line;
end Sum_Of_Amounts;
