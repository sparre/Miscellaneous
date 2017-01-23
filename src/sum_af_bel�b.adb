with Ada.Text_IO;

procedure Sum_Af_Bel�b is
   type Bel�b is delta 0.01 digits 11 range -10_000_000.00 .. +10_000_000.00;
   package Bel�b_IO is new Ada.Text_IO.Decimal_IO (Bel�b);

   use Ada.Text_IO, Bel�b_IO;

   Sum   : Bel�b := 0.0;
   Value : Bel�b;
begin
   loop
      Get (Value);
      Sum := Sum + Value;
   end loop;
exception
   when End_Error =>
      Put (Sum);
      New_Line;
end Sum_Af_Bel�b;
