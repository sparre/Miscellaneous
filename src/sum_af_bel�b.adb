with Ada.Text_IO;

procedure Sum_Af_Beløb is
   type Beløb is delta 0.01 digits 11 range -10_000_000.00 .. +10_000_000.00;
   package Beløb_IO is new Ada.Text_IO.Decimal_IO (Beløb);

   use Ada.Text_IO, Beløb_IO;

   Sum   : Beløb := 0.0;
   Value : Beløb;
begin
   loop
      Get (Value);
      Sum := Sum + Value;
   end loop;
exception
   when End_Error =>
      Put (Sum);
      New_Line;
end Sum_Af_Beløb;
