with Ada.Command_Line,
     Ada.Long_Long_Integer_Text_IO,
     Ada.Text_IO;

procedure Largest is
   use Ada.Command_Line,
       Ada.Long_Long_Integer_Text_IO,
       Ada.Text_IO;

   Found           : Boolean := False;
   Result, Current : Long_Long_Integer;
begin
   Get (Result); Skip_Line;
   Found := True;

   loop
      Get (Current); Skip_Line;

      Result := Long_Long_Integer'Max (Result,
                                       Current);
   end loop;
exception
   when End_Error =>
      if Found then
         Put (Result); New_Line;
      else
         Set_Exit_Status (Failure);
      end if;
end Largest;
