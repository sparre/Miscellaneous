procedure Get_And_Check (File  : in     Ada.Text_IO.File_Type;
                         Valid : in     Character) is
   Item : Character;
begin
   Ada.Text_IO.Get (File => File,
                    Item => Item);
   if Item /= Valid then
      raise Ada.Text_IO.Data_Error
        with "'" & Valid & "' expected.  Got '" & Item & "'.";
   end if;
end Get_And_Check;
