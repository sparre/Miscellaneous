with Ada.Characters.Latin_1,
     Ada.Strings.Unbounded.Text_IO;

with Get_And_Check;

package body Time_Log.Text_IO is
   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Line) is
      use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
      use Date, Time_Of_Day;
      Date   : Standard.Date.Instance;
      Marker : Character;
      EOL    : Boolean;
      Time   : Time_Of_Day.Instance;
      Sign   : Character;
      Label  : Ada.Strings.Unbounded.Unbounded_String;
   begin
      Get           (File, Date);
      Get_And_Check (File, Ada.Characters.Latin_1.HT);
      Look_Ahead    (File, Marker, EOL);

      if EOL then
         raise Data_Error with "Time field missing.";
      elsif Marker = '#' then
         Get_And_Check (File, '#');
         Get_And_Check (File, Ada.Characters.Latin_1.HT);
         Get_And_Check (File, '#');
         Get_And_Check (File, Ada.Characters.Latin_1.HT);
         Get_Line      (File, Label);

         Item := (Kind  => Day_Class,
                  Date  => Date,
                  Class => Label);
      elsif Marker in '0' .. '9' then
         Get           (File, Time);
         Get_And_Check (File, Ada.Characters.Latin_1.HT);
         Get           (File, Sign);
         Get_And_Check (File, Ada.Characters.Latin_1.HT);
         Get_Line      (File, Label);

         case Sign is
            when '+' =>
               Item := (Kind    => Task_Begin,
                        Date    => Date,
                        Time    => Time,
                        Task_ID => Label);
            when '-' =>
               Item := (Kind    => Task_End,
                        Date    => Date,
                        Time    => Time,
                        Task_ID => Label);
            when others =>
               raise Data_Error with "'" & Sign & "' is not a valid sign.";
         end case;
      else
         raise Data_Error
           with "Got '" & Marker & "'.  '#' or time of day expected.";
      end if;
   exception
      when Data_Error =>
         Put      (File => Standard_Error,
                   Item => "Remainder of line: """);
         Put      (File => Standard_Error,
                   Item => Get_Line (Standard_Input));
         Put_Line (File => Standard_Error,
                   Item => """");
         raise;
   end Get;

   procedure Get_Day (File        : in     Ada.Text_IO.File_Type;
                      First_Line  : in     Line;
                      Tags        :    out Tag_Sets.Set;
                      Tasks       :    out Task_Maps.Map;
                      End_Of_File :    out Boolean;
                      Last_Line   :    out Line) is
      use type Ada.Strings.Unbounded.Unbounded_String;
      use type Date.Instance, Time_Of_Day.Instance;
      Date              : Standard.Date.Instance renames First_Line.Date;
      Previous, Current : Line;
   begin
      Tags.Clear;
      Tasks.Clear;

      if Weekend (Date) then
         Tags.Insert (Ada.Strings.Unbounded.To_Unbounded_String ("Weekend"));
      end if;

      if First_Line.Kind = Day_Class then
         if not Tags.Contains (First_Line.Class) then
            Tags.Insert (First_Line.Class);
         end if;
      elsif First_Line.Kind = Task_End then
         raise Constraint_Error
           with "Inconsistent line sequence.";
      end if;

      Current := First_Line;
      loop
         Previous := Current;
         Get (File => File, Item => Current);

         exit when Current.Date /= Date;

         case Current.Kind is
            when Day_Class =>
               if Previous.Kind = Task_End or Previous.Kind = Day_Class then
                  if not Tags.Contains (Current.Class) then
                     Tags.Insert (Current.Class);
                  end if;
               else
                  raise Constraint_Error
                    with "Inconsistent line sequence.";
               end if;
            when Task_Begin =>
               if Previous.Kind = Task_Begin then
                  if Current.Time < Previous.Time then
                     raise Constraint_Error
                       with "Incorrectly ordered lines in the time log.";
                  else
                     Append (Tasks      => Tasks,
                             Task_ID    => Previous.Task_ID,
                             Time_Spent => Current.Time - Previous.Time);
                  end if;
               end if;
            when Task_End =>
               if Previous.Kind = Task_Begin and
                  Previous.Task_ID = Current.Task_ID
               then
                  if Current.Time < Previous.Time then
                     raise Constraint_Error
                       with "Incorrectly ordered lines in the time log.";
                  else
                     Append (Tasks      => Tasks,
                             Task_ID    => Previous.Task_ID,
                             Time_Spent => Current.Time - Previous.Time);
                  end if;
               else
                  raise Constraint_Error
                    with "Inconsistent line sequence.";
               end if;
         end case;
      end loop;

      if Previous.Kind = Task_Begin then
         if Previous.Date = Today and then Now >= Previous.Time then
            Append (Tasks      => Tasks,
                    Task_ID    => Previous.Task_ID,
                    Time_Spent => Now - Previous.Time);
         else
            raise Constraint_Error
              with "Inconsistent line sequence.";
         end if;
      end if;

      if Tags.Is_Empty then
         Tags.Insert
           (Ada.Strings.Unbounded.To_Unbounded_String ("Arbejdsdag"));
      end if;

      End_Of_File := False;
      Last_Line := Current;
   exception
      when Ada.Text_IO.End_Error =>
         if Previous.Kind = Task_Begin and then
            Previous.Date = Today and then
            Previous.Time <= Now
         then
            Append (Tasks      => Tasks,
                    Task_ID    => Previous.Task_ID,
                    Time_Spent => Now - Previous.Time);
         end if;

         if Tags.Is_Empty then
            Tags.Insert
              (Ada.Strings.Unbounded.To_Unbounded_String ("Arbejdsdag"));
         end if;

         End_Of_File := True;
      when others =>
         Put (File => Ada.Text_IO.Standard_Error, Item => Previous);
         Put (File => Ada.Text_IO.Standard_Error, Item => Current);
         raise;
   end Get_Day;

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Line) is
      use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
      use Date, Time_Of_Day;
   begin
      case Item.Kind is
         when Day_Class =>
            Put      (File, Item.Date);
            Put      (File, Ada.Characters.Latin_1.HT);
            Put      (File, '#');
            Put      (File, Ada.Characters.Latin_1.HT);
            Put      (File, '#');
            Put      (File, Ada.Characters.Latin_1.HT);
            Put_Line (File, Item.Class);
         when Task_Begin =>
            Put      (File, Item.Date);
            Put      (File, Ada.Characters.Latin_1.HT);
            Put      (File, Item.Time);
            Put      (File, Ada.Characters.Latin_1.HT);
            Put      (File, '+');
            Put      (File, Ada.Characters.Latin_1.HT);
            Put_Line (File, Item.Task_ID);
         when Task_End =>
            Put      (File, Item.Date);
            Put      (File, Ada.Characters.Latin_1.HT);
            Put      (File, Item.Time);
            Put      (File, Ada.Characters.Latin_1.HT);
            Put      (File, '-');
            Put      (File, Ada.Characters.Latin_1.HT);
            Put_Line (File, Item.Task_ID);
      end case;
   end Put;
end Time_Log.Text_IO;
