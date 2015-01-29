with Ada.Calendar.Formatting,
     Ada.Characters.Latin_1,
     Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Containers.Indefinite_Hashed_Sets,
     Ada.Integer_Text_IO,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Hash,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Text_IO;

with Date,
     Get_And_Check,
     Time_Of_Day;

procedure Summarise_Time_Spent_On_Tasks is
   type Line_Type is (Day_Class, Task_Begin, Task_End);
   type Line (Kind : Line_Type := Day_Class) is
      record
         Date : Standard.Date.Instance;
         case Kind is
            when Day_Class =>
               Class : Ada.Strings.Unbounded.Unbounded_String;
            when Task_Begin | Task_End =>
               Time    : Time_Of_Day.Instance;
               Task_ID : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   subtype Actual_Duration is Duration range 0.0 .. Duration'Last;

   type Task_Type is
      record
         Title      : Ada.Strings.Unbounded.Unbounded_String;
         Time_Spent : Actual_Duration := 0.0;
      end record;

   package Task_Maps is
      new Ada.Containers.Indefinite_Hashed_Maps
            (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
             Element_Type    => Task_Type,
             Hash            => Ada.Strings.Unbounded.Hash,
             Equivalent_Keys => Ada.Strings.Unbounded."=");

   procedure Append
     (Tasks      : in out Task_Maps.Map;
      Task_ID    : in     Ada.Strings.Unbounded.Unbounded_String;
      Time_Spent : in     Actual_Duration);

   procedure Accumulate (Accumulator : in out Task_Maps.Map;
                         New_Items   : in     Task_Maps.Map);

   package Tag_Sets is
      new Ada.Containers.Indefinite_Hashed_Sets
            (Element_Type        => Ada.Strings.Unbounded.Unbounded_String,
             Hash                => Ada.Strings.Unbounded.Hash,
             Equivalent_Elements => Ada.Strings.Unbounded."=",
             "="                 => Ada.Strings.Unbounded."=");

   function Weekend (Date : in Standard.Date.Instance) return Boolean;

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Line);
   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Line);

   procedure Summarise_Day (File        : in     Ada.Text_IO.File_Type;
                            First_Line  : in     Line;
                            Tags        :    out Tag_Sets.Set;
                            Tasks       :    out Task_Maps.Map;
                            End_Of_File :    out Boolean;
                            Last_Line   :    out Line);

   procedure Put_Summary (Date       : in     Standard.Date.Instance;
                          Tags       : in     Tag_Sets.Set;
                          Time_Spent : in     Task_Maps.Map);
   procedure Put_Summary (Time_Spent : in     Task_Maps.Map);

   procedure Accumulate (Accumulator : in out Task_Maps.Map;
                         New_Items   : in     Task_Maps.Map) is
      Cursor : Task_Maps.Cursor := New_Items.First;
   begin
      while Task_Maps.Has_Element (Cursor) loop
         declare
            T : Task_Type renames Task_Maps.Element (Cursor);
         begin
            if T.Time_Spent > 0.0 then
               Append (Tasks      => Accumulator,
                       Task_ID    => T.Title,
                       Time_Spent => T.Time_Spent);
            end if;
         end;

         Task_Maps.Next (Cursor);
      end loop;
   end Accumulate;

   procedure Append
     (Tasks      : in out Task_Maps.Map;
      Task_ID    : in     Ada.Strings.Unbounded.Unbounded_String;
      Time_Spent : in     Actual_Duration) is
      Accumulated : Task_Type;
   begin
      if Tasks.Contains (Task_ID) then
         Accumulated := Tasks.Element (Task_ID);
         Accumulated.Time_Spent := Accumulated.Time_Spent + Time_Spent;
         Tasks.Replace (Key      => Task_ID,
                        New_Item => Accumulated);
      else
         Tasks.Insert (Key      => Task_ID,
                       New_Item => (Title      => Task_ID,
                                    Time_Spent => Time_Spent));
      end if;
   end Append;

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

   procedure Put_Summary (Date       : in     Standard.Date.Instance;
                          Tags       : in     Tag_Sets.Set;
                          Time_Spent : in     Task_Maps.Map) is
   begin
      Put_Tags :
      declare
         use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
         use Standard.Date;
         Cursor : Tag_Sets.Cursor := Tags.First;
      begin
         while Tag_Sets.Has_Element (Cursor) loop
            Put      (Standard_Output, Date);
            Put      (Standard_Output, Ada.Characters.Latin_1.HT);
            Put      (Standard_Output, "#");
            Put      (Standard_Output, Ada.Characters.Latin_1.HT);
            Put_Line (Standard_Output, Tag_Sets.Element (Cursor));

            Tag_Sets.Next (Cursor);
         end loop;
      end Put_Tags;

      Put_Tasks :
      declare
         use Ada.Integer_Text_IO, Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
         use Standard.Date;
         Cursor : Task_Maps.Cursor := Time_Spent.First;
      begin
         while Task_Maps.Has_Element (Cursor) loop
            declare
               T : Task_Type renames Task_Maps.Element (Cursor);
            begin
               if T.Time_Spent > 0.0 then
                  Put      (Standard_Output, Date);
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put      (Standard_Output, Integer (T.Time_Spent / 60.0));
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put_Line (Standard_Output, T.Title);
               end if;
            end;

            Task_Maps.Next (Cursor);
         end loop;
      end Put_Tasks;
   end Put_Summary;

   procedure Put_Summary (Time_Spent : in     Task_Maps.Map) is
      use Ada.Integer_Text_IO, Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
      use Standard.Date;
      Cursor : Task_Maps.Cursor := Time_Spent.First;
   begin
      while Task_Maps.Has_Element (Cursor) loop
         declare
            T : Task_Type renames Task_Maps.Element (Cursor);
         begin
            if T.Time_Spent > 0.0 then
               Put      (Standard_Output, '#');
               Put      (Standard_Output, Ada.Characters.Latin_1.HT);
               Put      (Standard_Output, Integer (T.Time_Spent / 60.0));
               Put      (Standard_Output, Ada.Characters.Latin_1.HT);
               Put_Line (Standard_Output, T.Title);
            end if;
         end;

         Task_Maps.Next (Cursor);
      end loop;
   end Put_Summary;

   procedure Summarise_Day (File        : in     Ada.Text_IO.File_Type;
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
                  Append (Tasks      => Tasks,
                          Task_ID    => Previous.Task_ID,
                          Time_Spent => Current.Time - Previous.Time);
               end if;
            when Task_End =>
               if Previous.Kind = Task_Begin and
                  Previous.Task_ID = Current.Task_ID
               then
                  Append (Tasks      => Tasks,
                          Task_ID    => Previous.Task_ID,
                          Time_Spent => Current.Time - Previous.Time);
               else
                  raise Constraint_Error
                    with "Inconsistent line sequence.";
               end if;
         end case;
      end loop;

      if Previous.Kind = Task_Begin then
         raise Constraint_Error
           with "Inconsistent line sequence.";
      end if;

      if Tags.Is_Empty then
         Tags.Insert
           (Ada.Strings.Unbounded.To_Unbounded_String ("Arbejdsdag"));
      end if;

      End_Of_File := False;
      Last_Line := Current;
   exception
      when Ada.Text_IO.End_Error =>
         End_Of_File := True;
      when others =>
         Put (File => Ada.Text_IO.Standard_Error, Item => Previous);
         Put (File => Ada.Text_IO.Standard_Error, Item => Current);
         raise;
   end Summarise_Day;

   function Weekend (Date : in Standard.Date.Instance) return Boolean is
      use Ada.Calendar.Formatting;
      use type Standard.Date.Instance;
   begin
      return Day_Of_Week (+Date) in Saturday .. Sunday;
   end Weekend;

   use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;

   Daily_Tags        : Tag_Sets.Set;
   Daily_Tasks       : Task_Maps.Map;
   Current, Next     : Line;
   End_Of_File       : Boolean := False;
   Accumulated_Tasks : Task_Maps.Map;
begin
   Get (File => Standard_Input, Item => Next);
   while not End_Of_File loop
      Current := Next;
      Summarise_Day (File        => Standard_Input,
                     First_Line  => Current,
                     Tags        => Daily_Tags,
                     Tasks       => Daily_Tasks,
                     End_Of_File => End_Of_File,
                     Last_Line   => Next);

      Put_Summary (Date       => Current.Date,
                   Tags       => Daily_Tags,
                   Time_Spent => Daily_Tasks);

      Accumulate (Accumulator => Accumulated_Tasks,
                  New_Items   => Daily_Tasks);
   end loop;

   Put_Summary (Time_Spent => Accumulated_Tasks);
end Summarise_Time_Spent_On_Tasks;
