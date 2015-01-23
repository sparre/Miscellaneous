with Ada.Characters.Latin_1,
     Ada.Strings.Unbounded,
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

   procedure Get (File : in     Ada.Text_IO.File_Type;
                  Item :    out Line);
   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Line);

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

   use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;

   Item : Line;
begin
   loop
      Get (File => Standard_Input,  Item => Item);
      Put (File => Standard_Output, Item => Item);
   end loop;
exception
   when End_Error =>
      null;
end Summarise_Time_Spent_On_Tasks;
