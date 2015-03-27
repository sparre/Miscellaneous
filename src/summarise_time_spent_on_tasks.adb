with Ada.Characters.Latin_1,
     Ada.Integer_Text_IO,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Text_IO;

with Date,
     Time_Log,
     Time_Log.Text_IO;

procedure Summarise_Time_Spent_On_Tasks is
   procedure Put_Summary (Date       : in     Standard.Date.Instance;
                          Tags       : in     Time_Log.Tag_Sets.Set;
                          Time_Spent : in     Time_Log.Task_Maps.Map);
   procedure Put_Summary (Time_Spent : in     Time_Log.Task_Maps.Map);

   procedure Put_Summary (Date       : in     Standard.Date.Instance;
                          Tags       : in     Time_Log.Tag_Sets.Set;
                          Time_Spent : in     Time_Log.Task_Maps.Map) is
   begin
      Put_Tags :
      declare
         use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
         use Standard.Date;
         Cursor : Time_Log.Tag_Sets.Cursor := Tags.First;
      begin
         while Time_Log.Tag_Sets.Has_Element (Cursor) loop
            Put      (Standard_Output, Date);
            Put      (Standard_Output, Ada.Characters.Latin_1.HT);
            Put      (Standard_Output, "#");
            Put      (Standard_Output, Ada.Characters.Latin_1.HT);
            Put_Line (Standard_Output, Time_Log.Tag_Sets.Element (Cursor));

            Time_Log.Tag_Sets.Next (Cursor);
         end loop;
      end Put_Tags;

      Put_Tasks :
      declare
         use Ada.Integer_Text_IO, Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
         use Standard.Date;
         Cursor : Time_Log.Task_Maps.Cursor := Time_Spent.First;
      begin
         while Time_Log.Task_Maps.Has_Element (Cursor) loop
            declare
               T : Time_Log.Task_Type renames
                     Time_Log.Task_Maps.Element (Cursor);
            begin
               if T.Time_Spent > 0.0 then
                  Put      (Standard_Output, Date);
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put      (Standard_Output, Integer (T.Time_Spent / 60.0));
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put_Line (Standard_Output, T.Title);
               end if;
            end;

            Time_Log.Task_Maps.Next (Cursor);
         end loop;
      end Put_Tasks;
   end Put_Summary;

   procedure Put_Summary (Time_Spent : in     Time_Log.Task_Maps.Map) is
      use Ada.Integer_Text_IO, Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
      use Standard.Date;
      Cursor : Time_Log.Task_Maps.Cursor := Time_Spent.First;
   begin
      while Time_Log.Task_Maps.Has_Element (Cursor) loop
         declare
            T : Time_Log.Task_Type renames
                  Time_Log.Task_Maps.Element (Cursor);
         begin
            if T.Time_Spent > 0.0 then
               Put      (Standard_Output, '#');
               Put      (Standard_Output, Ada.Characters.Latin_1.HT);
               Put      (Standard_Output, Integer (T.Time_Spent / 60.0));
               Put      (Standard_Output, Ada.Characters.Latin_1.HT);
               Put_Line (Standard_Output, T.Title);
            end if;
         end;

         Time_Log.Task_Maps.Next (Cursor);
      end loop;
   end Put_Summary;

   use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
   use Time_Log, Time_Log.Text_IO;

   Daily_Tags        : Tag_Sets.Set;
   Daily_Tasks       : Task_Maps.Map;
   Current, Next     : Time_Log.Line;
   End_Of_File       : Boolean := False;
   Accumulated_Tasks : Task_Maps.Map;
begin
   Get (File => Standard_Input, Item => Next);
   while not End_Of_File loop
      Current := Next;
      Get_Day (File        => Standard_Input,
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
