with Ada.Characters.Latin_1,
     Ada.Environment_Variables,
     Ada.Integer_Text_IO,
     Ada.Strings.Fixed,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Text_IO;

with Date,
     Time_Log,
     Time_Log.Text_IO,
     Zero_Filled_2_Digit_Image;

procedure Generate_Daily_Timesheet is
   function Hours_And_Minutes (Seconds : in Duration) return String;

   procedure Put_Month (File : in     Ada.Text_IO.File_Type;
                        Date : in     Standard.Date.Instance);
   procedure Put_Name  (File : in     Ada.Text_IO.File_Type);

   procedure Put_Header  (Date       : in     Standard.Date.Instance);
   procedure Put_Summary (First_Day  : in out Boolean;
                          Date       : in     Standard.Date.Instance;
                          Time_Spent : in     Time_Log.Task_Maps.Map);
   procedure Put_Summary (Time_Spent : in     Time_Log.Task_Maps.Map);

   function Hours_And_Minutes (Seconds : in Duration) return String is
      use Ada.Strings, Ada.Strings.Fixed;
      Minutes : constant Natural := Natural (Seconds / 60.0);
      Hours   : constant Natural := Minutes / 60;
   begin
      return
        Trim (Natural'Image (Hours), Both) & ":" &
        Zero_Filled_2_Digit_Image (Minutes - Hours * 60);
   end Hours_And_Minutes;

   procedure Put_Header  (Date : in     Standard.Date.Instance) is
      use Ada.Text_IO;
   begin
      Put_Month (Standard_Output, Date => Date);
      Put_Name  (Standard_Output);
      New_Line  (Standard_Output);
      Put_Line  (Standard_Output, "Dato/Kunde/Opgave/Tid/Bemærkning");
      New_Line  (Standard_Output);
   end Put_Header;

   procedure Put_Month (File : in     Ada.Text_IO.File_Type;
                        Date : in     Standard.Date.Instance) is
      use Ada.Integer_Text_IO, Ada.Text_IO;
   begin
      Put      (File, Zero_Filled_2_Digit_Image (Date.Month));
      Put      (File, '-');
      Put      (File, Date.Year,  Width => 4);
      New_Line (File);
   end Put_Month;

   procedure Put_Name  (File : in     Ada.Text_IO.File_Type) is
      use Ada.Text_IO;
      Name : constant String := "FULL_NAME";
   begin
      if Ada.Environment_Variables.Exists (Name) then
         Put_Line (File, Ada.Environment_Variables.Value (Name));
      else
         raise Constraint_Error
           with "Environment variable '" & Name & "' not set.";
      end if;
   end Put_Name;

   procedure Put_Summary (First_Day  : in out Boolean;
                          Date       : in     Standard.Date.Instance;
                          Time_Spent : in     Time_Log.Task_Maps.Map) is
   begin
      if First_Day then
         Put_Header (Date => Date);
         First_Day := False;
      end if;

      Put_Tasks :
      declare
         use Ada.Integer_Text_IO, Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
         use Standard.Date, Time_Log.Text_IO;
         Cursor : Time_Log.Task_Maps.Cursor := Time_Spent.First;
      begin
         while Time_Log.Task_Maps.Has_Element (Cursor) loop
            declare
               T : Time_Log.Task_Type renames
                     Time_Log.Task_Maps.Element (Cursor);
            begin
               if T.Time_Spent > 0.0 then
                  Put      (Standard_Output, Date.Day, Width => 2);
                  Put      (Standard_Output, ".");
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put      (Standard_Output, T.Customer);
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put      (Standard_Output, T.Title);
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put      (Standard_Output, Hours_And_Minutes (T.Time_Spent));
                  Put      (Standard_Output, Ada.Characters.Latin_1.HT);
                  Put      (Standard_Output, T.Comments);
                  New_Line (Standard_Output);
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

   First_Day         : Boolean := True;
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

      Put_Summary (First_Day  => First_Day,
                   Date       => Current.Date,
                   Time_Spent => Daily_Tasks);

      Accumulate (Accumulator => Accumulated_Tasks,
                  New_Items   => Daily_Tasks);
   end loop;

   Put_Summary (Time_Spent => Accumulated_Tasks);
end Generate_Daily_Timesheet;
