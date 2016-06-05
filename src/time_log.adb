with Ada.Calendar.Formatting;

package body Time_Log is
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
                       Comments   => T.Comments,
                       Time_Spent => T.Time_Spent);
            end if;
         end;

         Task_Maps.Next (Cursor);
      end loop;
   end Accumulate;

   procedure Append
     (Tasks      : in out Task_Maps.Map;
      Task_ID    : in     Ada.Strings.Unbounded.Unbounded_String;
      Comments   : in     Comment_Sets.Set;
      Time_Spent : in     Actual_Duration) is
      use type Comment_Sets.Set;
      Accumulated : Task_Type;
   begin
      if Tasks.Contains (Task_ID) then
         Accumulated := Tasks.Element (Task_ID);
         Accumulated.Time_Spent := Accumulated.Time_Spent + Time_Spent;
         Accumulated.Comments   := Accumulated.Comments or Comments;
         Tasks.Replace (Key      => Task_ID,
                        New_Item => Accumulated);
      else
         Tasks.Insert (Key      => Task_ID,
                       New_Item => (Title      => Task_ID,
                                    Time_Spent => Time_Spent,
                                    Comments   => Comments));
      end if;
   end Append;

   procedure Append
     (Tasks      : in out Task_Maps.Map;
      Task_ID    : in     Ada.Strings.Unbounded.Unbounded_String;
      Comment    : in     Ada.Strings.Unbounded.Unbounded_String;
      Time_Spent : in     Actual_Duration) is
      use Ada.Strings.Unbounded;
      use Comment_Sets;
   begin
      if Length (Comment) > 0 then
         Append (Tasks      => Tasks,
                 Task_ID    => Task_ID,
                 Comments   => To_Set (Comment),
                 Time_Spent => Time_Spent);
      else
         Append (Tasks      => Tasks,
                 Task_ID    => Task_ID,
                 Comments   => Empty_Set,
                 Time_Spent => Time_Spent);
      end if;
   end Append;

   function Customer (Item : in Task_Type) return String is
      pragma Unreferenced (Item); --  TO FIX!
   begin
      return "ABENA (hard-coded)";
   end Customer;

   function Now return Standard.Time_Of_Day.Instance is
      use Ada.Calendar;
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Time_Of_Day : Day_Duration;
   begin
      Split (Date    => Clock,
             Year    => Year,
             Month   => Month,
             Day     => Day,
             Seconds => Time_Of_Day);
      return Result : Standard.Time_Of_Day.Instance := (Hour   => 0,
                                                        Minute => 0)
      do
         while Time_Of_Day >= 3600.0 loop
            Result.Hour := Result.Hour + 1;
            Time_Of_Day := Time_Of_Day - 3600.0;
         end loop;

         while Time_Of_Day >= 60.0 loop
            Result.Minute := Result.Minute + 1;
            Time_Of_Day   := Time_Of_Day - 60.0;
         end loop;
      end return;
   end Now;

   function Today return Standard.Date.Instance is
      use Ada.Calendar;
      Year        : Year_Number;
      Month       : Month_Number;
      Day         : Day_Number;
      Time_Of_Day : Day_Duration;
   begin
      Split (Date    => Clock,
             Year    => Year,
             Month   => Month,
             Day     => Day,
             Seconds => Time_Of_Day);
      return (Year  => Year,
              Month => Month,
              Day   => Day);
   end Today;

   function Weekend (Date : in Standard.Date.Instance) return Boolean is
      use Ada.Calendar.Formatting;
      use type Standard.Date.Instance;
   begin
      return Day_Of_Week (+Date) in Saturday .. Sunday;
   end Weekend;

   function Workday (Tags : in Tag_Sets.Set) return Boolean is
      use Ada.Strings.Unbounded;
   begin
      return Tags.Contains (To_Unbounded_String ("Arbejdsdag"));
   end Workday;
end Time_Log;
