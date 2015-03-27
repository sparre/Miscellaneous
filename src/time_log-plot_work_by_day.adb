with Ada.Text_IO;

with Time_Log.Text_IO;

procedure Time_Log.Plot_Work_By_Day is

   --  Initialised : Boolean := False;
   --  Latest_Day  : Standard.Date.Instance;

   procedure Plot_Day (Date       : in     Standard.Date.Instance;
                       Workday    : in     Boolean;
                       Time_Spent : in     Task_Maps.Map);

   procedure Plot_Day (Date       : in     Standard.Date.Instance;
                       Workday    : in     Boolean;
                       Time_Spent : in     Task_Maps.Map) is
   begin
      raise Program_Error with "Plot_Day not implemented yet.";
   end Plot_Day;

   use Time_Log.Text_IO;

   Current, Next : Line;
   Daily_Tags    : Tag_Sets.Set;
   Daily_Tasks   : Task_Maps.Map;
   End_Of_File   : Boolean := False;
begin
   Get (File => Ada.Text_IO.Standard_Input,
        Item => Next);
   while not End_Of_File loop
      Current := Next;
      Get_Day (File        => Ada.Text_IO.Standard_Input,
               First_Line  => Current,
               Tags        => Daily_Tags,
               Tasks       => Daily_Tasks,
               End_Of_File => End_Of_File,
               Last_Line   => Next);

      Plot_Day (Date       => Current.Date,
                Workday    => Workday (Daily_Tags),
                Time_Spent => Daily_Tasks);
   end loop;
end Time_Log.Plot_Work_By_Day;
