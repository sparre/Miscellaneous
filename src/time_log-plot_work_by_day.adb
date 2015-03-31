with Ada.Calendar,
     Ada.Integer_Text_IO,
     Ada.Strings.Unbounded,
     Ada.Text_IO;

with Time_Log.Text_IO;

procedure Time_Log.Plot_Work_By_Day is
   Work_Per_Day : constant Duration := 6.0 * 60.0 * 60.0;

   type Day_Info is
      record
         Blue, Green : Ada.Calendar.Day_Duration;
      end record;

   Initialised : Boolean := False;
   Month       : Standard.Date.Instance;
   Data        : array (Ada.Calendar.Day_Number) of Day_Info;

   function "+" (Item : in String)
                return Ada.Strings.Unbounded.Unbounded_String
     renames Ada.Strings.Unbounded.To_Unbounded_String;

   function Work (Time_Spent : in Task_Maps.Map)
                 return Ada.Calendar.Day_Duration;

   procedure Plot_Day (Date       : in     Standard.Date.Instance;
                       Workday    : in     Boolean;
                       Time_Spent : in     Task_Maps.Map);
   procedure Plot_Data;

   procedure Plot_Data is
      Height : constant := 200;
      Width  : constant := 20;

      function Scale (Item : in Ada.Calendar.Day_Duration) return Natural;
      function Scale (Item : in Ada.Calendar.Day_Duration) return Natural is
      begin
         return Natural (Duration'(Item) * Height) / (24 * 60 * 60);
      end Scale;

      use Ada.Integer_Text_IO, Ada.Text_IO;
   begin
      Put_Line ("P3");
      Put (Width * Data'Length);
      Put (" ");
      Put (Height);
      New_Line;
      Put_Line ("255");

      for Y in reverse 1 .. Height loop
         for M in Data'Range loop
            if Y <= Scale (Data (M).Blue) then
               for N in 1 .. Width loop
                  Put (" 0 0 255");
               end loop;
            elsif Y <= Scale (Data (M).Green) then
               for N in 1 .. Width loop
                  Put (" 0 255 0");
               end loop;
            else
               for N in 1 .. Width loop
                  Put (" 0 0 0");
               end loop;
            end if;

            New_Line;
         end loop;
      end loop;
   end Plot_Data;

   procedure Plot_Day (Date       : in     Standard.Date.Instance;
                       Workday    : in     Boolean;
                       Time_Spent : in     Task_Maps.Map) is
   begin
      if Initialised then
         if Month.Year = Date.Year and Month.Month = Date.Month then
            null; --  Same month.
         else
            Plot_Data;
            Month := Date;
            Data  := (others => (Blue | Green => 0.0));
         end if;
      else
         Month       := Date;
         Data        := (others => (Blue | Green => 0.0));
         Initialised := True;
      end if;

      if Workday then
         Data (Date.Day) :=
           (Blue  => Duration'Min (Work_Per_Day,
                                   Work (Time_Spent)),
            Green => Work (Time_Spent));
      else
         Data (Date.Day) := (Blue  => 0.0,
                             Green => Work (Time_Spent));
      end if;
   end Plot_Day;

   function Work (Time_Spent : in Task_Maps.Map)
                 return Ada.Calendar.Day_Duration is
      Pauser : Ada.Calendar.Day_Duration := 0.0;
      Cursor : Task_Maps.Cursor := Time_Spent.First;
   begin
      return Sum : Ada.Calendar.Day_Duration := 0.0 do
         while Task_Maps.Has_Element (Cursor) loop
            declare
               use type Ada.Strings.Unbounded.Unbounded_String;
               T : Task_Type renames Task_Maps.Element (Cursor);
            begin
               Sum := Sum + T.Time_Spent;

               if T.Title = +"Frokost" or else
                  T.Title = +"Skærmpause" or else
                  T.Title = +"coop.dk/LEGO"
               then
                  Pauser := Pauser + T.Time_Spent;
               end if;
            end;

            Task_Maps.Next (Cursor);
         end loop;
      end return;
   end Work;

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

   if Initialised then
      Plot_Data;
   end if;
end Time_Log.Plot_Work_By_Day;
