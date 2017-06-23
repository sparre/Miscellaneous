with Ada.Containers.Indefinite_Ordered_Maps,
     Ada.Containers.Indefinite_Ordered_Sets,
     Ada.Strings.Unbounded;

with Date,
     Time_Of_Day;

package Time_Log is
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
               Comment : Ada.Strings.Unbounded.Unbounded_String;
         end case;
      end record;

   subtype Actual_Duration is Duration range 0.0 .. Duration'Last;

   package Comment_Sets is
      new Ada.Containers.Indefinite_Ordered_Sets
            (Element_Type        => Ada.Strings.Unbounded.Unbounded_String,
             "="                 => Ada.Strings.Unbounded."=",
             "<"                 => Ada.Strings.Unbounded."<");

   type Task_Type is tagged
      record
         Title      : Ada.Strings.Unbounded.Unbounded_String;
         Time_Spent : Actual_Duration := 0.0;
         Comments   : Comment_Sets.Set;
      end record;

   function Customer (Item : in Task_Type) return String;

   package Task_Maps is
      new Ada.Containers.Indefinite_Ordered_Maps
            (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
             Element_Type    => Task_Type,
             "<"             => Ada.Strings.Unbounded."<");

   procedure Append
     (Tasks      : in out Task_Maps.Map;
      Task_ID    : in     Ada.Strings.Unbounded.Unbounded_String;
      Comments   : in     Comment_Sets.Set;
      Time_Spent : in     Actual_Duration);

   procedure Append
     (Tasks      : in out Task_Maps.Map;
      Task_ID    : in     Ada.Strings.Unbounded.Unbounded_String;
      Comment    : in     Ada.Strings.Unbounded.Unbounded_String;
      Time_Spent : in     Actual_Duration);

   procedure Accumulate (Accumulator : in out Task_Maps.Map;
                         New_Items   : in     Task_Maps.Map);

   package Tag_Sets is
      new Ada.Containers.Indefinite_Ordered_Sets
            (Element_Type        => Ada.Strings.Unbounded.Unbounded_String,
             "="                 => Ada.Strings.Unbounded."=",
             "<"                 => Ada.Strings.Unbounded."<");

   function Weekend (Date : in Standard.Date.Instance) return Boolean;
   function Workday (Tags : in Tag_Sets.Set) return Boolean;

   function Today return Standard.Date.Instance;
   function Now   return Standard.Time_Of_Day.Instance;
end Time_Log;
