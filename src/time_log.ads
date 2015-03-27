with Ada.Containers.Indefinite_Hashed_Maps,
     Ada.Containers.Indefinite_Hashed_Sets,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Hash;

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
   function Workday (Tags : in Tag_Sets.Set) return Boolean;

   function Today return Standard.Date.Instance;
   function Now   return Standard.Time_Of_Day.Instance;
end Time_Log;
