------------------------------------------------------------------------------
--
--  function Trim_Special_Characters (body)
--
--  Trims special characters from strings.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1996.07.08 (Jacob Sparre Andersen)
--    Written.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with Ada.Strings.Maps;

function Trim_Special_Characters (Source : in String;
                                  Side   : in Ada.Strings.Trim_End)
  return String is

   use Ada.Strings;
   use Ada.Strings.Fixed;
   use Ada.Strings.Maps;

   ---------------------------------------------------------------------------
   --  Mappings:

   Special_Sequence : constant Character_Sequence :=
     To_Sequence (To_Set (Character_Range'(Low  => Character'First,
                                           High => Space)));

   Special_To_Space : constant Ada.Strings.Maps.Character_Mapping :=
     To_Mapping (From => Special_Sequence,
                 To   => Special_Sequence'Length * " ");

   ---------------------------------------------------------------------------

begin --  Trim_Special_Characters
   return Trim (Source => Translate (Source  => Source,
                                     Mapping => Special_To_Space),
                Side   => Side);
end Trim_Special_Characters;
