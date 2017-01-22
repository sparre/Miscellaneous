------------------------------------------------------------------------------
--
--  function Trim_Special_Characters (spec)
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

with Ada.Strings;

function Trim_Special_Characters (Source : in String;
                                  Side   : in Ada.Strings.Trim_End)
  return String;
