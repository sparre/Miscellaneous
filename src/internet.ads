------------------------------------------------------------------------------
--
--  package Internet (spec)
--
--  This is the base package for Internet related operations.
--
------------------------------------------------------------------------------
--  Update information:
--
--  1996.07.19 (Jacob Sparre Andersen)
--    Written.
--
--  1996.08.04 (Jacob Sparre Andersen)
--    Moved everything to child packages.
--    Added functions Is_Domain_Name and Is_IP_Address.
--
--  1996.08.07 (Jacob Sparre Andersen)
--    Declared the exception Invalid_IP_Address.
--
--  (Insert additional update information above this line.)
------------------------------------------------------------------------------

package Internet is

   ---------------------------------------------------------------------------
   --  Network addresses:

   ------------------------------------------------------------------
   --  Exceptions:

   Invalid_Domain_Name : exception;
   Invalid_IP_Address  : exception;

   ------------------------------------------------------------------
   --  function Is_Domain_Name:

   function Is_Domain_Name (Item : in String) return Boolean;

   ------------------------------------------------------------------
   --  function Is_IP_Address:

   function Is_IP_Address (Item : in String) return Boolean;

   ---------------------------------------------------------------------------

end Internet;
