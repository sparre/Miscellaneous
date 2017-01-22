with
  Ada.Strings.Maps;

package body Internet is

   ---------------------------------------------------------------------------
   --  Network addresses:

   ------------------------------------------------------------------
   --  function Is_Domain_Name:

   function Is_Domain_Name (Item : in String) return Boolean is

      use Ada.Strings.Maps;

      Allowed : constant Character_Set :=
        To_Set (Character_Range'(Low => 'a', High => 'z')) or
        To_Set (Character_Range'(Low => '0', High => '9')) or
        To_Set ("-.");

   begin
      for Index in Item'Range loop
         if not Is_In (Item (Index), Allowed) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Domain_Name;

   ------------------------------------------------------------------
   --  function Is_IP_Address:

   function Is_IP_Address (Item : in String) return Boolean is
      use Ada.Strings.Maps;

      Allowed : constant Character_Set :=
        To_Set (Character_Range'(Low => '0', High => '9')) or
        To_Set (".");

      Dot_Count : Natural := 0;
   begin
      for Index in Item'Range loop
         if not Is_In (Item (Index), Allowed) then
            return False;
         elsif Item (Index) = '.' then
            Dot_Count := Dot_Count + 1;
         end if;
      end loop;

      return Dot_Count = 3;
   end Is_IP_Address;

   ---------------------------------------------------------------------------

end Internet;
