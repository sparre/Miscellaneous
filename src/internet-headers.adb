--  For processing of Internet (Mail/Usenet/...) headers.

with Ada.Characters.Handling,
     Ada.Strings.Fixed,
     Ada.Strings.Maps,
     Ada.Strings.Maps.Constants,
     Ada.Strings.Unbounded.Text_IO;

with Trim_Special_Characters;

package body Internet.Headers is

   use Ada.Strings.Fixed;
   use Ada.Strings.Maps;

   ---------------------------------------------------------------------------
   --  Character sequences:

   Control_Characters : constant Character_Sequence :=
                          To_Sequence (Constants.Control_Set);

   ---------------------------------------------------------------------------
   --  Character mappings:

   Control_To_Space : constant Character_Mapping :=
                        To_Mapping (From => Control_Characters,
                                    To   => Control_Characters'Length * ' ');

   ---------------------------------------------------------------------------
   --  procedure Get:
   --
   --  Reads a header from an open file. Terminates when a blank line has
   --  been read.

   procedure Get (File                       : in     Ada.Text_IO.File_Type;
                  Item                       :    out Header_Type;
                  Skip_Leading_Empty_Lines   : in     Boolean := False;
                  Skip_Unknown_Header_Titles : in     Boolean := False) is

      use Ada.Characters.Handling, Ada.Strings, Ada.Strings.Unbounded;
      use Ada.Strings.Unbounded.Text_IO;

      Line           : Unbounded_String;
      Previous_Field : Item_Type := Item_Type'First;
      On_First_Line  : Boolean := True;

   begin --  Get
      for Field in Item'Range loop
         Item (Field) := Null_Unbounded_String;
      end loop;

      loop
         Get_Line (File => File, Item => Line);

         exit when not Skip_Leading_Empty_Lines or Length (Line) /= 0;
      end loop;

      loop
         if Is_Letter (Element (Line, 1)) then --  New field:

            if Index (Line, ":") = 0 then
               raise Invalid_Header_Format
                 with "Line containing """ & To_String (Line) & """";
            else
               declare

                  Field_Name    : constant String :=
                    Slice (Line, 1, Index (Line, ":") - 1);
                  Field_Content : constant String :=
                    Slice (Line, Index (Line, ":") + 1, Length (Line));

               begin
                  if Is_Header_Title (Field_Name) then
                     Previous_Field := Value (Field_Name);
                     Item (Previous_Field) :=
                       +Trim_Special_Characters (Field_Content, Both);
                  elsif Skip_Unknown_Header_Titles then
                     null;
                  else
                     raise Unknown_Header_Title
                       with "Line containing """ & To_String (Line) & """";
                  end if;
               end;
            end if;
         elsif Element (Line, 1) in Character'First .. ' ' then
            --  Append to previous field:
            if On_First_Line then
               raise Invalid_Header_Title
                 with "Line containing """ & To_String (Line) & """";
            else
               Append (Item (Previous_Field), " ");
               Append (Item (Previous_Field), Line);
            end if;
         else --  Error
            raise Invalid_Header_Format
              with "Line containing """ & To_String (Line) & """";
         end if;

         Get_Line (File => File, Item => Line);
         On_First_Line := False;
         exit when Length (Line) = 0;
      end loop;
   end Get;

   ---------------------------------------------------------------------------
   --  function Image:
   --
   --  Converts an Item_Type object to a String. '_' is translated to '-'.

   function Image (Item : in Item_Type) return String is
      Underscore_To_Dash : constant Character_Mapping :=
        To_Mapping (From => "_", To => "-");

   begin --  Image
      return Translate (Source  => Item_Type'Image (Item),
                        Mapping => Underscore_To_Dash);
   end Image;

   ---------------------------------------------------------------------------
   --  function Is_Header_Title:
   --
   --  Checks if a title is valid in this context (using function Value).

   function Is_Header_Title (Item : in String) return Boolean is
      Dummy : Item_Type;
      pragma Unreferenced (Dummy);
   begin
      Dummy := Value (Item);
      return True;
   exception
      when Constraint_Error =>
         return False;
   end Is_Header_Title;

   ---------------------------------------------------------------------------
   --  procedure Put:
   --
   --  Writes a header to an open file (including a terminating blank line).

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Header_Type) is

      use Ada.Text_IO, Ada.Strings.Unbounded, Ada.Strings.Unbounded.Text_IO;

   begin --  Put
      for Field in Item'Range loop
         if Length (Item (Field)) /= 0 then
            Put      (File, Image (Field));
            Put      (File, ": ");
            Put_Line (File, Translate (Source  => Item (Field),
                                       Mapping => Control_To_Space));
         end if;
      end loop;

      New_Line (File);
   end Put;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Converts a String to an Item_Type object. '-' is translated to '_'.

   function Value (Item : in String) return Item_Type is
      Dash_To_Underscore : constant Character_Mapping :=
        To_Mapping (From => "-", To => "_");

   begin --  Value
      return Item_Type'Value (Translate (Source  => Item,
                                         Mapping => Dash_To_Underscore));
   end Value;

   ---------------------------------------------------------------------------

end Internet.Headers;
