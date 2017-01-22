--  For processing of Internet (Mail/Usenet/...) headers.

with Ada.Strings.Unbounded,
     Ada.Text_IO;

generic

   type Item_Type is (<>); --  For true Internet headers are only ASCII
                           --  letters (and digits?) and '_' (~'-') allowed
                           --  in the header item names.

package Internet.Headers is

   ---------------------------------------------------------------------------
   --  Exceptions:

   Invalid_Header_Format : exception;
   Unknown_Header_Title  : exception;
   Invalid_Header_Title  : exception;

   ---------------------------------------------------------------------------
   --  Types:

   ---------------------------------------------------------------------
   --  subtype Header_Content_Type:

   subtype Header_Content_Type is Ada.Strings.Unbounded.Unbounded_String;

   ---------------------------------------------------------------------
   --  type Header_Type:

   type Header_Type is array (Item_Type) of Header_Content_Type;

   ---------------------------------------------------------------------------
   --  function Image:
   --
   --  Converts an Item_Type object to a String. '_' is translated to '-'.

   function Image (Item : in Item_Type) return String;

   ---------------------------------------------------------------------------
   --  function Value:
   --
   --  Converts a String to an Item_Type object. '-' is translated to '_'.

   function Value (Item : in String) return Item_Type;

   ---------------------------------------------------------------------------
   --  function Is_Header_Title:
   --
   --  Checks if a title is valid in this context (using function Value).

   function Is_Header_Title (Item : in String) return Boolean;

   ---------------------------------------------------------------------------
   --  procedure Get:
   --
   --  Reads a header from an open file. Terminates when a blank line has
   --  been read.

   procedure Get (File                       : in     Ada.Text_IO.File_Type;
                  Item                       :    out Header_Type;
                  Skip_Leading_Empty_Lines   : in     Boolean := False;
                  Skip_Unknown_Header_Titles : in     Boolean := False);

   ---------------------------------------------------------------------------
   --  procedure Put:
   --
   --  Writes a header to an open file (including a terminating blank line).

   procedure Put (File : in     Ada.Text_IO.File_Type;
                  Item : in     Header_Type);

   ---------------------------------------------------------------------------

private
   function "+" (Item : in String) return Header_Content_Type
     renames Ada.Strings.Unbounded.To_Unbounded_String;
end Internet.Headers;
