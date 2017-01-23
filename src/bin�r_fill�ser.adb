------------------------------------------------------------------------------
--
--  procedure Bin�r_Fill�ser (body)
--
--  L�ser en fil som en tabel med 32-bit C-heltal eller C-decimaltal.
--
------------------------------------------------------------------------------
--  Opdateringslog:
--
--  2000.10.23 (Jacob Sparre Andersen)
--    Skrevet.
--
------------------------------------------------------------------------------
--  Standardpakker:

with Ada.Command_Line;
with Ada.Direct_IO;
with Ada.Text_IO;
with Ada.Unchecked_Conversion;
with Interfaces.C;

------------------------------------------------------------------------------

procedure Bin�r_Fill�ser is

   ---------------------------------------------------------------------------
   --  32-bit data:

   type Data_32_Bit is array (0 .. 31) of Boolean;
   pragma Pack (Data_32_Bit);
   for Data_32_Bit'Size use 32;

   ---------------------------------------------------------------------------
   --  C-heltal:

   type C_Heltal is new Interfaces.C.int;
   for C_Heltal'Size use 32;

   ---------------------------------------------------------------------------
   --  C-decimaltal:

   type C_Decimaltal is new Interfaces.C.C_float;
   for C_Decimaltal'Size use 32;

   ---------------------------------------------------------------------------
   --  function Til_Heltal:

   function Til_Heltal is new Ada.Unchecked_Conversion (Source => Data_32_Bit,
                                                        Target => C_Heltal);

   ---------------------------------------------------------------------------
   --  function Til_Decimaltal:

   function Til_Decimaltal is
     new Ada.Unchecked_Conversion (Source => Data_32_Bit,
                                   Target => C_Decimaltal);

   ---------------------------------------------------------------------------
   --  package Data_IO:

   package Data_IO is new Ada.Direct_IO (Element_Type => Data_32_Bit);

   ---------------------------------------------------------------------------
   --  package Heltal_Tekst_IO:

   package Heltal_Tekst_IO is new Ada.Text_IO.Integer_IO (Num => C_Heltal);

   ---------------------------------------------------------------------------
   --  package Decimaltal_Tekst_IO:

   package Decimaltal_Tekst_IO is
     new Ada.Text_IO.Float_IO (Num => C_Decimaltal);

   ---------------------------------------------------------------------------
   --  package Indeks_Tekst_IO:

   package Indeks_Tekst_IO is
     new Ada.Text_IO.Integer_IO (Num => Data_IO.Count);

   ---------------------------------------------------------------------------

   use Ada.Text_IO;
   use Data_IO;
   use Decimaltal_Tekst_IO;
   use Heltal_Tekst_IO;
   use Indeks_Tekst_IO;

   Bin�r_Fil : Data_IO.File_Type;
   Indeks    : Data_IO.Count := 0;
   Data      : Data_32_Bit;

begin --  Bin�r_Fill�ser
   Open (File => Bin�r_Fil,
         Name => Ada.Command_Line.Argument (1),
         Mode => In_File);

   while not End_Of_File (File => Bin�r_Fil) loop
      Indeks := Indeks + 1;
      Read (File => Bin�r_Fil,
            Item => Data);

      Put (File => Standard_Output,
           Item => Indeks);
      Put (File => Standard_Output,
           Item => "   ");
      Put (File => Standard_Output,
           Item => Til_Heltal (Data));
      Put (File => Standard_Output,
           Item => "   ");
      Put (File => Standard_Output,
           Item => Til_Decimaltal (Data));
      New_Line (File => Standard_Output);
   end loop;

   Close (File => Bin�r_Fil);
end Bin�r_Fill�ser;
