------------------------------------------------------------------------------
--
--  procedure Ordn_Filmliste (body)
--
--  Command line arguments should be alternating header names prefixed by "--"
--  and header data.
--
--  For example should
--
--    echo 'title: something' | ordn_filmliste --status +
--
--  give the output:
--
--    TITLE: something
--    STATUS: +
------------------------------------------------------------------------------

with
  Ada.Text_IO;
with
  Internet.Headers;

procedure Ordn_Filmliste is
   type Entry_Fields is (Titel, Originaltitel, Medie,
                         Anskaffelsesdato, Anskaffelsespris,
                         Instruktør, Produktionsår,
                         Lyd, Undertekster,
                         Spilletid,
                         Pris,
                         Værdi, Noter,
                         Aldersgrænse, Etiketter);

   package Entry_Text_IO is new Internet.Headers (Entry_Fields);

   use Ada.Text_IO;
   use Entry_Text_IO;

   Movie : Header_Type;
begin
   loop
      Get (File                     => Standard_Input,
           Item                     => Movie,
           Skip_Leading_Empty_Lines => True);
      Put (File => Standard_Output,
           Item => Movie);
   end loop;
exception
   when End_Error =>
      Put (File => Standard_Output,
           Item => Movie);
   when others =>
      Put_Line (File => Standard_Error,
                Item => "An exception was raised in procedure " &
                        "Ordn_Filmliste. At line " &
                        Line (Standard_Input)'Img & " in the input data.");
      raise;
end Ordn_Filmliste;
