with
  Ada.Text_IO;
with
  Internet.Headers;

procedure Ordn_Bogliste is
   type Entry_Fields is (Titel, Originaltitel,
                         Forfatter, Oversætter, Redaktør,
                         Forlag, ISBN,
                         Anskaffelsesdato, Anskaffelsespris,
                         Udgivelsesår,
                         Sprog,
                         Sidetal,
                         Pris, Værdi,
                         Læst, Noter);

   package Entry_Text_IO is new Internet.Headers (Entry_Fields);

   use Ada.Text_IO;
   use Entry_Text_IO;

   Book : Header_Type;
begin
   loop
      Get (File                     => Standard_Input,
           Item                     => Book,
           Skip_Leading_Empty_Lines => True);
      Put (File => Standard_Output,
           Item => Book);
   end loop;
exception
   when End_Error =>
      Put (File => Standard_Output,
           Item => Book);
   when others =>
      Put_Line (File => Standard_Error,
                Item => "An exception was raised in procedure " &
                        "Ordn_Bogliste. At line " &
                        Line (Standard_Input)'Img & " in the input data.");
      raise;
end Ordn_Bogliste;
