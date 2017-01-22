with
  Ada.Text_IO;
with
  Internet.Headers;

procedure Ordn_Tegneserieliste is
   type Entry_Fields is (Serie, Årgang, Nummer, Sprog, Titel, Originaltitel,
                         Historier,
                         Forfatter, Tegner, Farvelægger,
                         Oversætter, Tekstning, Kunstnere,
                         Udgave, Oplag,
                         Sidetal, Pris, Forlag, ISBN, Udgivelsesår,
                         Anskaffelsesdato, Anskaffelsespris,
                         Samlinger, Værdi, Noter);

   package Entry_Text_IO is new Internet.Headers (Entry_Fields);

   use Ada.Text_IO;
   use Entry_Text_IO;

   Comic : Header_Type;
begin
   loop
      Get (File                     => Standard_Input,
           Item                     => Comic,
           Skip_Leading_Empty_Lines => True);
      Put (File => Standard_Output,
           Item => Comic);
   end loop;
exception
   when End_Error =>
      Put (File => Standard_Output,
           Item => Comic);
   when others =>
      Put_Line (File => Standard_Error,
                Item => "An exception was raised in procedure " &
                        "Ordn_Tegneserieliste. At line " &
                        Line (Standard_Input)'Img & " in the input data.");
      raise;
end Ordn_Tegneserieliste;
