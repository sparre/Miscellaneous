with Ada.Characters.Handling,
     Ada.Command_Line,
     Ada.Directories,
     Ada.Strings.Fixed.Equal_Case_Insensitive,
     Ada.Strings.Maps,
     Ada.Strings.Unbounded,
     Ada.Strings.Unbounded.Text_IO,
     Ada.Text_IO;

procedure Extract_With_Clauses is
   use Ada.Strings.Unbounded;

   procedure Extract (From : in     String);

   procedure Remove_Comments (Line : in out Unbounded_String);

   procedure Split_At_Semicolon (Source       : in     Unbounded_String;
                                 First        :    out Unbounded_String;
                                 Remainder    :    out Unbounded_String;
                                 No_Semicolon :    out Boolean);
   function Matching (Line : in Unbounded_String;
                      Head : in String) return Boolean;

   function End_Of_Context_Clause (Line : in Unbounded_String) return Boolean;
   function Is_With_Clause        (Line : in Unbounded_String) return Boolean;

   function To_Unit_Name (File_Name : in String) return String;

   procedure Put_With_Clause_As_Graph (Line      : in Unbounded_String;
                                       Dependent : in String);

   function End_Of_Context_Clause (Line : in Unbounded_String) return Boolean
   is
   begin
      return
        Matching (Line, "generic ") or
        Matching (Line, "procedure ") or
        Matching (Line, "function ") or
        Matching (Line, "package ");
   end End_Of_Context_Clause;

   procedure Extract (From : in     String) is
      use Ada.Strings.Unbounded.Text_IO, Ada.Text_IO;
      Source      : File_Type;
      Line        : Unbounded_String;
      Accumulated : Unbounded_String;
      Remainder   : Unbounded_String;
      Done        : Boolean;
   begin
      Put_Line (Standard_Error,
                "Analysing '" & From & "'...");

      Open (File => Source,
            Name => From,
            Mode => In_File);

      Physical_Lines :
      loop
         Get_Line (File => Source,
                   Item => Line);

         Remove_Comments (Line);
         Append (Accumulated, Line);

         loop
            Split_At_Semicolon (Source       => Accumulated,
                                First        => Line,
                                Remainder    => Remainder,
                                No_Semicolon => Done);
            exit when Done;
            Accumulated := Remainder;

            Trim (Source => Line,
                  Side   => Ada.Strings.Both);

            exit Physical_Lines when End_Of_Context_Clause (Line);

            if Is_With_Clause (Line) then
               Put_Line (File => Standard_Error,
                         U    => From & ": " & Line);

               Put_With_Clause_As_Graph (Line      => Line,
                                         Dependent => To_Unit_Name (From));
            end if;
         end loop;
      end loop Physical_Lines;

      Close (File => Source);
   exception
      when End_Error =>
         Close (File => Source);
   end Extract;

   function Is_With_Clause (Line : in Unbounded_String) return Boolean is
   begin
      return Matching (Line, "with ");
   end Is_With_Clause;

   function Matching (Line : in Unbounded_String;
                      Head : in String) return Boolean is
   begin
      return
        Ada.Strings.Fixed.Equal_Case_Insensitive
          (To_String (Ada.Strings.Unbounded.Head (Line, Head'Length)),
           Head);
   end Matching;

   procedure Put_With_Clause_As_Graph (Line      : in Unbounded_String;
                                       Dependent : in String) is
      use Ada.Characters.Handling, Ada.Text_IO;

      With_Clause : Unbounded_String := Line;
      Position    : Natural;
      Dependency  : Unbounded_String;
   begin
      Delete (Source  => With_Clause,
              From    => 1,
              Through => 5);

      loop
         Position := Index (Source  => With_Clause,
                            Pattern => ",");

         exit when Position = 0;

         Dependency := To_Unbounded_String (Slice (Source => With_Clause,
                                                   Low    => 1,
                                                   High   => Position - 1));
         Trim (Source => Dependency,
               Side   => Ada.Strings.Both);

         Put_Line ("  """ & Dependent & """ -> """ &
                   To_Lower (To_String (Dependency)) & """");

         Delete (Source  => With_Clause,
                 From    => 1,
                 Through => Position);
      end loop;

      Position := Index (With_Clause, ";");
      pragma Assert (Position = Length (With_Clause));

      Dependency := To_Unbounded_String (Slice (Source => With_Clause,
                                                Low    => 1,
                                                High   => Position - 1));
      Trim (Source => Dependency,
            Side   => Ada.Strings.Both);

      Put_Line ("  """ & Dependent & """ -> """ &
                To_Lower (To_String (Dependency)) & """");
   end Put_With_Clause_As_Graph;

   procedure Remove_Comments (Line : in out Unbounded_String) is
      Position : constant Natural := Index (Line, "--");
   begin
      if Position > 0 then
         Delete (Source  => Line,
                 From    => Position,
                 Through => Length (Line));
      end if;
   end Remove_Comments;

   procedure Split_At_Semicolon (Source       : in     Unbounded_String;
                                 First        :    out Unbounded_String;
                                 Remainder    :    out Unbounded_String;
                                 No_Semicolon :    out Boolean) is
      Position : constant Natural := Index (Source, ";");
   begin
      if Position = 0 then
         No_Semicolon := True;
      else
         First :=     To_Unbounded_String (Slice (Source => Source,
                                                  Low    => 1,
                                                  High   => Position));
         Remainder := To_Unbounded_String (Slice (Source => Source,
                                                  Low    => Position + 1,
                                                  High   => Length (Source)));
         No_Semicolon := False;
      end if;
   end Split_At_Semicolon;

   function To_Unit_Name (File_Name : in String) return String is
      use Ada.Characters.Handling, Ada.Directories, Ada.Strings.Fixed,
          Ada.Strings.Maps;

      Bar_To_Dot : constant Character_Mapping := To_Mapping (From => "-",
                                                             To   => ".");
   begin
      return
        Translate (Source  => To_Lower (Base_Name (File_Name)),
                   Mapping => Bar_To_Dot);
   end To_Unit_Name;

   use Ada.Command_Line, Ada.Text_IO;
begin
   if Argument_Count = 0 then
      Put_Line (File => Standard_Error,
                Item => "Usage:");
      Put_Line (File => Standard_Error,
                Item => "  " & Command_Name & " ada_source_file_names");
      Set_Exit_Status (Failure);
      return;
   end if;

   Put_Line ("digraph Dependencies {");
   for Index in 1 .. Argument_Count loop
      Extract (From => Argument (Index));
   end loop;
   Put_Line ("}");
end Extract_With_Clauses;
