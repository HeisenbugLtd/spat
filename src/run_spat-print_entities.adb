------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

------------------------------------------------------------------------------
--
--  SPARK Proof Analysis Tool
--
--  S.P.A.T. - Main program - separate Print_Entities
--
------------------------------------------------------------------------------
separate (Run_SPAT)

procedure Print_Entities (Info    : in SPAT.Spark_Info.T;
                          Sort_By : in SPAT.Spark_Info.Sorting_Criterion)
is
   Entities : constant SPAT.Spark_Info.String_Array :=
                Info.List_All_Entities (Sort_By => Sort_By);
   Max_Length   : Ada.Text_IO.Count := 0;
begin
   for Entity of Entities loop
      Max_Length :=
        Ada.Text_IO.Count'Max
          (Max_Length,
           Ada.Text_IO.Count (SPAT.Length (Source => Entity)));
   end loop;

   Max_Length := Ada.Text_IO."+" (Max_Length, 2);

   for Entity of Entities loop
      Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Output,
                       Item => SPAT.To_String (Source => Entity));
      Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                           To   => Max_Length);
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Output,
         Item =>
           "=> " &
           Image (Value => Info.Max_Proof_Time (Entity => Entity)) &
           "/" &
           Image (Value => Info.Total_Proof_Time (Entity => Entity)));

      if SPAT.Command_Line.Details.Get then
         for P of Info.Proof_List (Entity => Entity) loop
            Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Output,
                             Item => "  " & P.Image);
            Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Output,
                             Item => " (" & SPAT.To_String (P.Rule) & ") => ");
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Output,
               Item => "  " & Image (P.Max_Time) & "/" & Image (P.Total_Time));
         end loop;
      end if;
   end loop;
end Print_Entities;
