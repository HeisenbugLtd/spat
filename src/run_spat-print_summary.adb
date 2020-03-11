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
--  S.P.A.T. - Main program - separate Print_Summary
--
------------------------------------------------------------------------------
separate (Run_SPAT)
procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                         Sort_By : in SPAT.Spark_Info.Sorting_Criterion)
is
   Files      : constant SPAT.Spark_Info.String_Array :=
                  Info.List_All_Files (Sort_By => Sort_By);

   use type Ada.Text_IO.Count;
   Max_Length : constant Ada.Text_IO.Count :=
                  SPAT.Spark_Info.Max_Length (Source => Files) + 2;
begin
   for File of Files loop
      Ada.Text_IO.Put
        (File => Ada.Text_IO.Standard_Output,
         Item =>
           Ada.Directories.Simple_Name
             (Name => SPAT.To_String (Source => File)));
      Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                           To   => Max_Length);
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Output,
         Item =>
           "=> (Flow  => " &
           Image (Value => Info.Flow_Time (File => File)) & ",");
      Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                           To   => Max_Length + 4);
      Ada.Text_IO.Put_Line
        (File => Ada.Text_IO.Standard_Output,
         Item => "Proof => " &
           Image (Value => Info.Proof_Time (File => File)) & ")");
   end loop;
end Print_Summary;
