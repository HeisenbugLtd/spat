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
with Ada.Text_IO;

with SPAT.Strings;

separate (Run_SPAT)

------------------------------------------------------------------------------
--  Print_Summary
------------------------------------------------------------------------------
procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                         Sort_By : in SPAT.Spark_Info.Sorting_Criterion;
                         Cut_Off : in Duration)
is
   Files         : constant SPAT.Strings.File_Names :=
     Info.List_All_Files (Sort_By => Sort_By);
   Count_Omitted : Natural := 0;
   Second_Column : Ada.Text_IO.Count := 0;
   Third_Column  : Ada.Text_IO.Count;
   use type Ada.Text_IO.Count;
begin
   for File of Files loop
      --  Can't use Files.Max_Length here, because we use the Simple_Name, not
      --  the actual string stored.
      Second_Column :=
        Ada.Text_IO.Count'Max (Second_Column,
                               Ada.Directories.Simple_Name
                                 (Name => SPAT.To_String (File))'Length);
   end loop;

   Second_Column := Second_Column + 2;
   Third_Column  := Second_Column + 4;

   for File of Files loop
      if Info.Proof_Time (File => File) < Cut_Off then
         --  Below cut off point, do nothing but count the number of items
         --  omitted.
         Count_Omitted := Count_Omitted + 1;
      else
         SPAT.Log.Message
           (Message  =>
              Ada.Directories.Simple_Name
                (Name => SPAT.To_String (Source => File)),
            New_Line => False);
         Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                              To   => Second_Column);
         SPAT.Log.Message
           (Message  =>
              "=> (Flow  => " &
              Image (Value => Info.Flow_Time (File => File)) & ",",
            New_Line => False);
         Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                              To   => Third_Column);
         SPAT.Log.Message
           (Message =>
              "Proof => " & Image (Value => Info.Proof_Time (File => File)) & ")");
      end if;
   end loop;

   if Count_Omitted /= 0 and then SPAT.Log.Debug_Enabled then
      SPAT.Log.Debug
        (Message =>
           "Omitted results below cut-off point (" & Image (Cut_Off) & "):" &
           Count_Omitted'Image & ".");
   end if;

end Print_Summary;
