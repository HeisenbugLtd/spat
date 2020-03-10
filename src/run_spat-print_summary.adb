separate (Run_SPAT)
procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                         Sort_By : in SPAT.Spark_Info.Sorting_Criterion)
is
   Files      : constant SPAT.Spark_Info.String_Array :=
                  Info.List_All_Files (Sort_By => Sort_By);
   Max_Length : Ada.Text_IO.Count := 0;
   use type Ada.Text_IO.Count;
begin
   for File of Files loop
      Max_Length :=
        Ada.Text_IO.Count'Max (Max_Length,
                               Ada.Directories.Simple_Name
                                 (Name => SPAT.To_String (File))'Length);
   end loop;

   Max_Length := Max_Length + 2;

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
