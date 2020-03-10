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
           Image (Value => Info.Max_Proof_Time (Element => Entity)) &
           "/" &
           Image (Value => Info.Total_Proof_Time (Element => Entity)));
   end loop;
end Print_Entities;
