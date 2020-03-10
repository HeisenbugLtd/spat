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
--  S.P.A.T. - Main program
--
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Containers;
with Ada.Directories;
with Ada.Real_Time;
with Ada.Text_IO;

with GNATCOLL.JSON;
with SI_Units.Metric;
with SI_Units.Names;
with SPAT.Command_Line;
with SPAT.File_Ops;
with SPAT.Spark_Files;
with SPAT.Spark_Info;

procedure Run_SPAT is

   function Image is new
     SI_Units.Metric.Fixed_Image (Item        => Duration,
                                  Default_Aft => 0,
                                  Unit        => SI_Units.Names.Second);

   use type Ada.Real_Time.Time;
begin
   if not SPAT.Command_Line.Parser.Parse then
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Do_Run_SPAT :
   declare
      SPARK_Files : SPAT.Spark_Files.T;
      Start_Time  : Ada.Real_Time.Time;
      Verbose     : constant Boolean := SPAT.Command_Line.Verbose.Get;
      Sort_By     : constant SPAT.Spark_Info.Sorting_Criterion :=
                      SPAT.Command_Line.Sort_By.Get;
   begin
      Collect_And_Parse :
      declare
         File_List : SPAT.File_Ops.File_List;
      begin
         --  Step 1: Collect all ".spark" files in the directories given on the
         --          command line recursively.
         for Dir of SPAT.Command_Line.Directories.Get loop
            Search_One_Directory :
            declare
               Search_Dir : constant String := SPAT.To_String (Source => Dir);
            begin
               if Verbose then
                  Start_Time := Ada.Real_Time.Clock;
               end if;

               File_List.Add_Files (Directory => Search_Dir,
                                    Extension => "spark");

               if Verbose then
                  Report_Timing :
                  declare
                     Num_Files : constant Ada.Containers.Count_Type :=
                                   File_List.Length;
                     use type Ada.Containers.Count_Type;
                  begin
                     Ada.Text_IO.Put_Line
                       (File => Ada.Text_IO.Standard_Output,
                        Item => "Search completed in " &
                          Image
                            (Value =>
                               Ada.Real_Time.To_Duration
                                 (TS => Ada.Real_Time.Clock - Start_Time)) &
                          "," & Num_Files'Image & " file" &
                          (if Num_Files /= 1
                           then "s"
                           else "") & " found so far.");
                  end Report_Timing;
               end if;
            exception
               when Ada.Directories.Name_Error =>
                  Ada.Text_IO.Put_Line
                    (File => Ada.Text_IO.Standard_Error,
                     Item => "Directory """ & Search_Dir & """ not found!");
            end Search_One_Directory;
         end loop;

         --  Step 2: Parse the files into JSON values.
         if not File_List.Is_Empty then
            if Verbose then
               Start_Time := Ada.Real_Time.Clock;
            end if;

            SPARK_Files.Read (Names => File_List);

            if Verbose then
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Output,
                  Item => "Parsing completed in " &
                    Image
                      (Value =>
                         Ada.Real_Time.To_Duration
                           (TS => Ada.Real_Time.Clock - Start_Time)) & ".");
            end if;
         end if;
      end Collect_And_Parse;

      Process_And_Output :
      declare
         Info : SPAT.Spark_Info.T;
      begin
         --  Step 3: Process the JSON data.
         if not SPARK_Files.Is_Empty then
            if Verbose then
               Start_Time := Ada.Real_Time.Clock;
            end if;

            for C in SPARK_Files.Iterate loop
               Parse_JSON_File :
               declare
                  Read_Result : constant GNATCOLL.JSON.Read_Result :=
                                  SPARK_Files (C);
                  File        : constant SPAT.Subject_Name :=
                                  SPAT.Spark_Files.Key (C);
               begin
                  if Read_Result.Success then
                     Info.Map_Spark_File (Root => Read_Result.Value,
                                          File => File);
                  else
                     Ada.Text_IO.Put_Line
                       (File => Ada.Text_IO.Standard_Output,
                        Item => SPAT.To_String (Source => File) & ": " &
                          GNATCOLL.JSON.Format_Parsing_Error
                            (Error => Read_Result.Error));
                  end if;
               end Parse_JSON_File;
            end loop;

            if Verbose then
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Output,
                  Item => "Reading completed in " &
                    Image
                      (Value =>
                         Ada.Real_Time.To_Duration
                           (TS => Ada.Real_Time.Clock - Start_Time)) & ".");
            end if;
         end if;

         --  Step 4: Output the JSON data.
         if SPAT.Command_Line.Summary.Get then
            Print_File_List :
            declare
               All_Files  : constant SPAT.Spark_Info.String_Array :=
                              Info.List_All_Files (Sort_By => Sort_By);
               Max_Length : Ada.Text_IO.Count := 0;
               use type Ada.Text_IO.Count;
            begin
               for File of All_Files loop
                  Max_Length :=
                    Ada.Text_IO.Count'Max
                      (Max_Length,
                       Ada.Directories.Simple_Name
                         (Name => SPAT.To_String (File))'Length);
               end loop;

               Max_Length := Max_Length + 2;

               for File of All_Files loop
                  Ada.Text_IO.Put
                    (File => Ada.Text_IO.Standard_Output,
                     Item =>
                       Ada.Directories.Simple_Name
                         (Name => SPAT.To_String (Source => File)));
                  Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                                       To   => Max_Length);
                  Ada.Text_IO.Put_Line
                    (File => Ada.Text_IO.Standard_Output,
                     Item => "=> (Flow  => " &
                       Image (Value => Info.Flow_Time (File => File)) &
                       ",");
                  Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                                       To   => Max_Length + 4);
                  Ada.Text_IO.Put_Line
                    (File => Ada.Text_IO.Standard_Output,
                     Item => "Proof => " &
                       Image (Value => Info.Proof_Time (File => File)) & ")");
               end loop;
            end Print_File_List;
         end if;

         if SPAT.Command_Line.List.Get then
            Print_Entity_List :
            declare
               All_Entities : constant SPAT.Spark_Info.String_Array :=
                                Info.List_All_Entities (Sort_By => Sort_By);
               Max_Length   : Ada.Text_IO.Count := 0;
            begin
               for Entity of All_Entities loop
                  Max_Length :=
                    Ada.Text_IO.Count'Max
                      (Max_Length,
                       Ada.Text_IO.Count (SPAT.Length (Source => Entity)));
               end loop;

               Max_Length := Ada.Text_IO."+" (Max_Length, 2);

               for Entity of All_Entities loop
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
            end Print_Entity_List;
         end if;
      end Process_And_Output;
   end Do_Run_SPAT;

   Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
end Run_SPAT;
