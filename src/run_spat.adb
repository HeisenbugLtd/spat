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
with GNATCOLL.Projects;
with GNATCOLL.VFS;
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

   procedure Print_Entities (Info    : in SPAT.Spark_Info.T;
                             Sort_By : in SPAT.Spark_Info.Sorting_Criterion);

   procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                            Sort_By : in SPAT.Spark_Info.Sorting_Criterion);

   procedure Print_Entities
     (Info    : in SPAT.Spark_Info.T;
      Sort_By : in SPAT.Spark_Info.Sorting_Criterion) is separate;

   procedure Print_Summary
     (Info    : in SPAT.Spark_Info.T;
      Sort_By : in SPAT.Spark_Info.Sorting_Criterion) is separate;

   use type Ada.Real_Time.Time;
begin
   if not SPAT.Command_Line.Parser.Parse then
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Do_Run_SPAT :
   declare
      SPARK_Files  : SPAT.Spark_Files.T;
      Start_Time   : Ada.Real_Time.Time;
      Verbose      : constant Boolean := SPAT.Command_Line.Verbose.Get;
      Sort_By      : constant SPAT.Spark_Info.Sorting_Criterion :=
        SPAT.Command_Line.Sort_By.Get;
      Project_File : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS."+" (S => SPAT.To_String (SPAT.Command_Line.Project.Get));
   begin
      Collect_And_Parse :
      declare
         File_List : SPAT.File_Ops.File_List;
      begin
         Load_Project_Files :
         declare
            Project_Tree : GNATCOLL.Projects.Project_Tree;
         begin
            if Verbose then
               Start_Time := Ada.Real_Time.Clock;
            end if;

            Project_Tree.Load
              (Root_Project_Path =>
                 GNATCOLL.VFS.Create (Full_Filename => Project_File));

            if Verbose then
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Output,
                  Item => "GNAT project loaded in " &
                    Image (Value =>
                             Ada.Real_Time.To_Duration
                               (TS => Ada.Real_Time.Clock - Start_Time)) &
                    ".");
            end if;

            --  Step 1: Collect all (potential) ".spark" files in the project
            --          file(s). TODO.
            if Verbose then
               Start_Time := Ada.Real_Time.Clock;
            end if;

            Load_Source_Files :
            declare
               Project_Files : GNATCOLL.VFS.File_Array_Access :=
                 Project_Tree.Root_Project.Source_Files (Recursive => True);
            begin
               for F of Project_Files.all loop
                  declare
                     Root_Project : constant GNATCOLL.Projects.Project_Type :=
                    Project_Tree.Root_Project;
                     SPARK_Name : constant String :=
                       Ada.Directories.Compose
                         (Containing_Directory =>
                            Ada.Directories.Compose
                              (Containing_Directory =>
                                 GNATCOLL.VFS."+"
                                   (GNATCOLL.Projects.Object_Dir
                                      (Project => Root_Project).Full_Name.all),
                               Name                 => "gnatprove"),
                          Name                 =>
                            Ada.Directories.Base_Name
                              (Name => F.Display_Full_Name),
                          Extension            => "spark");
                  begin
                     if Verbose then
                        Ada.Text_IO.Put_Line ("Found """ & F.Display_Base_Name &
                                                """, checking for """ &
                                                SPARK_Name & """...");
                     end if;

                     if Ada.Directories.Exists (Name => SPARK_Name) then
                        declare
                           File_Name : constant SPAT.Subject_Name :=
                             SPAT.To_Name (SPARK_Name);
                        begin
                           if not File_List.Contains (Item => File_Name) then
                              File_List.Append (New_Item => File_Name);
                           end if;
                        end;
                     end if;
                  end;
               end loop;

               GNATCOLL.VFS.Unchecked_Free (Arr => Project_Files);
            end Load_Source_Files;

            Project_Tree.Unload;
         exception
            when GNATCOLL.Projects.Invalid_Project =>
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Error,
                  Item =>
                    "Could not load """ &
                    SPAT.To_String (SPAT.Command_Line.Project.Get) &
                    """!");
         end Load_Project_Files;

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
                    Image (Value =>
                             Ada.Real_Time.To_Duration
                               (TS => Ada.Real_Time.Clock - Start_Time)) &
                    "," & Num_Files'Image & " file" &
                    (if Num_Files /= 1
                     then "s"
                     else "") & " found so far.");
            end Report_Timing;
         end if;

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
            Print_Summary (Info    => Info,
                           Sort_By => Sort_By);
         end if;

         if SPAT.Command_Line.List.Get then
            Print_Entities (Info    => Info,
                            Sort_By => Sort_By);
         end if;
      end Process_And_Output;
   end Do_Run_SPAT;

   Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
end Run_SPAT;
