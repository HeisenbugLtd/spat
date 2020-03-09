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

   declare
      SPARK_Files : SPAT.Spark_Files.T;
      Start_Time  : Ada.Real_Time.Time;
      Verbose     : constant Boolean := SPAT.Command_Line.Verbose.Get;
   begin
      declare
         File_List : SPAT.File_Ops.File_List;
      begin
         --  Step 1: Collect all ".spark" files in the directories given on the
         --          command line recursively.
         for Dir of SPAT.Command_Line.Directories.Get loop
            declare
               Search_Dir : constant String := SPAT.To_String (Source => Dir);
            begin
               if Verbose then
                  Start_Time := Ada.Real_Time.Clock;
               end if;

               File_List.Add_Files (Directory => Search_Dir,
                                    Extension => "spark");

               if Verbose then
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
                  end;
               end if;
            exception
               when Ada.Directories.Name_Error =>
                  Ada.Text_IO.Put_Line
                    (File => Ada.Text_IO.Standard_Error,
                     Item => "Directory """ & Search_Dir & """ not found!");
            end;
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
      end;

      declare
         Info : SPAT.Spark_Info.T;
      begin
         --  Step 3: Process the JSON data.
         if not SPARK_Files.Is_Empty then
            if Verbose then
               Start_Time := Ada.Real_Time.Clock;
            end if;

            for C in SPARK_Files.Iterate loop
               declare
                  Read_Result : constant GNATCOLL.JSON.Read_Result :=
                                  SPARK_Files (C);
                  File        : constant SPAT.File_Name :=
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
               end;
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
         for File of Info.List_All_Files loop
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Output,
               Item =>
                 Ada.Directories.Simple_Name
                   (Name => SPAT.To_String (Source => File)) &
                 " => [Flow => " &
                 Image (Value => Info.Flow_Time (File => File)) & "/" &
                 Info.Num_Flows'Image & "], [Proof => " &
                 Image (Value => Info.Proof_Time (File => File)) &
                 "/" & Info.Num_Proofs'Image & "]");
         end loop;

         if SPAT.Command_Line.List.Get then
            for S of Info.List_All_Entities loop
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Output,
                  Item => SPAT.To_String (Source => S) & " => " &
                    Image (Value => Info.Max_Proof_Time (Element => S)) & "/" &
                    Image (Value => Info.Total_Proof_Time (Element => S)));
            end loop;
         end if;
      end;
   end;

   Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
end Run_SPAT;
