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
with Ada.Directories;
with Ada.Real_Time;
with Ada.Strings.Unbounded;
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
      File_List  : SPAT.File_Ops.File_List;
      SPARK_Data : SPAT.Spark_Files.SPARK_Data;
      Start_Time : Ada.Real_Time.Time;
      Verbose    : constant Boolean := SPAT.Command_Line.Verbose.Get;
   begin
      --  Step 1: Collect all ".spark" files in the directories given on the
      --          command line recursively.
      for Dir of SPAT.Command_Line.Directories.Get loop
         declare
            Search_Dir : constant String :=
              Ada.Strings.Unbounded.To_String (Source => Dir);
         begin
            if Verbose then
               Start_Time := Ada.Real_Time.Clock;
            end if;

            File_List.Add_Files (Directory => Search_Dir,
                                 Extension => "spark");

            if Verbose then
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Output,
                  Item => "Search completed in " &
                    Image (Ada.Real_Time.To_Duration (TS => Ada.Real_Time.Clock - Start_Time)) &
                    "," & File_List.Length'Image & " files found.");
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

         SPARK_Data.Read_Files (Names => File_List);

         if Verbose then
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Output,
               Item => "Parsing completed in " &
                 Image (Ada.Real_Time.To_Duration (TS => Ada.Real_Time.Clock - Start_Time)) & ".");
         end if;
      end if;

      --  Step 3: TODO: Process the JSON data.
      if not SPARK_Data.Is_Empty then
         if Verbose then
            Start_Time := Ada.Real_Time.Clock;
         end if;

         declare
            C : SPAT.Spark_Files.Cursor := SPARK_Data.First;
            use type SPAT.SPARK_Files.Cursor;
         begin
            while C /= SPAT.Spark_Files.No_Element loop
               declare
                  Read_Result : GNATCOLL.JSON.Read_Result renames
                    SPAT.Spark_Files.Element (C);
               begin
                  Ada.Text_IO.Put
                    (File => Ada.Text_IO.Standard_Output,
                     Item => """" & SPAT.Spark_Files.Key (C) & """ => ");

                  if Read_Result.Success then
                     declare
                        Info : SPAT.Spark_Info.T;
                     begin
                        Info.Map_JSON (Root => Read_Result.Value);

                        Ada.Text_IO.Put_Line
                          (File => Ada.Text_IO.Standard_Output,
                           Item => "[Proof => " & Image (Info.Proof_Time) &
                             "], [Flow => " & Image (Info.Flow_Time) & "]");
                     end;
                  else
                     Ada.Text_IO.Put_Line
                       (File => Ada.Text_IO.Standard_Output,
                        Item => GNATCOLL.JSON.Format_Parsing_Error (Error => Read_Result.Error));
                  end if;

                  SPAT.Spark_Files.Next (C => C);
               end;
            end loop;
         end;

         if Verbose then
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Output,
               Item => "Reading completed in " &
                 Image (Ada.Real_Time.To_Duration (TS => Ada.Real_Time.Clock - Start_Time)) & ".");
         end if;
      end if;
   end;

   Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
end Run_SPAT;
