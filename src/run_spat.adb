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

with GNATCOLL.JSON;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with SPAT.Command_Line;
with SPAT.GPR_Support;
with SPAT.Log;
with SPAT.Spark_Files;
with SPAT.Spark_Info;
with SPAT.Strings;
with SPAT.Version;
with System;

------------------------------------------------------------------------------
--  Run_SPAT
------------------------------------------------------------------------------
procedure Run_SPAT is

   ---------------------------------------------------------------------------
   --  Print_Entities
   ---------------------------------------------------------------------------
   procedure Print_Entities (Info    : in SPAT.Spark_Info.T;
                             Sort_By : in SPAT.Spark_Info.Sorting_Criterion;
                             Cut_Off : in Duration);

   ---------------------------------------------------------------------------
   --  Print_Summary
   ---------------------------------------------------------------------------
   procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                            Sort_By : in SPAT.Spark_Info.Sorting_Criterion;
                            Cut_Off : in Duration);

   ---------------------------------------------------------------------------
   --  Print_Entities
   ---------------------------------------------------------------------------
   procedure Print_Entities (Info    : in SPAT.Spark_Info.T;
                             Sort_By : in SPAT.Spark_Info.Sorting_Criterion;
                             Cut_Off : in Duration) is separate;

   ---------------------------------------------------------------------------
   --  Print_Summary
   ---------------------------------------------------------------------------
   procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                            Sort_By : in SPAT.Spark_Info.Sorting_Criterion;
                            Cut_Off : in Duration) is separate;

   use type Ada.Real_Time.Time;
   use type SPAT.Subject_Name;

begin
   if not SPAT.Command_Line.Parser.Parse then
      SPAT.Spark_Files.Shutdown;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   if SPAT.Command_Line.Version.Get then
      SPAT.Log.Message
        (Message =>
           "run_spat V" & SPAT.Version.Number &
           " (compiled by " & System.System_Name'Image & " " &
           SPAT.Version.Compiler & ")");

      SPAT.Spark_Files.Shutdown;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
      return;
   end if;

   if SPAT.Command_Line.Project.Get = SPAT.Null_Name then
      --  The project file option is mandatory (AFAICS there is no way to
      --  require an option argument).
      SPAT.Log.Message
        (Message => "Argument parsing failed: Missing project file argument");
      SPAT.Log.Message (Message => SPAT.Command_Line.Parser.Help);

      SPAT.Spark_Files.Shutdown;
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
      return;
   end if;

   Do_Run_SPAT :
   declare
      SPARK_Files  : SPAT.Spark_Files.T;
      Start_Time   : Ada.Real_Time.Time;
      Sort_By      : constant SPAT.Spark_Info.Sorting_Criterion :=
        SPAT.Command_Line.Sort_By.Get;
      Cut_Off      : constant Duration := SPAT.Command_Line.Cut_Off.Get;
      Report_Mode  : constant SPAT.Command_Line.Report_Mode :=
        SPAT.Command_Line.Report.Get;
      Project_File : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS."+" (S => SPAT.To_String (SPAT.Command_Line.Project.Get));
      use type SPAT.Command_Line.Report_Mode;
   begin
      Collect_And_Parse :
      declare
         --  Step 1: Collect all .spark files.
         File_List : constant SPAT.Strings.SPARK_File_Names :=
           SPAT.GPR_Support.Get_SPARK_Files (GPR_File => Project_File);
      begin
         --  Step 2: Parse the files into JSON values.
         if not File_List.Is_Empty then
            SPAT.Log.Debug
              (Message =>
                 "Using up to" & SPAT.Spark_Files.Num_Workers'Image &
                 " parsing threads.");

            Start_Time := Ada.Real_Time.Clock;

            SPARK_Files.Read (Names => File_List);

            SPAT.Log.Debug
              (Message =>
                 "Parsing completed in " &
                 SPAT.Image
                   (Value =>
                      Ada.Real_Time.To_Duration
                        (TS => Ada.Real_Time.Clock - Start_Time)) & ".");
         end if;
      end Collect_And_Parse;

      Process_And_Output :
      declare
         Info : SPAT.Spark_Info.T;
      begin
         --  Step 3: Process the JSON data.
         if not SPARK_Files.Is_Empty then
            Start_Time := Ada.Real_Time.Clock;

            for C in SPARK_Files.Iterate loop
               Parse_JSON_File :
               declare
                  Read_Result : constant GNATCOLL.JSON.Read_Result :=
                                  SPARK_Files (C);
                  File        : constant SPAT.SPARK_File_Name :=
                                  SPAT.Spark_Files.Key (C);
               begin
                  if Read_Result.Success then
                     Info.Map_Spark_File (Root => Read_Result.Value,
                                          File => File);
                  else
                     SPAT.Log.Warning
                       (Message =>
                          SPAT.To_String (Source => File) & ": " &
                          GNATCOLL.JSON.Format_Parsing_Error
                            (Error => Read_Result.Error));
                  end if;
               end Parse_JSON_File;
            end loop;

            SPAT.Log.Debug
              (Message =>
                 "Reading completed in " &
                 SPAT.Image
                   (Value =>
                      Ada.Real_Time.To_Duration
                        (TS => Ada.Real_Time.Clock - Start_Time)) & ".");
         end if;

         SPAT.Log.Debug
           (Message => "Cut off point set to " & SPAT.Image (Cut_Off) & ".");

         --  Step 4: Output the JSON data.
         if SPAT.Command_Line.Summary.Get then
            Print_Summary (Info    => Info,
                           Sort_By => Sort_By,
                           Cut_Off => Cut_Off);
         end if;

         if Report_Mode /= SPAT.Command_Line.None then
            Print_Entities (Info    => Info,
                            Sort_By => Sort_By,
                            Cut_Off => Cut_Off);
         end if;
      exception
         when E : others =>
            SPAT.Log.Dump_Exception
              (E       => E,
               Message => "Internal error encountered when processing data!");
      end Process_And_Output;
   end Do_Run_SPAT;

   SPAT.Spark_Files.Shutdown;
   Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
exception
   when E : others =>
      SPAT.Spark_Files.Shutdown;

      --  This shouldn't happen, other exception handlers should have caught
      --  such earlier.
      SPAT.Log.Dump_Exception
        (E       => E,
         Message => "Fatal error encountered in SPAT!");
end Run_SPAT;
