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
with Ada.Containers.Vectors;
with Ada.Directories;

with GNAT.Regexp;
with GNATCOLL.JSON;
with GNATCOLL.Projects;
with GNATCOLL.VFS;
with SPAT.Command_Line;
with SPAT.GPR_Support;
with SPAT.Log;
with SPAT.Spark_Files;
with SPAT.Spark_Info;
with SPAT.Stop_Watch;
with SPAT.Strings;
with SPAT.Version;
with System;

------------------------------------------------------------------------------
--  Run_SPAT
------------------------------------------------------------------------------
procedure Run_SPAT is

   package Reg_Exp_List is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => GNAT.Regexp.Regexp,
                             "="          => GNAT.Regexp."=");

   ---------------------------------------------------------------------------
   --  Print_Entities
   ---------------------------------------------------------------------------
   procedure Print_Entities
     (Info          : in SPAT.Spark_Info.T;
      Sort_By       : in SPAT.Spark_Info.Sorting_Criterion;
      Cut_Off       : in Duration;
      Entity_Filter : in Reg_Exp_List.Vector);

   ---------------------------------------------------------------------------
   --  Print_Suggestion
   ---------------------------------------------------------------------------
   procedure Print_Suggestion
     (Info     : in SPAT.Spark_Info.T;
      File_Map : in SPAT.GPR_Support.SPARK_Source_Maps.Map);

   ---------------------------------------------------------------------------
   --  Print_Summary
   ---------------------------------------------------------------------------
   procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                            Sort_By : in SPAT.Spark_Info.Sorting_Criterion;
                            Cut_Off : in Duration);

   ---------------------------------------------------------------------------
   --  Print_Entities
   ---------------------------------------------------------------------------
   procedure Print_Entities
     (Info          : in SPAT.Spark_Info.T;
      Sort_By       : in SPAT.Spark_Info.Sorting_Criterion;
      Cut_Off       : in Duration;
      Entity_Filter : in Reg_Exp_List.Vector) is separate;

   ---------------------------------------------------------------------------
   --  Print_Suggestion
   ---------------------------------------------------------------------------
   procedure Print_Suggestion
     (Info     : in SPAT.Spark_Info.T;
      File_Map : in SPAT.GPR_Support.SPARK_Source_Maps.Map) is separate;

   ---------------------------------------------------------------------------
   --  Print_Summary
   ---------------------------------------------------------------------------
   procedure Print_Summary (Info    : in SPAT.Spark_Info.T;
                            Sort_By : in SPAT.Spark_Info.Sorting_Criterion;
                            Cut_Off : in Duration) is separate;

   use type SPAT.Subject_Name;

   Entity_Filter : Reg_Exp_List.Vector;
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

   --  If there were entity filter options given, try compiling the reg exps.
   --
   --  for Expression of SPAT.Command_Line.Entity_Filter.Get loop
   --  The above triggers a GNAT bug box with GNAT CE 2020.
   --
   declare
      Filter : constant SPAT.Command_Line.Entity_Filter.Result_Array
        := SPAT.Command_Line.Entity_Filter.Get;
   begin
      for Expression of Filter loop
         begin
            Entity_Filter.Append
              (New_Item =>
                 GNAT.Regexp.Compile
                   (Pattern        => SPAT.To_String (Expression),
                    Glob           => False,
                    Case_Sensitive => False));
            null;
         exception
            when GNAT.Regexp.Error_In_Regexp =>
               SPAT.Log.Message
                 (Message =>
                    "Argument parsing failed: """ &
                    SPAT.To_String (Source => Expression) &
                    """ is not a valid regular expression.");
               SPAT.Spark_Files.Shutdown;
               Ada.Command_Line.Set_Exit_Status
                 (Code => Ada.Command_Line.Failure);
               return;
         end;
      end loop;
   end;

   Do_Run_SPAT :
   declare
      SPARK_Files  : SPAT.Spark_Files.T;
      Timer        : SPAT.Stop_Watch.T := SPAT.Stop_Watch.Create;
      Sort_By      : constant SPAT.Spark_Info.Sorting_Criterion :=
        SPAT.Command_Line.Sort_By.Get;
      Cut_Off      : constant Duration := SPAT.Command_Line.Cut_Off.Get;
      Report_Mode  : constant SPAT.Command_Line.Report_Mode :=
        SPAT.Command_Line.Report.Get;
      Project_File : constant GNATCOLL.VFS.Filesystem_String :=
        GNATCOLL.VFS."+" (S => SPAT.To_String (SPAT.Command_Line.Project.Get));
      File_List : constant SPAT.GPR_Support.SPARK_Source_Maps.Map :=
        SPAT.GPR_Support.Get_SPARK_Files (GPR_File => Project_File);
      use type SPAT.Command_Line.Report_Mode;
   begin
      Collect_And_Parse :
      begin
         --  Step 2: Parse the files into JSON values.
         if not File_List.Is_Empty then
            SPAT.Log.Debug
              (Message =>
                 "Using up to" & SPAT.Spark_Files.Num_Workers'Image &
                 " parsing threads.");

            Timer.Start;

            declare
               File_Names : SPAT.Strings.SPARK_File_Names (Capacity => File_List.Length);
            begin
               for X in File_List.Iterate loop
                  File_Names.Append (SPAT.GPR_Support.SPARK_Source_Maps.Key (X));
               end loop;

               SPARK_Files.Read (Names => File_Names);
            end;

            SPAT.Log.Debug
              (Message => "Parsing completed in " & Timer.Elapsed & ".");
         end if;
      end Collect_And_Parse;

      Process_And_Output :
      declare
         Info : SPAT.Spark_Info.T;
      begin
         --  Step 3: Process the JSON data.
         if not SPARK_Files.Is_Empty then
            Timer.Start;

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
              (Message => "Reading completed in " & Timer.Elapsed & ".");
         end if;

         SPAT.Log.Debug
           (Message =>
              "Collecting files completed in " & Timer.Elapsed_Total & ".");

         SPAT.Log.Debug
           (Message => "Cut off point set to " & SPAT.Image (Cut_Off) & ".");

         --  Step 4: Output the JSON data.
         if SPAT.Command_Line.Summary.Get then
            Print_Summary (Info    => Info,
                           Sort_By => Sort_By,
                           Cut_Off => Cut_Off);
         end if;

         if Report_Mode /= SPAT.Command_Line.None then
            Print_Entities (Info          => Info,
                            Sort_By       => Sort_By,
                            Cut_Off       => Cut_Off,
                            Entity_Filter => Entity_Filter);
         end if;

         if SPAT.Command_Line.Suggest.Get then
            Print_Suggestion (Info     => Info,
                              File_Map => File_List);
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
      Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Failure);
end Run_SPAT;
