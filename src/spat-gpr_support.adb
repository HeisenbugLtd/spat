------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Directories;
with Ada.Real_Time;

with GNATCOLL.Projects;
with SI_Units.Metric;
with SI_Units.Names;
with SPAT.Log;

package body SPAT.GPR_Support is

   function Image is new
     SI_Units.Metric.Fixed_Image (Item        => Duration,
                                  Default_Aft => 0,
                                  Unit        => SI_Units.Names.Second);

   function SPARK_Name
     (Project_Tree : in GNATCOLL.Projects.Project_Tree;
      Source_File  : in GNATCOLL.VFS.Virtual_File) return String;

   function Get_SPARK_Files
     (GPR_File : GNATCOLL.VFS.Filesystem_String) return File_List
   is
      Start_Time  : Ada.Real_Time.Time;
      Raw_List    : SPAT.File_List; --  Stores candidate .spark files.
      Result_List : SPAT.File_List; --  Filtered list of files.

      use type Ada.Real_Time.Time;
   begin
      Load_Project_Files :
      declare
         Project_Tree : GNATCOLL.Projects.Project_Tree;
      begin
         Start_Time := Ada.Real_Time.Clock;

         --  Load project tree from command line argument.
         --  An exception Invalid_Projects may be raised by this call, this is
         --  handled below.
         Project_Tree.Load
           (Root_Project_Path =>
              GNATCOLL.VFS.Create (Full_Filename => GPR_File));

         Log.Debug
           (Message =>
              "GNAT project loaded in " &
              Image (Value =>
                       Ada.Real_Time.To_Duration
                         (TS => Ada.Real_Time.Clock - Start_Time)) &
              ".");

         Start_Time := Ada.Real_Time.Clock;

         Load_Source_Files :
         declare
            --  Retrieve all project files recursively.
            Project_Files : GNATCOLL.VFS.File_Array_Access :=
              Project_Tree.Root_Project.Source_Files (Recursive => True);
         begin
            for F of Project_Files.all loop
               --  TODO: We should probably check the language of the file here,
               --        if it's not Ada, we can likely skip it.
               Add_SPARK_File :
               declare
                  --  Translate source file name into it's .spark counterpart.
                  SPARK_Name : constant String :=
                    GPR_Support.SPARK_Name (Project_Tree => Project_Tree,
                                            Source_File  => F);
               begin
                  Log.Debug
                    (Message  =>
                       "Found """ & F.Display_Base_Name &
                       """, checking for """ & SPARK_Name & """...",
                     New_Line => False);

                  declare
                     File_Name : constant SPAT.Subject_Name :=
                       SPAT.To_Name (SPARK_Name);
                  begin
                     --  Prevent adding the same file twice. Above we retrieve
                     --  all files from the project, hence in most cases we will
                     --  encounter both a spec and a body file which will still
                     --  result in the same .spark file.
                     if not Raw_List.Contains (Item => File_Name) then
                        Raw_List.Append (New_Item => File_Name);

                        --  This was a new file, so if it exists on disk, add it
                        --  to the result list.
                        if Ada.Directories.Exists (Name => SPARK_Name) then
                           Result_List.Append (New_Item => File_Name);

                           Log.Debug (Message => "added to index.");
                        else
                           Log.Debug (Message => "not found on disk, skipped.");
                        end if;
                     else
                        Log.Debug (Message => "already in index.");
                     end if;
                  end;
               end Add_SPARK_File;
            end loop;

            GNATCOLL.VFS.Unchecked_Free (Arr => Project_Files);
         end Load_Source_Files;

         Project_Tree.Unload;
      exception
         when GNATCOLL.Projects.Invalid_Project =>
            Log.Error
              (Message =>
                 "Could not load """ & GNATCOLL.VFS."+" (GPR_File) & """!");
      end Load_Project_Files;

      Report_Timing :
      declare
         Num_Files : constant Ada.Containers.Count_Type := Result_List.Length;
         use type Ada.Containers.Count_Type;
      begin
         Log.Debug
           (Message =>
              "Search completed in " &
              Image (Value =>
                       Ada.Real_Time.To_Duration
                         (TS => Ada.Real_Time.Clock - Start_Time)) &
              "," & Num_Files'Image & " file" &
              (if Num_Files /= 1
               then "s"
               else "") & " found so far.");
      end Report_Timing;

      return Result_List;
   end Get_SPARK_Files;

   function SPARK_Name
     (Project_Tree : in GNATCOLL.Projects.Project_Tree;
      Source_File  : in GNATCOLL.VFS.Virtual_File) return String
   is
      use type GNATCOLL.VFS.Filesystem_String;

      File_Info : constant GNATCOLL.Projects.File_Info :=
        Project_Tree.Info (File => Source_File);
      Object_Directory : constant String :=
        +GNATCOLL.Projects.Object_Dir (Project => File_Info.Project).Full_Name.all;
   begin
      --  .spark files seem to reside in the "gnatprove" subdirectory of the
      --  object directory defined by the project.  As files might come from
      --  different projects, we query the project associated to the file and
      --  then use this project's object directory instead of using the object
      --  directory from the root project.
      --  This is untested, so maybe we should create some dummy projects to
      --  test this assumption.
      --  TODO: Also, aggregate projects
      --        (GNATCOLL.Projects.Is_Aggregate_Project) do not support the
      --        File_Info call, so there's still something to be done here...
      return
        Ada.Directories.Compose
          (Containing_Directory =>
             Ada.Directories.Compose
               (Containing_Directory => Object_Directory,
                Name                 => "gnatprove"),
           Name                 =>
             Ada.Directories.Base_Name
               (Name => Source_File.Display_Full_Name),
           Extension            => "spark");
   end SPARK_Name;

end SPAT.GPR_Support;
