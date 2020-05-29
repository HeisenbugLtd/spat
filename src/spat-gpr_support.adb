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
with Ada.Text_IO;

with GNATCOLL.Projects;
with SI_Units.Metric;
with SI_Units.Names;
with SPAT.Command_Line;
with SPAT.File_Ops;

package body SPAT.GPR_Support is

   function Image is new
     SI_Units.Metric.Fixed_Image (Item        => Duration,
                                  Default_Aft => 0,
                                  Unit        => SI_Units.Names.Second);

   function SPARK_Name
     (Project_Tree : in GNATCOLL.Projects.Project_Tree;
      Source_File  : in GNATCOLL.VFS.Virtual_File) return String;

   function Get_SPARK_Files
     (GPR_File : GNATCOLL.VFS.Filesystem_String) return File_Ops.File_List
   is
      Verbose    : constant Boolean := SPAT.Command_Line.Verbose.Get;
      Start_Time : Ada.Real_Time.Time;
      File_List  : SPAT.File_Ops.File_List;

      use type Ada.Real_Time.Time;
   begin
      Load_Project_Files :
      declare
         Project_Tree : GNATCOLL.Projects.Project_Tree;
      begin
         if Verbose then
            Start_Time := Ada.Real_Time.Clock;
         end if;

         --  Load project tree from command line argument.
         --  An exception Invalid_Projects may be raised by this call, this is
         --  handled below.
         Project_Tree.Load
           (Root_Project_Path =>
              GNATCOLL.VFS.Create (Full_Filename => GPR_File));

         if Verbose then
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Output,
               Item => "GNAT project loaded in " &
                 Image (Value =>
                          Ada.Real_Time.To_Duration
                            (TS => Ada.Real_Time.Clock - Start_Time)) &
                 ".");
         end if;

         if Verbose then
            Start_Time := Ada.Real_Time.Clock;
         end if;

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
                  if Verbose then
                     Ada.Text_IO.Put_Line ("Found """ & F.Display_Base_Name &
                                             """, checking for """ &
                                             SPARK_Name & """...");
                  end if;

                  declare
                     File_Name : constant SPAT.Subject_Name :=
                       SPAT.To_Name (SPARK_Name);
                  begin
                     --  Prevent adding the same file twice. Above we retrieve
                     --  all files from the project, hence in most cases we will
                     --  encounter both a spec and a body file which will still
                     --  result in the same .spark file.
                     if not File_List.Contains (Item => File_Name) then
                        File_List.Append (New_Item => File_Name);
                     end if;
                  end;
               end Add_SPARK_File;
            end loop;

            GNATCOLL.VFS.Unchecked_Free (Arr => Project_Files);
         end Load_Source_Files;

         Project_Tree.Unload;

         --  To prevent returning files for which no .spark file was found, we
         --  now remove files from the list that do not exist.  This could have
         --  been done when adding files in the loop above, but this approach
         --  could significantly increase the amount of actual file system
         --  operations (two files for spec/body, no .spark file exists, check
         --  is done for the same file twice). It seems better to optimize the
         --  slow operations, especially on file systems which may likely be
         --  remote or virtualized.
         for I in reverse 1 .. Integer (File_List.Length) loop
            --  Do it backwards, otherwise we will mess with the indexing.
            if not Ada.Directories.Exists (To_String (File_List.Element (I))) then
               File_List.Delete (Index => I);
            end if;
         end loop;
      exception
         when GNATCOLL.Projects.Invalid_Project =>
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item =>
                  "Error: Could not load """ &
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

      return File_List;
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
