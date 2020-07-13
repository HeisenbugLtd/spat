------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Containers.Hashed_Sets;
with Ada.Directories;

with GNATCOLL.Projects;
with SPAT.Log;
with SPAT.Stop_Watch;

package body SPAT.GPR_Support is

   package File_Name_Caches is new
     Ada.Containers.Hashed_Sets
       (Element_Type        => SPARK_File_Name,
        Hash                => SPAT.Hash,
        Equivalent_Elements => "=");

   ---------------------------------------------------------------------------
   --  Add_File
   ---------------------------------------------------------------------------
   procedure Add_File (Project_Tree : in     GNATCOLL.Projects.Project_Tree;
                       Source_File  : in     GNATCOLL.VFS.Virtual_File;
                       Cache        : in out File_Name_Caches.Set;
                       To           : in out SPARK_Source_Maps.Map);

   ---------------------------------------------------------------------------
   --  To_SPARK_Name
   ---------------------------------------------------------------------------
   function To_SPARK_Name
     (Project_Tree : in GNATCOLL.Projects.Project_Tree;
      Source_File  : in GNATCOLL.VFS.Virtual_File) return String;

   ---------------------------------------------------------------------------
   --  Prefer_Body_File
   --
   --  Checks if for the given key a mapping already exists and updates it if
   --  the given Value seems a better fit (i.e. is a body file).
   --  Apparently steps and timeout values should be given to the body file,
   --  if they are specified for the spec, proofs seem to fail, see
   --  https://github.com/HeisenbugLtd/spat/issues/55#issuecomment-657505781
   ---------------------------------------------------------------------------
   procedure Prefer_Body_File
     (File_Map     : in out SPARK_Source_Maps.Map;
      Project_Tree : in     GNATCOLL.Projects.Project_Tree;
      Key          : in     SPARK_File_Name;
      Value        : in     GNATCOLL.VFS.Virtual_File);

   ---------------------------------------------------------------------------
   --  Add_File
   ---------------------------------------------------------------------------
   procedure Add_File (Project_Tree : in     GNATCOLL.Projects.Project_Tree;
                       Source_File  : in     GNATCOLL.VFS.Virtual_File;
                       Cache        : in out File_Name_Caches.Set;
                       To           : in out SPARK_Source_Maps.Map)
   is
      SPARK_Name   : constant String :=
        To_SPARK_Name (Project_Tree => Project_Tree,
                       Source_File  => Source_File);
      As_File_Name : constant SPARK_File_Name :=
        SPARK_File_Name (To_Name (Source => SPARK_Name));
      Simple_Name  : constant String :=
        Ada.Directories.Simple_Name (Name => SPARK_Name);
      Exists       : constant Boolean :=
        Ada.Directories.Exists (Name => SPARK_Name);
      Dummy_Cursor : File_Name_Caches.Cursor; --  Don't care about the position.
      Inserted     : Boolean;
   begin
      --  Prevent adding the same file twice. The caller retrieves all files
      --  from the project, hence in most cases we will encounter both a spec
      --  and a body file which will still result in the same .spark file.
      Cache.Insert (New_Item => As_File_Name,
                    Position => Dummy_Cursor,
                    Inserted => Inserted);

      Log.Debug (Message  =>
                   """" & Simple_Name & """ " &
                   (if Inserted
                    then "added to"
                    else "already in") & " index.");

      --  If the .spark file exists, add it to the result map, possibly
      --  updating the file mapping as we do prefer the spec file.
      if Exists then
         Prefer_Body_File (File_Map     => To,
                           Project_Tree => Project_Tree,
                           Key          => As_File_Name,
                           Value        => Source_File);
      else
         Log.Debug
           (Message =>
              """" & Simple_Name & """ not found on disk, skipped.");
      end if;
   end Add_File;

   ---------------------------------------------------------------------------
   --  Get_SPARK_Files
   ---------------------------------------------------------------------------
   function Get_SPARK_Files
     (GPR_File : GNATCOLL.VFS.Filesystem_String) return SPARK_Source_Maps.Map
   is
      Timer : Stop_Watch.T := Stop_Watch.Create;

      Project_Tree : GNATCOLL.Projects.Project_Tree;
   begin
      Load_Project :
      begin
         Timer.Start; --  Start lap measurement.

         Project_Tree.Load
           (Root_Project_Path =>
              GNATCOLL.VFS.Create (Full_Filename => GPR_File));

         Log.Debug (Message => "GNAT project loaded in " & Timer.Elapsed & ".");
      exception
         when GNATCOLL.Projects.Invalid_Project =>
            Log.Error
              (Message =>
                  "Could not load """ & GNATCOLL.VFS."+" (GPR_File) & """!");
            return SPARK_Source_Maps.Empty_Map;
      end Load_Project;

      Timer.Start; --  Next lap measurement

      Load_Source_Files :
      declare
         --  Retrieve all project files recursively.
         Project_Files : GNATCOLL.VFS.File_Array_Access :=
           Project_Tree.Root_Project.Source_Files (Recursive => True);

         --  Get maximum number of entries we may encounter.
         Capacity : constant Ada.Containers.Count_Type :=
           Project_Files.all'Length;

         --  Initialize the lists.
         Raw_List    : File_Name_Caches.Set;
         --  Stores all encountered files.
         Result_List : SPARK_Source_Maps.Map;
         --  Stores only files that exist on disk.
      begin
         Raw_List.Reserve_Capacity (Capacity => Capacity);

         for F of Project_Files.all loop
            --  TODO: We should probably check the language of the file here, if
            --        it's not Ada, we can likely skip it.
            Log.Debug
              (Message  => "Found """ & F.Display_Base_Name & """...",
               New_Line => False);

            Add_File (Project_Tree => Project_Tree,
                      Source_File  => F,
                      Cache        => Raw_List,
                      To           => Result_List);
         end loop;

         --  Cleanup.
         GNATCOLL.VFS.Unchecked_Free (Arr => Project_Files);
         Project_Tree.Unload;

         Report_Timing :
         declare
            Num_Files : constant Ada.Containers.Count_Type :=
              Result_List.Length;
            use type Ada.Containers.Count_Type;
         begin
            Log.Debug
              (Message =>
                 "Search completed in " & Timer.Elapsed &
                 "," & Num_Files'Image & " file" &
               (if Num_Files /= 1 then "s" else "") & " found.");
         end Report_Timing;

         return Result_List;
      end Load_Source_Files;
   end Get_SPARK_Files;

   ---------------------------------------------------------------------------
   --  Prefer_Body_File
   ---------------------------------------------------------------------------
   procedure Prefer_Body_File
     (File_Map     : in out SPARK_Source_Maps.Map;
      Project_Tree : in     GNATCOLL.Projects.Project_Tree;
      Key          : in     SPARK_File_Name;
      Value        : in     GNATCOLL.VFS.Virtual_File)
   is
      use all type GNATCOLL.Projects.Unit_Parts;
   begin
      if not File_Map.Contains (Key => Key) then
         --  Not yet in map, insert unconditionally.
         File_Map.Insert
           (Key      => Key,
            New_Item => Source_File_Name (To_Name (Value.Display_Base_Name)));
      elsif Project_Tree.Info (File => Value).Unit_Part = Unit_Body then
         --  We already have an entry, but this is (a/the) body, we prefer that.
         File_Map.Include
           (Key      => Key,
            New_Item => Source_File_Name (To_Name (Value.Display_Base_Name)));
      end if;
   end Prefer_Body_File;

   ---------------------------------------------------------------------------
   --  SPARK_Name
   ---------------------------------------------------------------------------
   function To_SPARK_Name
     (Project_Tree : in GNATCOLL.Projects.Project_Tree;
      Source_File  : in GNATCOLL.VFS.Virtual_File) return String
   is
      use type GNATCOLL.VFS.Filesystem_String;

      File_Info : constant GNATCOLL.Projects.File_Info :=
        Project_Tree.Info (File => Source_File);
      Object_Directory : constant String :=
        +GNATCOLL.Projects.Object_Dir (Project => File_Info.Project).Full_Name.all;
   begin
      --  .spark files seem to reside in the "gnatprove" sub-directory of the
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
   end To_SPARK_Name;

end SPAT.GPR_Support;
