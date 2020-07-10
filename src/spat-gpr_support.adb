------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Containers.Indefinite_Hashed_Sets;
with Ada.Directories;
with Ada.Real_Time;
with Ada.Strings.Hash;

with GNATCOLL.Projects;
with SPAT.Log;

package body SPAT.GPR_Support is

   package File_Name_Caches is new
     Ada.Containers.Indefinite_Hashed_Sets
       (Element_Type        => String,
        Hash                => Ada.Strings.Hash,
        Equivalent_Elements => "=");
   --  The Add_File subroutine below receives the filename as a string, so I
   --  decided to use these as set elements, hence we need to use the indefinite
   --  version of Hashed_Sets.
   --  The reasoning behind that is that even though storing the
   --  Unbounded_String would probably require less memory allocations while
   --  adding elements to the set, that also happens at the cost of an extra
   --  conversion back to String when the runtime calculates the hash. While
   --  secondary stack is relatively cheap (at least in comparison to a memory
   --  allocator), the additional copy of the actual string probably still beats
   --  that. Also, the Hashing must be done twice when inserting an element,
   --  while we expect only about half of actual insertions.

   ---------------------------------------------------------------------------
   --  Add_File
   ---------------------------------------------------------------------------
   procedure Add_File (Name  : in     String;
                       Cache : in out File_Name_Caches.Set;
                       To    : in out SPARK_Source_Maps.Map);

   ---------------------------------------------------------------------------
   --  SPARK_Name
   ---------------------------------------------------------------------------
   function SPARK_Name
     (Project_Tree : in GNATCOLL.Projects.Project_Tree;
      Source_File  : in GNATCOLL.VFS.Virtual_File) return String;

   ---------------------------------------------------------------------------
   --  Add_File
   ---------------------------------------------------------------------------
   procedure Add_File (Name  : in     String;
                       Cache : in out File_Name_Caches.Set;
                       To    : in out SPARK_Source_Maps.Map)
   is
      As_File_Name : constant SPARK_File_Name :=
        SPARK_File_Name (To_Name (Source => Name));
      Simple_Name  : constant String :=
        Ada.Directories.Simple_Name (Name => Name);
      Dummy_Cursor : File_Name_Caches.Cursor; --  Don't care about the position.
      Inserted     : Boolean;
   begin
      --  Prevent adding the same file twice. The caller retrieves all files
      --  from the project, hence in most cases we will encounter both a spec
      --  and a body file which will still result in the same .spark file.
      Cache.Insert (New_Item => Name, --  Original input.
                    Position => Dummy_Cursor,
                    Inserted => Inserted);

      if Inserted then
         --  This was a new file, so if it exists on disk, add it to the result
         --  list.
         if Ada.Directories.Exists (Name => Name) then
            To.Include (Key      => As_File_Name,
                        New_Item => Source_File_Name (To_Name (Name)));
            Log.Debug (Message => """" & Simple_Name & """ added to index.");
         else
            Log.Debug
              (Message =>
                 """" & Simple_Name & """ not found on disk, skipped.");
         end if;
      else
         Log.Debug (Message => """" & Simple_Name & """ already in index.");
      end if;
   end Add_File;

   ---------------------------------------------------------------------------
   --  Get_SPARK_Files
   ---------------------------------------------------------------------------
   function Get_SPARK_Files
     (GPR_File : GNATCOLL.VFS.Filesystem_String) return SPARK_Source_Maps.Map
   is
      Start_Time  : Ada.Real_Time.Time;

      ------------------------------------------------------------------------
      --  Elapsed_Time
      ------------------------------------------------------------------------
      function Elapsed_Time return String;

      ------------------------------------------------------------------------
      --  Elapsed_Time
      ------------------------------------------------------------------------
      function Elapsed_Time return String is
         use type Ada.Real_Time.Time;
         Elapsed : constant Duration :=
           Ada.Real_Time.To_Duration (TS => Ada.Real_Time.Clock - Start_Time);
      begin
         return Image (Value => Elapsed);
      end Elapsed_Time;

      Project_Tree : GNATCOLL.Projects.Project_Tree;
   begin
      Load_Project :
      begin
         Start_Time := Ada.Real_Time.Clock; --  Reset measurement.

         Project_Tree.Load
           (Root_Project_Path =>
              GNATCOLL.VFS.Create (Full_Filename => GPR_File));

         Log.Debug (Message => "GNAT project loaded in " & Elapsed_Time & ".");
      exception
         when GNATCOLL.Projects.Invalid_Project =>
            Log.Error
              (Message =>
                  "Could not load """ & GNATCOLL.VFS."+" (GPR_File) & """!");
            return SPARK_Source_Maps.Empty_Map;
      end Load_Project;

      Start_Time := Ada.Real_Time.Clock;

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

            Add_File (Name  => SPARK_Name (Project_Tree => Project_Tree,
                                           Source_File  => F),
                      Cache => Raw_List,
                      To    => Result_List);
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
                 "Search completed in " & Elapsed_Time &
                 "," & Num_Files'Image & " file" &
               (if Num_Files /= 1 then "s" else "") & " found.");
         end Report_Timing;

         return Result_List;
      end Load_Source_Files;
   end Get_SPARK_Files;

   ---------------------------------------------------------------------------
   --  SPARK_Name
   ---------------------------------------------------------------------------
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
   end SPARK_Name;

end SPAT.GPR_Support;
