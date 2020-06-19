------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Directories;
with Ada.Strings.Fixed;

with SPAT.Entity_Line;
with SPAT.Entity_Location;
with SPAT.Field_Names;
with SPAT.Flow_Item;
with SPAT.Log;
with SPAT.Preconditions;
with SPAT.Proof_Attempt;
with SPAT.Proof_Item;
with SPAT.Strings;

package body SPAT.Spark_Info is

   use type Ada.Containers.Count_Type;
   use type Analyzed_Entities.Cursor;

   --  Make JSON type enumeration literals directly visible.
   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Subprogram prototypes.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Get_Sentinel (Flows)
   ---------------------------------------------------------------------------
   function Get_Sentinel (Node : in Analyzed_Entities.Constant_Reference_Type)
                          return Flows_Sentinel;

   ---------------------------------------------------------------------------
   --  Get_Sentinel (Proofs)
   ---------------------------------------------------------------------------
   function Get_Sentinel (Node : in Analyzed_Entities.Constant_Reference_Type)
                          return Proofs_Sentinel;

   ---------------------------------------------------------------------------
   --  Guess_Version
   --
   --  Checks for presence of certain fields that are presumed version
   --  specific.
   ---------------------------------------------------------------------------
   function Guess_Version (Root : in JSON_Value) return File_Version;

   ---------------------------------------------------------------------------
   --  Map_Assumptions_Elements
   ---------------------------------------------------------------------------
   procedure Map_Assumptions_Elements (This : in out T;
                                       Root : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Entities
   ---------------------------------------------------------------------------
   procedure Map_Entities (This      : in out T;
                           Root      : in     JSON_Value;
                           From_File : in     File_Sets.Cursor) with
     Pre => (Preconditions.Ensure_Field (Object => Root,
                                         Field  => Field_Names.Name,
                                         Kind   => JSON_String_Type) and then
             Preconditions.Ensure_Field (Object => Root,
                                         Field  => Field_Names.Sloc,
                                         Kind   => JSON_Array_Type));

   ---------------------------------------------------------------------------
   --  Map_Flow_Elements
   ---------------------------------------------------------------------------
   procedure Map_Flow_Elements (This : in out T;
                                Root : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Proof_Elements
   ---------------------------------------------------------------------------
   procedure Map_Proof_Elements
     (This         : in out T;
      Version      : in     File_Version;
      Root         : in     JSON_Array;
      Cache_Cursor : in     File_Cached_Info.Cursor);

   ---------------------------------------------------------------------------
   --  Map_Sloc_Elements
   ---------------------------------------------------------------------------
   procedure Map_Sloc_Elements (This     : in out T;
                                The_Tree : in out Entity.Tree.T;
                                Position : in     Entity.Tree.Cursor;
                                Root     : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Spark_Elements
   ---------------------------------------------------------------------------
   procedure Map_Spark_Elements (This      : in out T;
                                 Root      : in     JSON_Array;
                                 From_File : in     File_Sets.Cursor);

   ---------------------------------------------------------------------------
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This    : in out T;
                          File    : in     SPARK_File_Name;
                          Root    : in     JSON_Value;
                          Version : in     File_Version);

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Name
   --
   --  Sort code entities by their name.
   --  Sorting: alphabetical, ascending
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Name (This      : in     T;
                                  Container : in out Strings.Entity_Names);

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Proof_Time
   --
   --  Sort code entities by how much total proof time they required.
   --  Sorting: numerical, descending
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Proof_Time
     (This      : in     T;
      Container : in out Strings.Entity_Names);

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Success_Time
   --
   --  Sort code entities by how much maximum time for a successful proofs.
   --  Sorting: numerical, descending
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Success_Time
     (This      : in     T;
      Container : in out Strings.Entity_Names);

   ---------------------------------------------------------------------------
   --  Sort_File_By_Basename
   --
   --  Sort files by their base name (i.e. without containing directory or
   --  extension).
   --  Sorting: alphabetical, ascending
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Basename
     (This      : in     T;
      Container : in out Strings.SPARK_File_Names);

   ---------------------------------------------------------------------------
   --  Sort_File_By_Proof_Time
   --
   --  Sort files by how much total time it required to spend in flow analysis
   --  and proof.
   --  Sorting: numerical, descending
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Proof_Time
     (This      : in     T;
      Container : in out Strings.SPARK_File_Names);

   ---------------------------------------------------------------------------
   --  Subprogram implementations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Flow_Time
   ---------------------------------------------------------------------------
   not overriding
   function Flow_Time (This : in T;
                       File : in SPARK_File_Name) return Duration is
     (This.Timings (File).Flow);

   ---------------------------------------------------------------------------
   --  Get_Sentinel (Flows)
   ---------------------------------------------------------------------------
   function Get_Sentinel (Node : in Analyzed_Entities.Constant_Reference_Type)
                          return Flows_Sentinel
   is
     (Flows_Sentinel (SPAT.Entity.Tree.Element (Position => Node.Flows)));

   ---------------------------------------------------------------------------
   --  Get_Sentinel (Proofs)
   ---------------------------------------------------------------------------
   function Get_Sentinel (Node : in Analyzed_Entities.Constant_Reference_Type)
                          return Proofs_Sentinel
   is
     (Proofs_Sentinel (SPAT.Entity.Tree.Element (Position => Node.Proofs)));

   ---------------------------------------------------------------------------
   --  Guess_Version
   ---------------------------------------------------------------------------
   function Guess_Version (Root : in JSON_Value) return File_Version
   is
      Result : File_Version := GNAT_CE_2019;
   begin
      if Root.Has_Field (Field => Field_Names.Session_Map) then
         --  "session_map" seems new in CE 2020.
         Result := GNAT_CE_2020;
      elsif Root.Has_Field (Field => Field_Names.Timings) then
         declare
            Timings : constant GNATCOLL.JSON.JSON_Value :=
              Root.Get (Field => Field_Names.Timings);
         begin
            if Timings.Has_Field (Field => Field_Names.Translation_Of_Compilation_Unit) then
               --  This field seems to have disappeared in GNAT CE 2020, so if
               --  it is present, we assume GNAT CE 2019.
               Result := GNAT_CE_2019;
            else
               Result := GNAT_CE_2020;
            end if;
         end;
      end if;

      Log.Debug (Message => "File version: " & Result'Image);

      return Result;
   end Guess_Version;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This   : in T;
                                 Entity : in Entity_Name) return Boolean is
      Reference : constant Analyzed_Entities.Constant_Reference_Type :=
        This.Entities.Constant_Reference (Key => Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      return Sentinel.Cache.Has_Failed_Attempts;
   end Has_Failed_Attempts;

   ---------------------------------------------------------------------------
   --  Has_Unjustified_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Unjustified_Attempts
     (This   : in T;
      Entity : in Entity_Name) return Boolean
   is
      Reference : constant Analyzed_Entities.Constant_Reference_Type :=
        This.Entities.Constant_Reference (Key => Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      return Sentinel.Cache.Has_Unjustified_Attempts;
   end Has_Unjustified_Attempts;

   ---------------------------------------------------------------------------
   --  Has_Unproved_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Unproved_Attempts (This   : in T;
                                   Entity : in Entity_Name) return Boolean is
      Reference : constant Analyzed_Entities.Constant_Reference_Type :=
        This.Entities.Constant_Reference (Key => Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      return Sentinel.Cache.Has_Unproved_Attempts;
   end Has_Unproved_Attempts;

   ---------------------------------------------------------------------------
   --  Iterate_Children
   ---------------------------------------------------------------------------
   not overriding
   function Iterate_Children (This     : in T;
                              Entity   : in Entity_Name;
                              Position : in SPAT.Entity.Tree.Cursor)
                              return SPAT.Entity.Tree.Forward_Iterator'Class is
   begin
      return
        This.Entities (Entity).The_Tree.Iterate_Children (Parent => Position);
   end Iterate_Children;

   ---------------------------------------------------------------------------
   --  List_All_Entities
   ---------------------------------------------------------------------------
   not overriding
   function List_All_Entities
     (This    : in T;
      Sort_By : in Sorting_Criterion := Name) return Strings.Entity_Names is
   begin
      return Result : Strings.Entity_Names (Capacity => This.Entities.Length) do
         for Position in This.Entities.Iterate loop
            Result.Append
              (New_Item => Analyzed_Entities.Key (Position => Position));
         end loop;

         case Sort_By is
            when None =>
               null;

            when Name =>
               This.Sort_Entity_By_Name (Container => Result);

            when Max_Success_Time =>
               This.Sort_Entity_By_Success_Time (Container => Result);

            when Max_Time =>
               This.Sort_Entity_By_Proof_Time (Container => Result);
         end case;
      end return;
   end List_All_Entities;

   ---------------------------------------------------------------------------
   --  List_All_Files
   ---------------------------------------------------------------------------
   not overriding
   function List_All_Files
     (This    : in T;
      Sort_By : in Sorting_Criterion := None) return Strings.SPARK_File_Names is
   begin
      return
        Result : Strings.SPARK_File_Names (Capacity => This.Files.Length)
      do
         for File of This.Files loop
            Result.Append (New_Item => File);
         end loop;

         case Sort_By is
            when None =>
               null;

            when Name =>
               This.Sort_File_By_Basename (Container => Result);

            when Max_Success_Time | Max_Time =>
               Log.Warning
                 (Message =>
                     "Sorting files by success time not implemented yet, " &
                     "falling back to maximum time.");
               This.Sort_File_By_Proof_Time (Container => Result);
         end case;
      end return;
   end List_All_Files;

   ---------------------------------------------------------------------------
   --  Map_Assumptions_Elements
   ---------------------------------------------------------------------------
   procedure Map_Assumptions_Elements (This : in out T;
                                       Root : in     JSON_Array) is
   begin
      --  TODO: Add all elements from the "assumptions" array.
      null;
   end Map_Assumptions_Elements;

   ---------------------------------------------------------------------------
   --  Map_Entities
   ---------------------------------------------------------------------------
   procedure Map_Entities (This      : in out T;
                           Root      : in     JSON_Value;
                           From_File : in     File_Sets.Cursor)
   is
      Obj_Name : constant Entity_Name :=
        Entity_Name (Subject_Name'(Root.Get (Field => Field_Names.Name)));
      Slocs    : constant JSON_Array  := Root.Get (Field => Field_Names.Sloc);
      Index    :          Analyzed_Entities.Cursor :=
        This.Entities.Find (Key => Obj_Name);
   begin
      if Index = Analyzed_Entities.No_Element then
         declare
            Element        : Analyzed_Entity;
            Dummy_Inserted : Boolean;
         begin
            Element.SPARK_File := From_File;

            This.Entities.Insert (Key      => Obj_Name,
                                  New_Item => Element,
                                  Position => Index,
                                  Inserted => Dummy_Inserted);
         end;
      end if;

      declare
         Reference : constant Analyzed_Entities.Reference_Type :=
           This.Entities.Reference (Position => Index);
         use type Entity.Tree.Cursor;
      begin
         --  Add sentinel(s) if not yet present.
         if Reference.Source_Lines = Entity.Tree.No_Element then
            Reference.The_Tree.Insert_Child
              (Parent   => Reference.The_Tree.Root,
               Before   => Entity.Tree.No_Element,
               New_Item => Source_Lines_Sentinel'(Entity.T with null record),
               Position => Reference.Source_Lines);
         end if;

         if Reference.Flows = Entity.Tree.No_Element then
            Reference.The_Tree.Insert_Child
              (Parent   => Reference.The_Tree.Root,
               Before   => Entity.Tree.No_Element,
               New_Item => Empty_Flows_Sentinel,
               Position => Reference.Flows);
         end if;

         if Reference.Proofs = Entity.Tree.No_Element then
            Reference.The_Tree.Insert_Child
              (Parent   => Reference.The_Tree.Root,
               Before   => Entity.Tree.No_Element,
               New_Item => Empty_Proofs_Sentinel,
               Position => Reference.Proofs);
         end if;

         This.Map_Sloc_Elements (Root     => Slocs,
                                 The_Tree => Reference.The_Tree,
                                 Position => Reference.Source_Lines);
      end;
   end Map_Entities;

   ---------------------------------------------------------------------------
   --  Map_Flow_Elements
   ---------------------------------------------------------------------------
   procedure Map_Flow_Elements (This : in out T;
                                Root : in     JSON_Array) is
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Element : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                                Index => I);
         begin
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Entity,
                                          Kind   => JSON_Object_Type)
            then
               declare
                  Source_Entity : constant JSON_Value :=
                    Element.Get (Field => Field_Names.Entity);
               begin
                  --  The name referenced here should match a name we already
                  --  have in the hash table.
                  if
                    Preconditions.Ensure_Field (Object => Source_Entity,
                                                Field  => Field_Names.Name,
                                                Kind   => JSON_String_Type)
                  then
                     declare
                        The_Key : constant Entity_Name :=
                          Entity_Name
                            (Subject_Name'
                               (Source_Entity.Get (Field => Field_Names.Name)));
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Flow_Item.Has_Required_Fields (Object => Element)
                           then
                              declare
                                 Reference : constant
                                   Analyzed_Entities.Reference_Type :=
                                     This.Entities.Reference
                                       (Position => Update_At);
                              begin
                                 Reference.The_Tree.Append_Child
                                   (Parent   => Reference.Flows,
                                    New_Item =>
                                      Flow_Item.Create (Object => Element));
                              end;
                           end if;
                        else
                           Log.Warning
                             (Message =>
                                "flow: """ & To_String (Source => The_Key) &
                                """ not found in index.");
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Sort flows by file name:line:column.
      for E of This.Entities loop
         SPAT.Flow_Item.Sort_By_Location (This   => E.The_Tree,
                                          Parent => E.Flows);
      end loop;
   end Map_Flow_Elements;

   ---------------------------------------------------------------------------
   --  Map_Proof_Elements
   ---------------------------------------------------------------------------
   procedure Map_Proof_Elements
     (This         : in out T;
      Version      : in     File_Version;
      Root         : in     JSON_Array;
      Cache_Cursor : in     File_Cached_Info.Cursor)
   is
      ------------------------------------------------------------------------
      --  Update_Caches
      ------------------------------------------------------------------------
      procedure Update_Caches
        (Reference : in Analyzed_Entities.Reference_Type);

      ------------------------------------------------------------------------
      --  Update_Caches
      ------------------------------------------------------------------------
      procedure Update_Caches
        (Reference : in Analyzed_Entities.Reference_Type)
      is
         N : constant Proof_Item.T :=
           Proof_Item.T
             (Entity.T'Class'
                (Reference.The_Tree
                   (Entity.Tree.Last_Child (Position => Reference.Proofs))));
      begin
         Update_Sentinel :
         declare
            ------------------------------------------------------------------
            --  Local_Update
            ------------------------------------------------------------------
            procedure Local_Update (Element : in out Entity.T'Class);

            ------------------------------------------------------------------
            --  Local_Update
            ------------------------------------------------------------------
            procedure Local_Update (Element : in out Entity.T'Class) is
               S : Proofs_Sentinel renames Proofs_Sentinel (Element);
            begin
               --  Update sentinel (proof list specific).
               S.Cache :=
                 Proof_Cache'(Max_Proof_Time =>
                                Duration'Max (S.Cache.Max_Proof_Time,
                                              N.Max_Time),
                              Max_Success_Proof_Time =>
                                Duration'Max (S.Cache.Max_Success_Proof_Time,
                                              N.Max_Success_Time),
                              Total_Proof_Time =>
                                S.Cache.Total_Proof_Time + N.Total_Time,
                              Has_Failed_Attempts =>
                                S.Cache.Has_Failed_Attempts or else
                                  N.Has_Failed_Attempts,
                              Has_Unproved_Attempts =>
                                S.Cache.Has_Unproved_Attempts or else
                                  N.Has_Unproved_Attempts,
                              Has_Unjustified_Attempts =>
                                S.Cache.Has_Unjustified_Attempts or else
                                  N.Is_Unjustified);
            end Local_Update;
         begin
            Reference.The_Tree.Update_Element (Position => Reference.Proofs,
                                               Process  => Local_Update'Access);
         end Update_Sentinel;

         Update_File_Cache :
         declare
            ------------------------------------------------------------------
            --  Local_Update
            ------------------------------------------------------------------
            procedure Local_Update (Key     : in     SPARK_File_Name;
                                    Element : in out Cache_Info);

            ------------------------------------------------------------------
            --  Local_Update
            ------------------------------------------------------------------
            procedure Local_Update (Key     : in     SPARK_File_Name;
                                    Element : in out Cache_Info)
            is
               pragma Unreferenced (Key);
            begin
               if N.Has_Unproved_Attempts then
                  null; --  VC is not fully proven, so don't update the max
                        --  time for successful proofs
               else
                  Element.Max_Success_Proof_Time :=
                    Duration'Max (Element.Max_Success_Proof_Time,
                                  N.Max_Success_Time);
               end if;

               Element.Max_Proof_Time := Duration'Max (Element.Max_Proof_Time,
                                                       N.Max_Time);
            end Local_Update;
         begin
            This.Cached.Update_Element (Position => Cache_Cursor,
                                        Process  => Local_Update'Access);
         end Update_File_Cache;
      end Update_Caches;
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Element : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                                Index => I);
         begin
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Entity,
                                          Kind   => JSON_Object_Type)
            then
               declare
                  Source_Entity : constant JSON_Value :=
                    Element.Get (Field => Field_Names.Entity);
               begin
                  --  The name referenced here should match a name we already
                  --  have in the hash table.
                  if
                    Preconditions.Ensure_Field (Object => Source_Entity,
                                                Field  => Field_Names.Name,
                                                Kind   => JSON_String_Type)
                  then
                     declare
                        The_Key : constant Entity_Name :=
                          Entity_Name
                            (Subject_Name'
                               (Source_Entity.Get (Field => Field_Names.Name)));
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Proof_Item.Has_Required_Fields (Object  => Element,
                                                             Version => Version)
                           then
                              declare
                                 Reference : constant
                                   Analyzed_Entities.Reference_Type :=
                                     This.Entities.Reference
                                       (Position => Update_At);
                              begin
                                 Proof_Item.Add_To_Tree
                                   (Object  => Element,
                                    Version => Version,
                                    Tree    => Reference.The_Tree,
                                    Parent  => Reference.Proofs);

                                 --  Update parent sentinel and file info with
                                 --  new proof times.
                                 Update_Caches (Reference => Reference);
                              end;
                           end if;
                        else
                           Log.Warning
                             (Message =>
                                "proof: """ & To_String (Source => The_Key) &
                                """ not found in index.");
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Sort proofs by time to proof them.
      for E of This.Entities loop
         SPAT.Proof_Item.Sort_By_Duration (Tree   => E.The_Tree,
                                           Parent => E.Proofs);
      end loop;
   end Map_Proof_Elements;

   ---------------------------------------------------------------------------
   --  Map_Sloc_Elements
   ---------------------------------------------------------------------------
   procedure Map_Sloc_Elements (This     : in out T;
                                The_Tree : in out Entity.Tree.T;
                                Position : in     Entity.Tree.Cursor;
                                Root     : in     JSON_Array)
   is
      pragma Unreferenced (This);
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Sloc : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                             Index => I);
         begin
            if Entity_Line.Has_Required_Fields (Object => Sloc) then
               The_Tree.Append_Child
                 (Parent   => Position,
                  New_Item => Entity_Line.Create (Object => Sloc));
            end if;
         end;
      end loop;
   end Map_Sloc_Elements;

   ---------------------------------------------------------------------------
   --  Map_Spark_Elements
   ---------------------------------------------------------------------------
   procedure Map_Spark_Elements (This      : in out T;
                                 Root      : in     JSON_Array;
                                 From_File : in     File_Sets.Cursor)
   is
      Length : constant Natural := GNATCOLL.JSON.Length (Arr => Root);
   begin
      This.Entities.Reserve_Capacity
        (Capacity => Ada.Containers.Count_Type (Length));

      for I in 1 .. Length loop
         declare
            Element : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                                Index => I);
         begin
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Name,
                                          Kind   => JSON_String_Type) and then
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Sloc,
                                          Kind   => JSON_Array_Type)
            then
               This.Map_Entities (Root      => Element,
                                  From_File => From_File);
            end if;
         end;
      end loop;
   end Map_Spark_Elements;

   ---------------------------------------------------------------------------
   --  Map_Spark_File
   ---------------------------------------------------------------------------
   not overriding
   procedure Map_Spark_File (This : in out T;
                             File : in     SPARK_File_Name;
                             Root : in     JSON_Value)
   is
      Version      : constant File_Version := Guess_Version (Root => Root);
      Cache_Cursor : File_Cached_Info.Cursor;
      File_Cursor  : File_Sets.Cursor;
   begin
      --  Clear cache data.
      This.Flow_Count  := -1;
      This.Proof_Count := -1;

      --  Establish reference to file. May add it to the Files list if it was
      --  not known yet.
      declare
         Dummy_Inserted : Boolean;
      begin
         This.Files.Insert (New_Item => File,
                            Position => File_Cursor,
                            Inserted => Dummy_Inserted);

         --  Same for the cached information (which may get updated).
         This.Cached.Insert
           (Key => File,
            New_Item => Cache_Info'(Max_Success_Proof_Time => 0.0,
                                    Max_Proof_Time         => 0.0),
            Position => Cache_Cursor,
            Inserted => Dummy_Inserted);
      end;

      --  If I understand the .spark file format correctly, this should
      --  establish the table of all known analysis elements.
      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Spark,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Spark_Elements
           (Root      => Root.Get (Field => Field_Names.Spark),
            From_File => File_Cursor);
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Flow,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Flow_Elements (Root => Root.Get (Field => Field_Names.Flow));
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Proof,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Proof_Elements
           (Root         => Root.Get (Field => Field_Names.Proof),
            Version      => Version,
            Cache_Cursor => Cache_Cursor);
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Assumptions,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Assumptions_Elements
           (Root => Root.Get (Field => Field_Names.Assumptions));
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Timings,
                                    Kind   => JSON_Object_Type)
      then
         --  The "timings" object is version dependent.
         This.Map_Timings (File    => File,
                           Root    => Root.Get (Field => Field_Names.Timings),
                           Version => Version);
      end if;
   exception
      when E : others =>
         Log.Dump_Exception
           (E       => E,
            Message =>
               "Internal problem encountered while mapping JSON objects.",
            File    => To_String (Source => File));
         Log.Warning
           (Message =>
               "Internal error encountered, results will be inaccurate.");
   end Map_Spark_File;

   ---------------------------------------------------------------------------
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This    : in out T;
                          File    : in     SPARK_File_Name;
                          Root    : in     JSON_Value;
                          Version : in     File_Version) is
   begin
      if
        Timing_Item.Has_Required_Fields (Object  => Root,
                                         Version => Version)
      then
         This.Timings.Insert
           (Key      => File,
            New_Item => Timing_Item.Create (Object  => Root,
                                            Version => Version));
      else
         This.Timings.Insert (Key      => File,
                              New_Item => Timing_Item.None);
      end if;
   end Map_Timings;

   ---------------------------------------------------------------------------
   --  Max_Proof_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Proof_Time (This   : in T;
                            Entity : in Entity_Name) return Duration
   is
      Reference : constant Analyzed_Entities.Constant_Reference_Type :=
        This.Entities.Constant_Reference (Key => Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      return Sentinel.Cache.Max_Proof_Time;
   end Max_Proof_Time;

   ---------------------------------------------------------------------------
   --  Max_Proof_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Proof_Time (This : in T;
                            File : in SPARK_File_Name) return Duration is
     (This.Cached (File).Max_Proof_Time);

   ---------------------------------------------------------------------------
   --  Max_Success_Proof_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Success_Proof_Time (This   : in T;
                                    Entity : in Entity_Name) return Duration
   is
      Reference : constant Analyzed_Entities.Constant_Reference_Type :=
        This.Entities.Constant_Reference (Key => Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      return Sentinel.Cache.Max_Success_Proof_Time;
   end Max_Success_Proof_Time;

   ---------------------------------------------------------------------------
   --  Max_Success_Proof_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Success_Proof_Time (This : in T;
                                    File : in SPARK_File_Name) return Duration
   is
     (This.Cached (File).Max_Success_Proof_Time);

   ---------------------------------------------------------------------------
   --  Num_Flows
   ---------------------------------------------------------------------------
   not overriding
   function Num_Flows (This : not null access T)
                       return Ada.Containers.Count_Type is
   begin
      --  Data not yet cached?
      if This.Flow_Count = -1 then
         declare
            Result : Ada.Containers.Count_Type := 0;
         begin
            for E of This.Entities loop
               Result := Result + Entity.Tree.Child_Count (Parent => E.Flows);
            end loop;

            --  Update cache.
            This.Flow_Count := Result;
         end;
      end if;

      return This.Flow_Count;
   end Num_Flows;

   ---------------------------------------------------------------------------
   --  Num_Proofs
   ---------------------------------------------------------------------------
   not overriding
   function Num_Proofs (This : not null access T)
                        return Ada.Containers.Count_Type is
   begin
      --  Data not yet cached?
      if This.Proof_Count = -1 then
         declare
            Result : Ada.Containers.Count_Type := 0;
         begin
            for E of This.Entities loop
               Result := Result + Entity.Tree.Child_Count (Parent => E.Proofs);
            end loop;

            --  Update cache.
            This.Proof_Count := Result;
         end;
      end if;

      return This.Proof_Count;
   end Num_Proofs;

   ---------------------------------------------------------------------------
   --  Print_Trees
   ---------------------------------------------------------------------------
   not overriding
   procedure Print_Trees (This : in T) is
   begin
      if SPAT.Log.Debug_Enabled then
         for Position in This.Entities.Iterate loop
            declare
               Reference : constant Analyzed_Entities.Constant_Reference_Type :=
                 This.Entities.Constant_Reference (Position => Position);
            begin
               for C in
                 SPAT.Entity.Tree.Iterate_Subtree (Position => Reference.Proofs)
               loop
                  declare
                     E : constant Entity.T'Class := Entity.Tree.Element (C);
                     use Ada.Strings.Fixed;
                  begin
                     SPAT.Log.Debug
                       (Natural
                          (SPAT.Entity.Tree.Child_Depth
                             (Reference.Proofs, C)) * ' ' & "[]: " & E.Image);
                  end;
               end loop;
            end;
         end loop;
      end if;
   end Print_Trees;

   ---------------------------------------------------------------------------
   --  Proof_Time
   ---------------------------------------------------------------------------
   not overriding
   function Proof_Time (This : in T;
                        File : in SPARK_File_Name) return Duration
   is
      Timings : constant Timing_Item.T := This.Timings (File);
   begin
      case Timings.Version is
         when GNAT_CE_2019 =>
            --  In this version we have a "proof" timing field, report it
            --  directly.
            return Timings.Proof;

         when GNAT_CE_2020 =>
            declare
               File_Cursor       : constant File_Sets.Cursor :=
                 This.Files.Find (Item => File);
               --  No need to compare filenames, just check the cursor to the
               --  expected file.
               use type File_Sets.Cursor;

               Summed_Proof_Time : Duration := Timings.Proof;
            begin
               --  In this version there's no proof timing field anymore, so we
               --  need to sum the proof times of the entities, too.
               for Position in This.Entities.Iterate loop
                  declare
                     Name : constant Entity_Name :=
                       Analyzed_Entities.Key (Position);
                  begin
                     if
                       Analyzed_Entities.Element
                         (Position).SPARK_File = File_Cursor
                     then
                        Summed_Proof_Time :=
                          Summed_Proof_Time + This.Total_Proof_Time (Name);
                     end if;
                  end;
               end loop;

               return Summed_Proof_Time;
            end;
      end case;
   end Proof_Time;

   ---------------------------------------------------------------------------
   --  Proof_Tree
   ---------------------------------------------------------------------------
   not overriding
   function Proof_Tree (This   : in T;
                        Entity : in Entity_Name)
                        return SPAT.Entity.Tree.Forward_Iterator'Class
   is
      Reference : constant Analyzed_Entities.Constant_Reference_Type :=
        This.Entities.Constant_Reference (Key => Entity);
   begin
      return Reference.The_Tree.Iterate_Children (Parent => Reference.Proofs);
   end Proof_Tree;

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Name
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Name (This      : in     T;
                                  Container : in out Strings.Entity_Names)
   is
      pragma Unreferenced (This);

      package Sorting is new
        Strings.Implementation.Entities.Base_Vectors.Generic_Sorting ("<" => "<");
   begin
      Sorting.Sort
        (Container =>
           Strings.Implementation.Entities.Base_Vectors.Vector (Container));
   end Sort_Entity_By_Name;

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Proof_Time
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Proof_Time (This      : in     T;
                                        Container : in out Strings.Entity_Names)
   is
      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in Entity_Name;
                    Right : in Entity_Name) return Boolean;

      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in Entity_Name;
                    Right : in Entity_Name) return Boolean is
         Left_Total  : constant Duration :=
           This.Total_Proof_Time (Entity => Left);
         Right_Total : constant Duration :=
           This.Total_Proof_Time (Entity => Right);
         Left_Max    : constant Duration :=
           This.Max_Proof_Time (Entity => Left);
         Right_Max   : constant Duration :=
           This.Max_Proof_Time (Entity => Right);
         Left_Success : constant Duration :=
           This.Max_Success_Proof_Time (Entity => Left);
         Right_Success : constant Duration :=
           This.Max_Success_Proof_Time (Entity => Right);
      begin
         --  First by total time.
         if Left_Total /= Right_Total then
            return Left_Total > Right_Total;
         end if;

         --  Total time is the same, try to sort by max time.
         if Left_Max /= Right_Max then
            return Left_Max > Right_Max;
         end if;

         --  Try sorting by max time for successful proof.
         if Left_Success /= Right_Success then
            return Left_Success > Right_Success;
         end if;

         --  Resort to alphabetical order.
         return SPAT."<" (Left, Right); --  Trap! "Left < Right" is recursive.
      end "<";

      package Sorting is new
        Strings.Implementation.Entities.Base_Vectors.Generic_Sorting ("<" => "<");
   begin
      Sorting.Sort
        (Container =>
           Strings.Implementation.Entities.Base_Vectors.Vector (Container));
   end Sort_Entity_By_Proof_Time;

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Success_Time
   --
   --  Sort code entities by how much maximum time for a successful proofs.
   --  Sorting: numerical, descending
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Success_Time
     (This      : in     T;
      Container : in out Strings.Entity_Names)
   is
      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in Entity_Name;
                    Right : in Entity_Name) return Boolean;

      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in Entity_Name;
                    Right : in Entity_Name) return Boolean is
         Left_Success : constant Duration :=
           This.Max_Success_Proof_Time (Entity => Left);
         Right_Success : constant Duration :=
           This.Max_Success_Proof_Time (Entity => Right);
         Left_Total  : constant Duration :=
           This.Total_Proof_Time (Entity => Left);
         Right_Total : constant Duration :=
           This.Total_Proof_Time (Entity => Right);
         Left_Max    : constant Duration :=
           This.Max_Proof_Time (Entity => Left);
         Right_Max   : constant Duration :=
           This.Max_Proof_Time (Entity => Right);
      begin
         --  First by success time.
         if Left_Success /= Right_Success then
            return Left_Success > Right_Success;
         end if;

         --  Total time.
         if Left_Total /= Right_Total then
            return Left_Total > Right_Total;
         end if;

         --  Total time is the same, try to sort by max time.
         if Left_Max /= Right_Max then
            return Left_Max > Right_Max;
         end if;

         --  Resort to alphabetical order.
         return SPAT."<" (Left, Right); --  Trap! "Left < Right" is recursive.
      end "<";

      package Sorting is new
        Strings.Implementation.Entities.Base_Vectors.Generic_Sorting ("<" => "<");
   begin
      Sorting.Sort
        (Container =>
           Strings.Implementation.Entities.Base_Vectors.Vector (Container));
   end Sort_Entity_By_Success_Time;

   ---------------------------------------------------------------------------
   --  By_Basename
   ---------------------------------------------------------------------------
   function By_Basename (Left  : in SPARK_File_Name;
                         Right : in SPARK_File_Name) return Boolean is
     (Ada.Directories.Base_Name (Name => To_String (Source => Left)) <
        Ada.Directories.Base_Name (Name => To_String (Source => Right)));

   package File_Name_Sorting is new
     Strings.Implementation.SPARK_File_Names.Base_Vectors.Generic_Sorting
       ("<" => By_Basename);

   ---------------------------------------------------------------------------
   --  Sort_File_By_Basename
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Basename
     (This      : in     T;
      Container : in out Strings.SPARK_File_Names)
   is
      pragma Unreferenced (This); --  Only provided for consistency.
   begin
      File_Name_Sorting.Sort
        (Container =>
           Strings.Implementation.SPARK_File_Names.Base_Vectors.Vector
             (Container));
   end Sort_File_By_Basename;

   ---------------------------------------------------------------------------
   --  Sort_File_By_Proof_Time
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Proof_Time
     (This      : in     T;
      Container : in out Strings.SPARK_File_Names)
   is
      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in SPARK_File_Name;
                    Right : in SPARK_File_Name) return Boolean;

      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in SPARK_File_Name;
                    Right : in SPARK_File_Name) return Boolean
      is
         Left_Proof  : constant Duration := This.Proof_Time (File => Left);
         Right_Proof : constant Duration := This.Proof_Time (File => Right);
         Left_Flow   : constant Duration := This.Flow_Time (File => Left);
         Right_Flow  : constant Duration := This.Flow_Time (File => Right);
         Left_Total  : constant Duration := Left_Proof + Left_Flow;
         Right_Total : constant Duration := Right_Proof + Right_Flow;
      begin
         --  First by total time.
         if Left_Total /= Right_Total then
            return Left_Total > Right_Total;
         end if;

         --  If totals differ, prioritize proof time.
         if Left_Proof /= Right_Proof then
            return Left_Proof > Right_Proof;
         end if;

         --  Total and proof times were equal, so flow times must be equal, too.
         pragma Assert (Left_Flow = Right_Flow);

         --  Last resort, sort by name.
         return By_Basename (Left  => Left,
                             Right => Right);
      end "<";

      package Sorting is new
        Strings.Implementation.SPARK_File_Names.Base_Vectors.Generic_Sorting
          ("<" => "<");
   begin
      Sorting.Sort
        (Container =>
           Strings.Implementation.SPARK_File_Names.Base_Vectors.Vector
             (Container));
   end Sort_File_By_Proof_Time;

   ---------------------------------------------------------------------------
   --  Total_Proof_Time
   ---------------------------------------------------------------------------
   not overriding
   function Total_Proof_Time (This   : in T;
                              Entity : in Entity_Name) return Duration
   is
      Reference  : constant Analyzed_Entities.Constant_Reference_Type :=
        This.Entities.Constant_Reference (Key => Entity);
      Sentinel   : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      return Sentinel.Cache.Total_Proof_Time;
   end Total_Proof_Time;

   ---------------------------------------------------------------------------
   --  For tests, hardcode the list of known provers.
   type Provers is (Altergo, CVC4, Z3, Unknown);
   --  FIXME: This should become dynamic.

   function To_Prover (Name : in String) return Provers;
   function To_Prover (Name : in String) return Provers is
   begin
      if Name = "altergo" then
         return Altergo;
      elsif Name = "CVC4" then
         return CVC4;
      elsif Name = "Z3" then
         return Z3;
      end if;

      return Unknown;
   end To_Prover;

   type Times is
      record
         Success : Duration;
         Failed  : Duration;
      end record;

   type Time_Per_Prover is array (Provers) of Times;

   package Per_File is new
     Ada.Containers.Hashed_Maps (Key_Type        => SPAT.Source_File_Name,
                                 Element_Type    => Time_Per_Prover,
                                 Hash            => SPAT.Hash,
                                 Equivalent_Keys => SPAT."=",
                                 "="             => "=");

   procedure ZZZ_Find_Optimum (This : in T)
   is
      --  FIXME: This should probably go into the README.md instead of here.
      --
      --  For starters, trying to optimize proof times is relatively simple.
      --  For that you just need to collect all proofs which failed on one
      --  prover, but were successful with the other.  Of course, once you
      --  change the configuration, the picture may be different.
      --
      --  The problematic part is that at no point we have the full
      --  information (i.e. some provers might have been faster than others,
      --  but they were never tried, because slower ones still proved the VC).
      --
      --  Example:
      --
      --  RFLX.RFLX_Types.U64_Insert => 120.0 s/1.6 ks
      --  `-VC_PRECONDITION rflx-rflx_generic_types.adb:221:39 => 120.0 s/491.0 s
      --   `-CVC4: 120.0 s (Timeout)
      --    -Z3: 120.0 s (Timeout)
      --    -altergo: 188.2 ms (Valid)
      --   `-Z3: 120.0 s (Timeout)
      --    -CVC4: 5.4 s (Valid)
      --   `-Z3: 120.0 s (Timeout)
      --    -CVC4: 5.3 s (Valid)
      --   `-Z3: 60.0 ms (Valid)
      --   `-Z3: 40.0 ms (Valid)
      --   `-Z3: 20.0 ms (Valid)
      --   `-Trivial: 0.0 s (Valid)
      --
      --  Here we have 7 proof paths in total, let's ignore the ones that have
      --  only one prover (for these we can't say anything), leaving us with
      --
      --  RFLX.RFLX_Types.U64_Insert => 120.0 s/1.6 ks
      --  `-VC_PRECONDITION rflx-rflx_generic_types.adb:221:39 => 120.0 s/491.0 s
      --   `-CVC4: 120.0 s (Timeout)
      --    -Z3: 120.0 s (Timeout)
      --    -altergo: 188.2 ms (Valid)
      --   `-Z3: 120.0 s (Timeout)
      --    -CVC4: 5.4 s (Valid)
      --   `-Z3: 120.0 s (Timeout)
      --    -CVC4: 5.3 s (Valid)
      --
      --  We see, that altergo could proof the first path quite fast, so
      --  chances are it might be able to proof the remaining paths similarly
      --  fast. But without trying, there's no way of knowing it, so stick to
      --  the information we have.
      --
      --  Path  Prover       Max_Success Max_Failed Saving
      --  1     "altergo"    188.2 ms    --         119.8 s
      --        "Z3"           0.0 s     120.0 s    --
      --        "CVC4"         0.0 s     120.0 s    --
      --  2     "altergo"      --        --         --
      --        "Z3"           --        120.0 s    --
      --        "CVC4"         5.4 s     --         114.6
      --  3     "altergo"      --        --         --
      --        "Z3"           --        120.0 s    --
      --        "CVC4"         5.3 s     --         114.7
      --
      --  We take different orders into account (maybe we can even read them
      --  from the project file?).
      --
      --  "altergo", "CVC4",    "Z3"  : *maybe* -119.8 s
      --  "CVC4",    "altergo", "Z3"  : *maybe* -114.6 s
      --  "altergo", "Z3",      "CVC4": *maybe* -119.8 s

      --  We need to split the proofs per file, as this is the minimum
      --  granularity we can specify for the order of provers.
      --  TODO: Handle spec/body/separates correctly.

      Source_List : Per_File.Map;
      use type Per_File.Cursor;

      Times_Position : Per_File.Cursor;
      Dummy_Inserted : Boolean;
   begin
      --  Collect all proof items in the Per_File/Proof_Records structure.
      for E of This.Entities loop
         if
           --  Quick check. No failed attempts, no preferred prover.
           Proofs_Sentinel (Entity.Tree.Element (Position => E.Proofs)).Cache.Has_Failed_Attempts
         then
            for Proof in
              E.The_Tree.Iterate_Children (Parent => E.Proofs)
            loop
               declare
                  --  Extract our proof component from the tree.
                  The_Proof : constant Proof_Item.T'Class :=
                    Proof_Item.T'Class (Entity.Tree.Element (Position => Proof));
               begin
                  if
                    Source_List.Find
                      (Key => The_Proof.Source_File) = Per_File.No_Element
                  then
                     Source_List.Insert
                       (Key      => The_Proof.Source_File,
                        New_Item => Time_Per_Prover'(others => (others => 0.0)),
                        Position => Times_Position,
                        Inserted => Dummy_Inserted);
                     Log.Debug (Message => To_String (The_Proof.Source_File));
                  end if;

                  if The_Proof.Has_Failed_Attempts then
                     Log.Debug (Message => The_Proof.Image);

                     --  Iterate over all the verification conditions within
                     --  the proof.
                     for VC in
                       E.The_Tree.Iterate_Children
                         (Parent => Entity.Tree.First_Child (Position => Proof))
                     loop
                        declare
                           --  Extract our VC component from the tree.
                           The_Attempt : constant Proof_Attempt.T'Class :=
                             Proof_Attempt.T'Class
                               (Entity.Tree.Element (Position => VC));
                           P : constant Provers :=
                             To_Prover (Name => To_String (The_Attempt.Prover));
                           use type Proof_Attempt.Prover_Result;
                        begin
                           Log.Debug (Message => "  " & The_Attempt.Image);

                           declare
                              Ref : constant Per_File.Reference_Type :=
                                Source_List.Reference
                                  (Position => Times_Position);
                           begin
                              if The_Attempt.Result = Proof_Attempt.Valid then
                                 Ref.Element (P).Success :=
                                   Ref.Element (P).Success + The_Attempt.Time;
                              else
                                 Ref.Element (P).Failed :=
                                   Ref.Element (P).Failed + The_Attempt.Time;
                              end if;
                           end;
                        end;
                     end loop;
                  end if;
               end;
            end loop;
         end if;
      end loop;

      --  Debug output result.
      if Log.Debug_Enabled then
         for C in Source_List.Iterate loop
            Log.Debug (Message => To_String (Per_File.Key (Position => C)));

            declare
               E : constant Time_Per_Prover := Per_File.Element (Position => C);
            begin
               for P in E'Range loop
                  Log.Debug (Message => "  " & P'Image);
                  Log.Debug (Message => "    S  " & SPAT.Image (E (P).Success));
                  Log.Debug (Message => "    F  " & SPAT.Image (E (P).Failed));
               end loop;
            end;
         end loop;
      end if;
   end ZZZ_Find_Optimum;

end SPAT.Spark_Info;
