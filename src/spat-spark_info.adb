------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Directories;

with SPAT.Entity_Line;
with SPAT.Field_Names;
with SPAT.Flow_Item;
with SPAT.Log;
with SPAT.Preconditions;
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
   function Get_Sentinel (Node : in Analyzed_Entity) return Flows_Sentinel;

   ---------------------------------------------------------------------------
   --  Get_Sentinel (Proofs)
   ---------------------------------------------------------------------------
   function Get_Sentinel (Node : in Analyzed_Entity) return Proofs_Sentinel;

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
                           From_File : in     Subject_Name) with
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
   procedure Map_Proof_Elements (This : in out T;
                                 Root : in     JSON_Array);

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
                                 From_File : in     Subject_Name);

   ---------------------------------------------------------------------------
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This    : in out T;
                          File    : in     Subject_Name;
                          Root    : in     JSON_Value;
                          Version : in     File_Version);

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Name
   --
   --  Sort code entities by their name.
   --  Sorting: alphabetical, ascending
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Name (This      : in     T;
                                  Container : in out Strings.List);

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Proof_Time
   --
   --  Sort code entities by how much total proof time they required.
   --  Sorting: numerical, descending
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Proof_Time (This      : in     T;
                                        Container : in out Strings.List);

   ---------------------------------------------------------------------------
   --  Sort_File_By_Basename
   --
   --  Sort files by their base name (i.e. without containing directory or
   --  extension).
   --  Sorting: alphabetical, ascending
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Basename (This      : in     T;
                                    Container : in out Strings.List);

   ---------------------------------------------------------------------------
   --  Sort_File_By_Proof_Time
   --
   --  Sort files by how much total time it required to spend in flow analysis
   --  and proof.
   --  Sorting: numerical, descending
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Proof_Time (This      : in     T;
                                      Container : in out Strings.List);

   ---------------------------------------------------------------------------
   --  Subprogram implementations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Flow_Time
   ---------------------------------------------------------------------------
   function Flow_Time (This : in T;
                       File : in Subject_Name) return Duration is
     (This.Files (File).Flow);

   ---------------------------------------------------------------------------
   --  Get_Sentinel (Flows)
   ---------------------------------------------------------------------------
   function Get_Sentinel (Node : in Analyzed_Entity) return Flows_Sentinel is
     (Flows_Sentinel (SPAT.Entity.Tree.Element (Position => Node.Flows)));

   ---------------------------------------------------------------------------
   --  Get_Sentinel (Proofs)
   ---------------------------------------------------------------------------
   function Get_Sentinel (Node : in Analyzed_Entity) return Proofs_Sentinel is
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
   function Has_Failed_Attempts (This   : in T;
                                 Entity : in Subject_Name) return Boolean is
      Reference : Analyzed_Entity renames This.Entities (Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      if Sentinel.Cache.Is_Valid then
         return Sentinel.Cache.Has_Failed_Attempts;
      end if;

      return (for some C in
                Reference.The_Tree.Iterate_Children (Parent => Reference.Proofs) =>
                Proof_Item.T'Class (SPAT.Entity.Tree.Element (Position => C)).
                  Has_Failed_Attempts);
   end Has_Failed_Attempts;

   ---------------------------------------------------------------------------
   --  Has_Unproved_Attempts
   ---------------------------------------------------------------------------
   function Has_Unproved_Attempts (This   : in T;
                                   Entity : in Subject_Name) return Boolean is
      Reference : Analyzed_Entity renames This.Entities (Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
   begin
      if Sentinel.Cache.Is_Valid then
         return Sentinel.Cache.Has_Unproved_Attempts;
      end if;

      return (for some C in
                Reference.The_Tree.Iterate_Children (Parent => Reference.Proofs) =>
                Proof_Item.T'Class (SPAT.Entity.Tree.Element (Position => C)).
                  Has_Unproved_Attempts);
   end Has_Unproved_Attempts;

   ---------------------------------------------------------------------------
   --  List_All_Entities
   ---------------------------------------------------------------------------
   function List_All_Entities
     (This    : in T;
      Sort_By : in Sorting_Criterion := Name) return Strings.List'Class is
   begin
      return Result : Strings.List do
         for Position in This.Entities.Iterate loop
            Result.Append
              (New_Item => Analyzed_Entities.Key (Position => Position));
         end loop;

         case Sort_By is
            when None =>
               null;

            when Name =>
               This.Sort_Entity_By_Name (Container => Result);

            when Time =>
               This.Sort_Entity_By_Proof_Time (Container => Result);
         end case;
      end return;
   end List_All_Entities;

   ---------------------------------------------------------------------------
   --  List_All_Files
   ---------------------------------------------------------------------------
   function List_All_Files
     (This    : in T;
      Sort_By : in Sorting_Criterion := None) return Strings.List'Class is
   begin
      return Result : Strings.List do
         for Position in This.Files.Iterate loop
            Result.Append (New_Item => File_Timings.Key (Position => Position));
         end loop;

         case Sort_By is
            when None =>
               null;

            when Name =>
               This.Sort_File_By_Basename (Container => Result);

            when Time =>
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
                           From_File : in     Subject_Name)
   is
      Obj_Name : constant Subject_Name := Root.Get (Field => Field_Names.Name);
      Slocs    : constant JSON_Array   := Root.Get (Field => Field_Names.Sloc);
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
         Reference_Entity : Analyzed_Entity renames This.Entities (Index);
         use type Entity.Tree.Cursor;
      begin
         --  Add sentinel(s) if not yet present.
         if Reference_Entity.Source_Lines = Entity.Tree.No_Element then
            Reference_Entity.The_Tree.Insert_Child
              (Parent   => Reference_Entity.The_Tree.Root,
               Before   => Entity.Tree.No_Element,
               New_Item => Source_Lines_Sentinel'(Entity.T with null record),
               Position => Reference_Entity.Source_Lines);
         end if;

         if Reference_Entity.Flows = Entity.Tree.No_Element then
            Reference_Entity.The_Tree.Insert_Child
              (Parent   => Reference_Entity.The_Tree.Root,
               Before   => Entity.Tree.No_Element,
               New_Item => Empty_Flows_Sentinel,
               Position => Reference_Entity.Flows);
         end if;

         if Reference_Entity.Proofs = Entity.Tree.No_Element then
            Reference_Entity.The_Tree.Insert_Child
              (Parent   => Reference_Entity.The_Tree.Root,
               Before   => Entity.Tree.No_Element,
               New_Item => Empty_Proofs_Sentinel,
               Position => Reference_Entity.Proofs);
         end if;

         This.Map_Sloc_Elements (Root     => Slocs,
                                 The_Tree => Reference_Entity.The_Tree,
                                 Position => Reference_Entity.Source_Lines);
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
                        The_Key : constant Subject_Name :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Flow_Item.Has_Required_Fields (Object => Element)
                           then
                              declare
                                 Reference : Analyzed_Entity renames
                                   This.Entities (Update_At);
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
   procedure Map_Proof_Elements (This : in out T;
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
                        The_Key : constant Subject_Name :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Proof_Item.Has_Required_Fields (Object => Element)
                           then
                              declare
                                 Reference : Analyzed_Entity renames
                                   This.Entities (Update_At);
                              begin
                                 Reference.The_Tree.Append_Child
                                   (Parent   => Reference.Proofs,
                                    New_Item =>
                                      Proof_Item.Create (Object => Element));
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
         SPAT.Proof_Item.Sort_By_Duration (This   => E.The_Tree,
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
                                 From_File : in     Subject_Name)
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
   procedure Map_Spark_File (This : in out T;
                             File : in     Subject_Name;
                             Root : in     JSON_Value)
   is
      Version : constant File_Version := Guess_Version (Root => Root);
   begin
      This.Flow_Count := -1;

      --  If I understand the .spark file format correctly, this should
      --  establish the table of all known analysis elements.
      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Spark,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Spark_Elements
           (Root      => Root.Get (Field => Field_Names.Spark),
            From_File => File);
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
           (Root => Root.Get (Field => Field_Names.Proof));
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
   end Map_Spark_File;

   ---------------------------------------------------------------------------
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This    : in out T;
                          File    : in     Subject_Name;
                          Root    : in     JSON_Value;
                          Version : in     File_Version) is
   begin
      if
        Timing_Item.Has_Required_Fields (Object  => Root,
                                         Version => Version)
      then
         This.Files.Insert
           (Key      => File,
            New_Item => Timing_Item.Create (Object  => Root,
                                            Version => Version));
      else
         This.Files.Insert (Key      => File,
                            New_Item => Timing_Item.None);
      end if;
   end Map_Timings;

   ---------------------------------------------------------------------------
   --  Max_Proof_Time
   ---------------------------------------------------------------------------
   function Max_Proof_Time (This   : in T;
                            Entity : in Subject_Name) return Duration
   is
      Reference : Analyzed_Entity renames This.Entities (Entity);
      Sentinel  : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
      Max_Time  : Duration := 0.0;
   begin
      if Sentinel.Cache.Is_Valid then
         return Sentinel.Cache.Max_Proof_Time;
      end if;

      for C in
        Reference.The_Tree.Iterate_Children (Parent => Reference.Proofs)
      loop
         Max_Time :=
           Duration'Max
             (Max_Time,
              Proof_Item.T'Class (SPAT.Entity.Tree.Element (Position => C)).
                Max_Time);
      end loop;

      return Max_Time;
   end Max_Proof_Time;

   ---------------------------------------------------------------------------
   --  Num_Flows
   ---------------------------------------------------------------------------
   function Num_Flows (This : in T) return Ada.Containers.Count_Type is
      Result : Ada.Containers.Count_Type := 0;
   begin
      if This.Flow_Count /= -1 then
         return This.Flow_Count;
      end if;

      for E of This.Entities loop
         Result := Result + Entity.Tree.Child_Count (Parent => E.Flows);
      end loop;

      return Result;
   end Num_Flows;

   ---------------------------------------------------------------------------
   --  Num_Proofs
   ---------------------------------------------------------------------------
   function Num_Proofs (This : in T) return Ada.Containers.Count_Type is
      Result : Ada.Containers.Count_Type := 0;
   begin
      for E of This.Entities loop
         Result := Result + Entity.Tree.Child_Count (Parent => E.Proofs);
      end loop;

      return Result;
   end Num_Proofs;

   ---------------------------------------------------------------------------
   --  Proof_List
   ---------------------------------------------------------------------------
   function Proof_List (This   : in T;
                        Entity : in Subject_Name) return Proof_Item.List.T is
      Result : Proof_Item.List.T;
      Reference : Analyzed_Entity renames This.Entities (Entity);
   begin
      --  FIXME: It is stupid to return a whole Vector each time.
      for C in
        Reference.The_Tree.Iterate_Children (Parent => Reference.Proofs)
      loop
         Result.Append
           (New_Item =>
              Proof_Item.T (SPAT.Entity.Tree.Element (Position => C)));
      end loop;

      return Result;
   end Proof_List;

   ---------------------------------------------------------------------------
   --  Proof_Time
   ---------------------------------------------------------------------------
   function Proof_Time (This : in T;
                        File : in Subject_Name) return Duration is
      Timings : constant Timing_Item.T := This.Files (File);
   begin
      case Timings.Version is
         when GNAT_CE_2019 =>
            --  In this version we have a "proof" timing field, report it
            --  directly.
            return Timings.Proof;

         when GNAT_CE_2020 =>
            declare
               Summed_Proof_Time : Duration := Timings.Proof;
            begin
               --  In this version there's no proof timing field anymore, so we
               --  need to sum the proof times of the entities, too.
               for Position in This.Entities.Iterate loop
                  declare
                     Name : constant Subject_Name :=
                       Analyzed_Entities.Key (Position);
                  begin
                     if
                       Analyzed_Entities.Element (Position).SPARK_File = File
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
   --  Sort_Entity_By_Name
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Name (This      : in     T;
                                  Container : in out Strings.List)
   is
      pragma Unreferenced (This);
      package Sorting is new
        Strings.Implementation.Vectors.Generic_Sorting ("<" => "<");
   begin
      Sorting.Sort
        (Container => Strings.Implementation.Vectors.Vector (Container));
   end Sort_Entity_By_Name;

   ---------------------------------------------------------------------------
   --  Sort_Entity_By_Proof_Time
   ---------------------------------------------------------------------------
   procedure Sort_Entity_By_Proof_Time (This      : in     T;
                                        Container : in out Strings.List)
   is
      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in Subject_Name;
                    Right : in Subject_Name) return Boolean is
        (This.Total_Proof_Time (Entity => Left) >
         This.Total_Proof_Time (Entity => Right));

      package Sorting is new
        Strings.Implementation.Vectors.Generic_Sorting ("<" => "<");
   begin
      Sorting.Sort
        (Container => Strings.Implementation.Vectors.Vector (Container));
   end Sort_Entity_By_Proof_Time;

   ---------------------------------------------------------------------------
   --  Sort_File_By_Basename
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Basename (This      : in     T;
                                    Container : in out Strings.List)
   is
      pragma Unreferenced (This);

      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in Subject_Name;
                    Right : in Subject_Name) return Boolean is
        (Ada.Directories.Base_Name (Name => To_String (Source => Left)) <
           Ada.Directories.Base_Name (Name => To_String (Source => Right)));

      package Sorting is new
        Strings.Implementation.Vectors.Generic_Sorting ("<" => "<");
   begin
      Sorting.Sort
        (Container => Strings.Implementation.Vectors.Vector (Container));
   end Sort_File_By_Basename;

   ---------------------------------------------------------------------------
   --  Sort_File_By_Proof_Time
   ---------------------------------------------------------------------------
   procedure Sort_File_By_Proof_Time (This      : in     T;
                                      Container : in out Strings.List)
   is
      ------------------------------------------------------------------------
      --  "<"
      ------------------------------------------------------------------------
      function "<" (Left  : in Subject_Name;
                    Right : in Subject_Name) return Boolean is
        ((This.Proof_Time (File => Left) + This.Flow_Time (File => Left)) >
         (This.Proof_Time (File => Right) + This.Flow_Time (File => Right)));

      package Sorting is new
        Strings.Implementation.Vectors.Generic_Sorting ("<" => "<");
   begin
      Sorting.Sort
        (Container => Strings.Implementation.Vectors.Vector (Container));
   end Sort_File_By_Proof_Time;

   ---------------------------------------------------------------------------
   --  Total_Proof_Time
   ---------------------------------------------------------------------------
   function Total_Proof_Time (This   : in T;
                              Entity : in Subject_Name) return Duration
   is
      Reference  : Analyzed_Entity renames This.Entities (Entity);
      Sentinel   : constant Proofs_Sentinel := Get_Sentinel (Node => Reference);
      Total_Time : Duration := 0.0;
   begin
      if Sentinel.Cache.Is_Valid then
         return Sentinel.Cache.Total_Proof_Time;
      end if;

      for C in
        Reference.The_Tree.Iterate_Children (Parent => Reference.Proofs)
      loop
         Total_Time :=
           Total_Time +
             Proof_Item.T'Class (SPAT.Entity.Tree.Element (Position => C)).
               Total_Time;
      end loop;

      return Total_Time;
   end Total_Proof_Time;

   ---------------------------------------------------------------------------
   --  Update
   ---------------------------------------------------------------------------
   not overriding
   procedure Update (This : in out T) is
   begin
      for Position in This.Entities.Iterate loop
         declare
            Reference : Analyzed_Entity renames This.Entities (Position);
         begin
            declare
               Key : constant Subject_Name :=
                 Analyzed_Entities.Key (Position => Position);

               --  Call the potentially "slow" recursive calculation functions.
               Max_Proof_Time   : constant Duration :=
                 This.Max_Proof_Time (Entity => Key);
               Total_Proof_Time : constant Duration :=
                 This.Total_Proof_Time (Entity => Key);
               Has_Failed_Attempts : constant Boolean :=
                 This.Has_Failed_Attempts (Entity => Key);
               Has_Unproved_Attempts : constant Boolean :=
                 This.Has_Unproved_Attempts (Entity => Key);
            begin
               Entity.Tree.Replace_Element
                 (Container => Reference.The_Tree,
                  Position  => Reference.Proofs,
                  New_Item  =>
                    Proofs_Sentinel'
                      (Entity.T with
                       Cache =>
                         Proof_Cache'
                           (Is_Valid              => True,
                            Max_Proof_Time        => Max_Proof_Time,
                            Total_Proof_Time      => Total_Proof_Time,
                            Has_Failed_Attempts   => Has_Failed_Attempts,
                            Has_Unproved_Attempts => Has_Unproved_Attempts)));
            end;

            This.Flow_Count :=
              This.Flow_Count + Entity.Tree.Child_Count (Parent => Reference.Flows);
         end;
      end loop;
   end Update;

end SPAT.Spark_Info;
