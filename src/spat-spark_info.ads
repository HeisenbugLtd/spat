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
--  S.P.A.T. - Map information from JSON data into internal data structure.
--
--  Collect file contents.
--
------------------------------------------------------------------------------

limited private with Ada.Containers.Hashed_Maps;
private with SPAT.Entity.Tree;
with SPAT.Flow_Item.List;
with SPAT.Proof_Item.List;
limited with SPAT.Strings;
with SPAT.Timing_Item;

package SPAT.Spark_Info is

   type Sorting_Criterion is (None, Name, Time);

   type T is tagged limited private;
   --  Binary representation of the information obtained from a .spark JSON
   --  file.

   ---------------------------------------------------------------------------
   --  Map_Spark_File
   --
   --  Traverses through the JSON data given in Root and translates it into
   --  the data structure given in This.
   ---------------------------------------------------------------------------
   not overriding
   procedure Map_Spark_File (This : in out T;
                             File : in     Subject_Name;
                             Root : in     JSON_Value);

   ---------------------------------------------------------------------------
   --  List_All_Entities
   --
   --  Returns an (optionally sorted) list of all entities (source unit names)
   --  currently stored in This.
   ---------------------------------------------------------------------------
   not overriding
   function List_All_Entities
     (This    : in T;
      Sort_By : in Sorting_Criterion := Name) return Strings.List'Class;

   ---------------------------------------------------------------------------
   --  List_All_Files
   --
   --  Returns an (optionally sorted) list of the names of all files that have
   --  been parsed into This.
   ---------------------------------------------------------------------------
   not overriding
   function List_All_Files
     (This    : in T;
      Sort_By : in Sorting_Criterion := None) return Strings.List'Class;

   ---------------------------------------------------------------------------
   --  Num_Flows
   --
   --  Return the total number of flows (flow items) collected.
   ---------------------------------------------------------------------------
   not overriding
   function Num_Flows (This : in T) return Ada.Containers.Count_Type;

   ---------------------------------------------------------------------------
   --  Flow_Time
   --
   --  Reported time taken for the flow analysis for File.
   ---------------------------------------------------------------------------
   not overriding
   function Flow_Time (This : in T;
                       File : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Num_Proofs
   --
   --  Return the total number of proofs collected.
   ---------------------------------------------------------------------------
   not overriding
   function Num_Proofs (This : in T) return Ada.Containers.Count_Type;

   ---------------------------------------------------------------------------
   --  Proof_Time
   --
   --  Reported time taken for all the proofs for File.
   ---------------------------------------------------------------------------
   not overriding
   function Proof_Time (This : in T;
                        File : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Max_Proof_Time
   --
   --  Maximum time taken for a single proof for Entity.
   ---------------------------------------------------------------------------
   not overriding
   function Max_Proof_Time (This   : in T;
                            Entity : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Total_Proof_Time
   --
   --  Total (accumulated) time taken for all proofs for Entity.
   ---------------------------------------------------------------------------
   not overriding
   function Total_Proof_Time (This   : in T;
                              Entity : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Proof_List
   --
   --  List of proof items for given Entity, sorted by time.
   ---------------------------------------------------------------------------
   not overriding
   function Proof_List (This   : in T;
                        Entity : in Subject_Name) return Proof_Item.List.T;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   --
   --  Returns True if any of the proof attempts for Entity do not have a
   --  "Valid" result.
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This   : in T;
                                 Entity : in Subject_Name) return Boolean;

   ---------------------------------------------------------------------------
   --  Has_Unproved_Attempts
   --
   --  Returns True if some of the proof attempts for Entity do have a "Valid"
   --  result.
   ---------------------------------------------------------------------------
   not overriding
   function Has_Unproved_Attempts (This   : in T;
                                   Entity : in Subject_Name) return Boolean;

private

   type Analyzed_Entity is
      record
         SPARK_File   : Subject_Name; --  Which file this entity was found in.
         Source_Lines : Entity.Tree.T; --  Currently unused.
         Flows        : Flow_Item.List.T;
         Proofs       : Proof_Item.List.T;
      end record;

   --  Type representing a source (file) entity.
   package Analyzed_Entities is new
     Ada.Containers.Hashed_Maps (Key_Type        => Subject_Name,
                                 Element_Type    => Analyzed_Entity,
                                 Hash            => Hash,
                                 Equivalent_Keys => "=");

   package File_Timings is new
     Ada.Containers.Hashed_Maps (Key_Type        => Subject_Name,
                                 Element_Type    => Timing_Item.T,
                                 Hash            => Hash,
                                 Equivalent_Keys => "=",
                                 "="             => Timing_Item."=");

   type T is tagged limited
      record
         Entities : Analyzed_Entities.Map;
         Files    : File_Timings.Map;
      end record;

end SPAT.Spark_Info;
