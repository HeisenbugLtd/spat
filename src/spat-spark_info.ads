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
with SPAT.Proof_Items;
private with Ada.Containers.Hashed_Maps;
private with SPAT.Entity_Lines;
private with SPAT.Flow_Items;
private with SPAT.Timing_Items;

package SPAT.Spark_Info is

   --  Helper types.
   type String_Array is array (Positive range <>) of Subject_Name;

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
   procedure Map_Spark_File (This : in out T;
                             File : in     Subject_Name;
                             Root : in     JSON_Value);

   ---------------------------------------------------------------------------
   --  List_All_Entities
   --
   --  Returns an (optionally sorted) list of all entities (source unit names)
   --  currently stored in This.
   ---------------------------------------------------------------------------
   function List_All_Entities
     (This    : in T;
      Sort_By : in Sorting_Criterion := Name) return String_Array;

   ---------------------------------------------------------------------------
   --  List_All_Files
   --
   --  Returns an (optionally sorted) list of the names of all files that have
   --  been parsed into This.
   ---------------------------------------------------------------------------
   function List_All_Files
     (This    : in T;
      Sort_By : in Sorting_Criterion := None) return String_Array;

   ---------------------------------------------------------------------------
   --  Num_Flows
   --
   --  Return the total number of flows (flow items) collected.
   ---------------------------------------------------------------------------
   function Num_Flows (This : in T) return Ada.Containers.Count_Type;

   ---------------------------------------------------------------------------
   --  Flow_Time
   --
   --  Reported time taken for the flow analysis for File.
   ---------------------------------------------------------------------------
   function Flow_Time (This : in T;
                       File : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Num_Proofs
   --
   --  Return the total number of proofs collected.
   ---------------------------------------------------------------------------
   function Num_Proofs (This : in T) return Ada.Containers.Count_Type;

   ---------------------------------------------------------------------------
   --  Proof_Time
   --
   --  Reported time taken for all the proofs for File.
   ---------------------------------------------------------------------------
   function Proof_Time (This : in T;
                        File : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Max_Proof_Time
   --
   --  Maximum time taken for a single proof for Entity.
   ---------------------------------------------------------------------------
   function Max_Proof_Time (This   : in T;
                            Entity : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Total_Proof_Time
   --
   --  Total (accumulated) time taken for all proofs for Entity.
   ---------------------------------------------------------------------------
   function Total_Proof_Time (This   : in T;
                              Entity : in Subject_Name) return Duration;

   ---------------------------------------------------------------------------
   --  Proof_List
   --
   --  List of proof items for given Entity, sorted by time.
   ---------------------------------------------------------------------------
   function Proof_List (This   : in T;
                        Entity : in Subject_Name) return Proof_Items.Vector;

private

   type Analyzed_Entity is
      record
         Source_Lines : Entity_Lines.Vector;
         Flows        : Flow_Items.Vector;
         Proofs       : Proof_Items.Vector;
      end record;

   --  Type representing a source (file) entity.
   package Analyzed_Entities is new
     Ada.Containers.Hashed_Maps (Key_Type        => Subject_Name,
                                 Element_Type    => Analyzed_Entity,
                                 Hash            => Hash,
                                 Equivalent_Keys => "=");

   package File_Timings is new
     Ada.Containers.Hashed_Maps (Key_Type        => Subject_Name,
                                 Element_Type    => Timing_Items.T,
                                 Hash            => Hash,
                                 Equivalent_Keys => "=",
                                 "="             => Timing_Items."=");

   type T is tagged limited
      record
         Entities : Analyzed_Entities.Map := Analyzed_Entities.Empty_Map;
         Files    : File_Timings.Map      := File_Timings.Empty_Map;
      end record;

end SPAT.Spark_Info;
