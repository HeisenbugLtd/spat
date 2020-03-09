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
with Ada.Containers.Generic_Array_Sort;

private with Ada.Containers.Hashed_Maps;
private with SPAT.Entity_Lines;
private with SPAT.Flow_Items;
private with SPAT.Proof_Items;
private with SPAT.Timing_Items;

package SPAT.Spark_Info is

   --  Helper types.
   type String_Array is array (Positive range <>) of Entity_Name;

   procedure Sort_By_Name is new
     Ada.Containers.Generic_Array_Sort (Index_Type   => Positive,
                                        Element_Type => Entity_Name,
                                        Array_Type   => String_Array);

   type T is tagged limited private;
   --  Binary representation of the information obtained from a .spark JSON
   --  file.

   procedure Map_Spark_File (This : in out T;
                             File : in     File_Name;
                             Root : in     JSON_Value);
   --  Traverses through the JSON data given in Root and translates it into the
   --  data structure given in This.

   function List_All_Entities (This : in T) return String_Array;
   --  Returns a sorted list of all entities (source unit names) currently
   --  stored in This.

   function List_All_Files (This : in T) return String_Array;
   --  Returns a sorted list of the names of all files that have been parsed
   --  into T.

   --  Access functions.
   function Num_Flows (This : in T) return Ada.Containers.Count_Type;
   function Flow_Time (This : in T;
                       File : in File_Name) return Duration;
   function Num_Proofs (This : in T) return Ada.Containers.Count_Type;
   function Proof_Time (This : in T;
                        File : in File_Name) return Duration;

   --  Lookup functions.
   function Max_Proof_Time (This    : in T;
                            Element : in Entity_Name) return Duration;
   function Total_Proof_Time (This    : in T;
                              Element : in Entity_Name) return Duration;

private

   type Analyzed_Entity is
      record
         Source_Lines : Entity_Lines.Vector;
         Flows        : Flow_Items.Vector;
         Proofs       : Proof_Items.Vector;
      end record;

   --  Type representing a source (file) entity.
   package Analyzed_Entities is new
     Ada.Containers.Hashed_Maps (Key_Type        => Entity_Name,
                                 Element_Type    => Analyzed_Entity,
                                 Hash            => Hash,
                                 Equivalent_Keys => "=");

   package File_Timings is new
     Ada.Containers.Hashed_Maps (Key_Type        => File_Name,
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
