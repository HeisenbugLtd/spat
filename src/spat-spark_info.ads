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
with Ada.Containers.Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded.Hash;
with GNATCOLL.JSON;

package SPAT.Spark_Info is

   --  Helper types.
   type String_Array is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Sort_By_Name is new
     Ada.Containers.Generic_Array_Sort
       (Index_Type   => Positive,
        Element_Type => Ada.Strings.Unbounded.Unbounded_String,
        Array_Type   => String_Array,
        "<"          => Ada.Strings.Unbounded."<");

   type T is tagged limited private;
   --  Binary representation of the information obtained from a .spark JSON
   --  file.

   procedure Map_SPARK_File (This :    out T;
                             Root : in     GNATCOLL.JSON.JSON_Value);
   --  Traverses through the JSON data given in Root and translates it into the
   --  data structure given in This.

   function List_All_Entities (This : in T) return String_Array;
   --  Returns a sorted list of all entities (source unit names) currently
   --  stored in This.

   --  Access functions.
   function Num_Flows (This : in T) return Natural;
   function Flow_Time (This : in T) return Duration;

   function Num_Proofs (This : in T) return Natural;
   function Proof_Time (This : in T) return Duration;


private

   --  Information obtained from the timing section of a .spark file.
   type Timing_Item is
      record
         Proof : Duration; --  Total time the prover spent.
         Flow  : Duration; --  Total time of flow analysis.
      end record;

   Null_Timing_Item : constant Timing_Item := Timing_Item'(Proof => 0.0,
                                                           Flow  => 0.0);

   type File_Line_Item is
      record
         File_Name   : Ada.Strings.Unbounded.Unbounded_String;
         Line_Number : Natural;
      end record;

   package File_Line_Items is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => File_Line_Item);

   type Entity_Location is
      record
         File   : Ada.Strings.Unbounded.Unbounded_String;
         Line   : Natural;
         Column : Natural;
      end record;

   type Flow_Item is
      record
         Where    : Entity_Location;
         Rule     : Ada.Strings.Unbounded.Unbounded_String;
         Severity : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Flow_Items is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Flow_Item);

   type Proof_Item is
      record
         Where    : Entity_Location;
         Rule     : Ada.Strings.Unbounded.Unbounded_String;
         Severity : Ada.Strings.Unbounded.Unbounded_String;
      end record;

   package Proof_Items is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Proof_Item);

   type Analyzed_Entity is
      record
         Locations : File_Line_Items.Vector;
         Flows     : Flow_Items.Vector;
         Proofs    : Proof_Items.Vector;
      end record;

   --  Type representing a source (file) entity.
   package Analyzed_Entities is new
     Ada.Containers.Hashed_Maps (Key_Type        => Ada.Strings.Unbounded.Unbounded_String,
                                 Element_Type    => Analyzed_Entity,
                                 Hash            => Ada.Strings.Unbounded.Hash,
                                 Equivalent_Keys => Ada.Strings.Unbounded."=",
                                 "="             => "=");

   type T is tagged limited
      record
         Entities : Analyzed_Entities.Map := Analyzed_Entities.Empty_Map;
         Timings  : Timing_Item           := Null_Timing_Item;
      end record;

end SPAT.Spark_Info;
