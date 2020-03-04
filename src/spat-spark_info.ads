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
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
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
   function Proof_Time (This : in T) return Duration;
   function Flow_Time (This : in T) return Duration;

private

   --  Information obtained from the timing section of a .spark file.
   type Timing_Info is
      record
         Proof : Duration; --  Total time the prover spent.
         Flow  : Duration; --  Total time of flow analysis.
      end record;

   Null_Timing_Info : constant Timing_Info := Timing_Info'(Proof => 0.0,
                                                           Flow  => 0.0);

   type Line_Location is
      record
         File_Name   : Ada.Strings.Unbounded.Unbounded_String;
         Line_Number : Natural;
      end record;

   package Source_Line_Locations is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Line_Location);

   type Source_Entity is
      record
         Locations : Source_Line_Locations.Vector;
      end record;

   --  Type representing a source (file) entity.
   package Source_Entity_Lists is new
     Ada.Containers.Indefinite_Hashed_Maps (Key_Type        => String,
                                            Element_Type    => Source_Entity,
                                            Hash            => Ada.Strings.Hash,
                                            Equivalent_Keys => Standard."=",
                                            "="             => "=");

   type T is tagged limited
      record
         Source_Entity : Source_Entity_Lists.Map := Source_Entity_Lists.Empty_Map;
         Timings       : Timing_Info             := Null_Timing_Info;
      end record;

end SPAT.Spark_Info;
