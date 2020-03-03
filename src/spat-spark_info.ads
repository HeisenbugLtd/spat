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
--  S.P.A.T. - Parse information from JSON data into internal data structure.
--
--  Collect file contents.
--
------------------------------------------------------------------------------
with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;

package SPAT.Spark_Info is

   type T is tagged limited private;
   --  Binary representation of the information obtained from a .spark JSON
   --  file.

   procedure Parse_JSON (This : in out T;
                         Root : in     GNATCOLL.JSON.JSON_Value);
   --  Parses JSON data from Root into data structure in This.

   --  Access functions.
   function Proof_Time (Info : in T) return Duration;
   function Flow_Time (Info : in T) return Duration;

private

   --  Information obtained from the timing section of a .spark file.
   type Timing_Info is
      record
         Proof : Duration; --  Total time the prover spent.
         Flow  : Duration; --  Total time of flow analysis.
      end record;

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
         Source_Entity : Source_Entity_Lists.Map;
         Timings       : Timing_Info;
         --  Timing information.
      end record;

end SPAT.Spark_Info;
