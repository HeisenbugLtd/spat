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
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;

package SPAT.Spark_Info is

   type T is tagged private;
   --  Binary representation of the information obtained from a .spark JSON
   --  file.

   function Parse_JSON (Root : in GNATCOLL.JSON.JSON_Value) return T;

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

   --  Type representing a source (file) entity.
   type Source_Entities is
      record
         Name    : Ada.Strings.Unbounded.Unbounded_String;
         --  Name of the Ada entity.
         Timings : Timing_Info;
         --  Timing information.
      end record;

   type T is tagged
      record
         Source_Entity : Source_Entities;
      end record;

end SPAT.Spark_Info;
