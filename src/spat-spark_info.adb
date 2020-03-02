------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Spark_Info is

   function Proof_Time (Info : in T) return Duration is
     (Info.Source_Entity.Timings.Proof);

   function Flow_Time (Info : in T) return Duration is
     (Info.Source_Entity.Timings.Flow);

   function Parse_JSON (Root : in GNATCOLL.JSON.JSON_Value) return T
   is
      Timings : GNATCOLL.JSON.JSON_Value;
   begin
      Timings := Root.Get (Field => "timings");

      return Info : SPARK_Info.T do
         Info.Source_Entity.Timings.Proof :=
           Duration (Float'(Timings.Get (Field => "proof")));
         Info.Source_Entity.Timings.Flow :=
           Duration (Float'(Timings.Get (Field => "flow analysis")));
      end return;
   end Parse_JSON;

end SPAT.Spark_Info;
