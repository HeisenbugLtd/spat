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
     (Info.Timings.Proof);

   function Flow_Time (Info : in T) return Duration is
     (Info.Timings.Flow);

   function Parse_JSON (Root : in GNATCOLL.JSON.JSON_Value) return T
   is
      SPARK   : GNATCOLL.JSON.JSON_Array;
      Timings : GNATCOLL.JSON.JSON_Value;
   begin
      SPARK   := Root.Get (Field => "spark");
      Timings := Root.Get (Field => "timings");

      return Info : SPARK_Info.T do
         for S in 1 .. GNATCOLL.JSON.Length (SPARK) loop
            declare
               Obj : constant GNATCOLL.JSON.JSON_Value :=
                       GNATCOLL.JSON.Get (SPARK, S);
               Slocs : constant GNATCOLL.JSON.JSON_Array :=
                        Obj.Get (Field => "sloc");
            begin
               for I in 1 .. GNATCOLL.JSON.Length (Slocs) loop
                  declare
                     Sloc : GNATCOLL.JSON.JSON_Value :=
                              GNATCOLL.JSON.Get (Slocs, I);
                  begin
                     Info.Source_Entity.Append
                       (New_Item => Source_Entity'(Name => Obj.Get (Field => "name"),
                                                   File => Sloc.Get (Field => "file"),
                                                   Line => Sloc.Get (Field => "line")));
                  end;
               end loop;
            end;
         end loop;
         Info.Timings.Proof := Duration (Float'(Timings.Get (Field => "proof")));
         Info.Timings.Flow := Duration (Float'(Timings.Get (Field => "flow analysis")));
      end return;
   end Parse_JSON;

end SPAT.Spark_Info;
