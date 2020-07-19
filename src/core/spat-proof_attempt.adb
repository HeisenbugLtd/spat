------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Proof_Attempt is

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   not overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean is
      use type Proof_Attempt_Ids.Id;
   begin
      --  Sort by time, steps, result, and then prover name.
      if Left.Time /= Right.Time then
         return Left.Time > Right.Time;
      end if;

      if Left.Steps /= Right.Steps then
         return Left.Steps > Right.Steps;
      end if;

      if Left.Result /= Right.Result then
         return Left.Result < Right.Result;
      end if;

      if Left.Prover /= Right.Prover then
         return Left.Prover < Right.Prover;
      end if;

      --  Last resort, the unique id.
      return Left.Id < Right.Id;
   end "<";

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : JSON_Value;
                    Prover : Prover_Name) return T
   is
      Time_Field : constant JSON_Value :=
                     Object.Get (Field => Field_Names.Time);

   begin
      return T'(Entity.T with
                  Prover => Prover,
                  Result =>
                    Result_Name
                      (Subject_Name'(Object.Get (Field => Field_Names.Result))),
                  Workload =>
                    Time_And_Steps'
                      (Time   =>
                         (case Time_Field.Kind is
                             when JSON_Float_Type =>
                               Duration (Time_Field.Get_Long_Float),
                             when JSON_Int_Type   =>
                               Duration (Long_Long_Integer'(Time_Field.Get)),
                             when others          =>
                                raise Program_Error
                                  with
                                    "Fatal: Impossible Kind """ &
                                    Time_Field.Kind'Image & """ of JSON object!"),
                       Steps  =>
                         --  FIXME: Step scaling will not be necessary anymore for
                         --         the SPARK development version (i.e. SPARK CE 2021).
                         Scaled (Prover    => Prover,
                                 Raw_Steps =>
                                   Prover_Steps
                                     (Long_Integer'
                                        (Object.Get (Field => Field_Names.Steps))))),
                  Id     => Proof_Attempt_Ids.Next);
   end Create;

end SPAT.Proof_Attempt;
