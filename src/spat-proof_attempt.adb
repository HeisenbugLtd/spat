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
                    Prover : Subject_Name) return T
   is
      Time_Field : constant JSON_Value :=
                     Object.Get (Field => Field_Names.Time);

      ------------------------------------------------------------------------
      --  Scaled
      --
      --  Implement prover specific steps scaling.
      ------------------------------------------------------------------------
      function Scaled (Prover    : in Subject_Name;
                       Raw_Steps : in Prover_Steps) return Prover_Steps;

      ------------------------------------------------------------------------
      --  Scaled
      --
      --  See https://github.com/AdaCore/why3/blob/master/src/gnat/gnat_config.ml#L538
      ------------------------------------------------------------------------
      function Scaled (Prover    : in Subject_Name;
                       Raw_Steps : in Prover_Steps) return Prover_Steps is
      begin
         if Ada.Strings.Unbounded.Index (Source  => Prover,
                                         Pattern => "CVC4") = 1
         then
            --  add = 15_000, mult = 35
            return Prover_Steps'Max (Raw_Steps - 15_000, 0) / 35 + 1;
         elsif Ada.Strings.Unbounded.Index (Source => Prover,
                                            Pattern => "Z3") = 1
         then
            --  add = 450_000, mult = 800
            return Prover_Steps'Max (Raw_Steps - 450_000, 0) / 800 + 1;
         else
            --  alt-ergo, and others => no scaling
            return Raw_Steps + 1;
         end if;
      end Scaled;
   begin
      return T'(Entity.T with
                  Prover => Prover,
                  Result => Object.Get (Field => Field_Names.Result),
                  Time   =>
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
                Steps =>
                  Scaled (Prover    => Prover,
                          Raw_Steps =>
                            Prover_Steps
                              (Long_Integer'
                                 (Object.Get (Field => Field_Names.Steps)))),
                  Id    => Proof_Attempt_Ids.Next);
   end Create;

end SPAT.Proof_Attempt;
