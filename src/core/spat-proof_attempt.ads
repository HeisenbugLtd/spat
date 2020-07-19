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
--  S.P.A.T. - Object representing a JSON "proof attempt" object.
--
------------------------------------------------------------------------------

with SPAT.Entity;
with SPAT.Field_Names;
with SPAT.Preconditions;
private with SPAT.Unique_Ids;

package SPAT.Proof_Attempt is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object : JSON_Value) return Boolean is
     (Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Result,
                                  Kind   => JSON_String_Type) and
      Preconditions.Ensure_Field (Object        => Object,
                                  Field         => Field_Names.Time,
                                  Kinds_Allowed => Preconditions.Number_Kind) and
      Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Steps,
                                  Kind   => JSON_Int_Type));

   type Prover_Result is (Valid, Unproved);
   --  FIXME: There are more, but right now we're only concerned with if it's
   --         proven or not.

   type T is new Entity.T with private;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   not overriding
   function Create (Object : JSON_Value;
                    Prover : Prover_Name) return T
     with Pre => Has_Required_Fields (Object => Object);

   ---------------------------------------------------------------------------
   --  Trivial_True
   ---------------------------------------------------------------------------
   not overriding
   function Trivial_True return T;
   --  Special Proof_Attempt instance that represents a trivially true proof.
   --
   --  Since GNAT_CE_2020 we can also have a "trivial_true" in the check_tree
   --  which - unlike a proper proof attempt - has no Result nor Time value, so
   --  we assume "Valid" and "no time" (i.e. 0.0 s). These kind of proof
   --  attempts are registered to a special prover object "Trivial" (which
   --  subsequently appears in the "stats" objects).

   --  Sorting instantiations.

   ---------------------------------------------------------------------------
   --  "<"
   --
   --  Comparison operator.
   ---------------------------------------------------------------------------
   not overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Prover
   ---------------------------------------------------------------------------
   not overriding
   function Prover (This : in T) return Prover_Name;

   ---------------------------------------------------------------------------
   --  Result
   ---------------------------------------------------------------------------
   not overriding
   function Result (This : in T) return Prover_Result;

   ---------------------------------------------------------------------------
   --  Steps
   ---------------------------------------------------------------------------
   not overriding
   function Steps (This : in T) return Prover_Steps;

   ---------------------------------------------------------------------------
   --  Time
   ---------------------------------------------------------------------------
   not overriding
   function Time (This : in T) return Duration;

private

   package Proof_Attempt_Ids is new SPAT.Unique_Ids;

   type T is new Entity.T with
      record
         Prover   : Prover_Name;          --  Prover involved.
         Result   : Result_Name;          --  "Valid", "Unknown", etc.
         Workload : Time_And_Steps;
         --  time spent during proof, and number of steps the prover took
         --  Steps might be negative (e.g. with Z3, the number of steps is
         --  recorded as -1 if Z3 ran out of memory).
         Id       : Proof_Attempt_Ids.Id; --  unique id for stable sorting
      end record;

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : in T) return String is
     (To_String (This.Prover) & ": " &
      Image (Value => This.Time,
             Steps => This.Steps) & ", " &
      To_String (This.Result));

   function Trivial_True return T is
      (T'(Entity.T with
              Prover    => Prover_Name (To_Name (Source => "Trivial")),
              Result    => Result_Name (To_Name (Source => "Valid")),
              Workload  => None,
              Id        => Proof_Attempt_Ids.Next));

   ---------------------------------------------------------------------------
   --  Prover
   ---------------------------------------------------------------------------
   not overriding
   function Prover (This : in T) return Prover_Name is
     (This.Prover);

   ---------------------------------------------------------------------------
   --  Result
   ---------------------------------------------------------------------------
   not overriding
   function Result (This : in T) return Prover_Result is
     (if This.Result = Result_Name (To_Name (Source => "Valid"))
      then Valid
      else Unproved);

   ---------------------------------------------------------------------------
   --  Steps
   ---------------------------------------------------------------------------
   not overriding
   function Steps (This : in T) return Prover_Steps is
     (This.Workload.Steps);

   ---------------------------------------------------------------------------
   --  Time
   ---------------------------------------------------------------------------
   not overriding
   function Time (This : in T) return Duration is
     (This.Workload.Time);

end SPAT.Proof_Attempt;
