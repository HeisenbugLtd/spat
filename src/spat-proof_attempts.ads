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

with Ada.Containers.Vectors;

with SPAT.Field_Names;
with SPAT.Preconditions;

package SPAT.Proof_Attempts is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object : JSON_Value) return Boolean is
     (Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Result,
                                  Kind   => JSON_String_Type) and then
      Preconditions.Ensure_Field (Object        => Object,
                                  Field         => Field_Names.Time,
                                  Kinds_Allowed => Preconditions.Number_Kind));

   type T is tagged
      record
         Prover     : Subject_Name; --  Prover involved.
         Result     : Subject_Name; --  "Valid", "Unknown", etc.
         Time       : Duration;     --  time spent during proof
         --  Steps -- part of the JSON data, but we don't care.
      end record;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : JSON_Value;
                    Prover : Subject_Name) return T
     with Pre => Has_Required_Fields (Object => Object);

   --  Sorting instantiations.

   ---------------------------------------------------------------------------
   --  Slower_Than
   ---------------------------------------------------------------------------
   not overriding function Slower_Than (Left  : in T;
                                        Right : in T) return Boolean is
     (Left.Time > Right.Time);

   package Vectors is new
     Ada.Containers.Vectors (Index_Type   => Ada.Containers.Count_Type,
                             Element_Type => T);

   type Vector is new Vectors.Vector with null record;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in Vector) return Boolean;

   ---------------------------------------------------------------------------
   --  Sort_By_Duration
   ---------------------------------------------------------------------------
   not overriding
   procedure Sort_By_Duration (Container : in out Vector);

   Empty_Vector : Vector := (Vectors.Empty_Vector with null record);

end SPAT.Proof_Attempts;