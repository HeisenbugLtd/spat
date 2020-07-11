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
--  S.P.A.T. - Object representing a list of JSON "proof attempt" objects.
--
------------------------------------------------------------------------------

limited with Ada.Containers.Vectors;

package SPAT.Proof_Attempt.List is

   package Implementation is

      package Vectors is new
        Ada.Containers.Vectors (Index_Type   => Ada.Containers.Count_Type,
                                Element_Type => T);

   end Implementation;

   type T is new Implementation.Vectors.Vector with private;

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   not overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Is_Unproved
   ---------------------------------------------------------------------------
   not overriding
   function Is_Unproved (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Sort_By_Duration
   ---------------------------------------------------------------------------
   not overriding
   procedure Sort_By_Duration (Container : in out T);

private

   type T is new Implementation.Vectors.Vector with null record;

end SPAT.Proof_Attempt.List;
