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
--  S.P.A.T. - Object representing a JSON "proof" object.
--
------------------------------------------------------------------------------
with Ada.Containers.Vectors;

with SPAT.Entity_Locations;
with SPAT.Field_Names;
with SPAT.Preconditions;
with SPAT.Proof_Attempts;

package SPAT.Proof_Items is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object : in JSON_Value) return Boolean is
      (Entity_Locations.Has_Required_Fields (Object => Object) and then
       Preconditions.Ensure_Rule_Severity (Object => Object) and then
       Preconditions.Ensure_Field (Object => Object,
                                   Field  => Field_Names.Check_Tree,
                                   Kind   => JSON_Array_Type));

   package Checks_Tree is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Proof_Attempts.Vector,
                             "="          => Proof_Attempts."=");

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   function "<" (Left  : in Proof_Attempts.Vector;
                 Right : in Proof_Attempts.Vector) return Boolean;

   package Checks_By_Duration is new Checks_Tree.Generic_Sorting ("<" => "<");

   type T is new Entity_Locations.T with
      record
         Rule       : Subject_Name;
         Severity   : Subject_Name;
         Check_Tree : Checks_Tree.Vector;
         Max_Time   : Duration; --  Longest time spent in proof (successful or not)
         Total_Time : Duration; --  Accumulated proof time.
      end record;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Slower_Than
   ---------------------------------------------------------------------------
   not overriding function Slower_Than (Left  : in T;
                                        Right : in T) return Boolean is
     (Left.Total_Time > Right.Total_Time);

   package Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => T);

   subtype Vector is Vectors.Vector;

   --  Sorting instantiations.
   package By_Location is new Vectors.Generic_Sorting ("<" => "<");
   package By_Duration is new Vectors.Generic_Sorting ("<" => Slower_Than);

end SPAT.Proof_Items;
