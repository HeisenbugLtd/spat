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
--  S.P.A.T. - Object representing a JSON "flow" object.
--
------------------------------------------------------------------------------
with Ada.Containers.Vectors;

with SPAT.Entity_Locations;
with SPAT.Preconditions;

private package SPAT.Flow_Items is

   function Has_Required_Fields (Object : in JSON_Value) return Boolean is
     (Entity_Locations.Has_Required_Fields (Object => Object) and
      Preconditions.Ensure_Rule_Severity (Object => Object));

   type T is new Entity_Locations.T with
      record
         Rule     : Subject_Name;
         Severity : Subject_Name;
      end record;

   overriding function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   package Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => T);

   subtype Vector is Vectors.Vector;

   package By_Location is new Vectors.Generic_Sorting ("<" => "<");

end SPAT.Flow_Items;
