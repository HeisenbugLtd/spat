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
--  S.P.A.T. - Object representing an entity (name) with a file location based
--             on a single source line (no column information).
--
------------------------------------------------------------------------------
with Ada.Containers.Vectors;
with SPAT.Preconditions;

private package SPAT.Entity_Lines is

   type T is tagged
      record
         File : Subject_Name;
         Line : Natural;
      end record;

   not overriding function Create (Object : in JSON_Value) return T with
     Pre => Preconditions.Ensure_File_Line (Object => Object);

   package Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => T);

   subtype Vector is Vectors.Vector;

end SPAT.Entity_Lines;
