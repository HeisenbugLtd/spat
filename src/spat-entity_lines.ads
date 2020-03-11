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
with SPAT.Field_Names;
with SPAT.Preconditions;

package SPAT.Entity_Lines is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   function Has_Required_Fields (Object : in JSON_Value) return Boolean is
     (Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.File,
                                  Kind   => JSON_String_Type) and then
      Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Line,
                                  Kind   => JSON_Int_Type));

   type T is tagged
      record
         File : Subject_Name;
         Line : Natural;
      end record;

   not overriding function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   not overriding function Image (This : T) return String;

   package Vectors is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => T);

   subtype Vector is Vectors.Vector;

end SPAT.Entity_Lines;
