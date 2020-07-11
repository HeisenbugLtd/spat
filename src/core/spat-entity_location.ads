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
--             on a source line and column.
--
------------------------------------------------------------------------------

with SPAT.Entity_Line;
with SPAT.Field_Names;
with SPAT.Preconditions;

package SPAT.Entity_Location is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object : in JSON_Value) return Boolean is
     (Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.File,
                                  Kind   => JSON_String_Type) and
      Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Line,
                                  Kind   => JSON_Int_Type));

   type T is new Entity_Line.T with private;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding
   function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   not overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : in T) return String;

   ---------------------------------------------------------------------------
   --  Source_Column
   ---------------------------------------------------------------------------
   not overriding
   function Source_Column (This : in T) return Natural;

private

   type T is new Entity_Line.T with
      record
         Column : Natural;
      end record;

   ---------------------------------------------------------------------------
   --  Source_Column
   ---------------------------------------------------------------------------
   not overriding
   function Source_Column (This : in T) return Natural is
     (This.Column);

end SPAT.Entity_Location;
