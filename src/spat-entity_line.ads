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

with SPAT.Entity;
with SPAT.Field_Names;
with SPAT.Preconditions;

package SPAT.Entity_Line is

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

   type T is new Entity.T with private;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   not overriding
   function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : T) return String;

   ---------------------------------------------------------------------------
   --  Source_File
   ---------------------------------------------------------------------------
   not overriding
   function Source_File (This : in T) return Source_File_Name;

   ---------------------------------------------------------------------------
   --  Source_Line
   ---------------------------------------------------------------------------
   not overriding
   function Source_Line (This : in T) return Natural;

private

   type T is new Entity.T with
      record
         --  These are source file references.
         File : Source_File_Name;
         Line : Natural;
      end record;

   ---------------------------------------------------------------------------
   --  Source_File
   ---------------------------------------------------------------------------
   not overriding
   function Source_File (This : in T) return Source_File_Name is
      (This.File);

   ---------------------------------------------------------------------------
   --  Source_Line
   ---------------------------------------------------------------------------
   not overriding
   function Source_Line (This : in T) return Natural is
     (This.Line);

end SPAT.Entity_Line;
