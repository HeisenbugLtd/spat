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
--  S.P.A.T. - Root package
--
------------------------------------------------------------------------------
with Ada.Containers;
with Ada.Strings.Unbounded.Hash;
with GNATCOLL.JSON;

package SPAT is

   subtype JSON_Data    is Ada.Strings.Unbounded.Unbounded_String;
   subtype Subject_Name is Ada.Strings.Unbounded.Unbounded_String;
   --  Type denoting some kind of name (i.e. file name, entity name, rule name
   --  etc. pp.)

   Null_Name : Subject_Name renames Ada.Strings.Unbounded.Null_Unbounded_String;
   --  Provide a renaming for the null string.

   function To_String (Source : in Subject_Name) return String renames
     Ada.Strings.Unbounded.To_String;

   function To_Name (Source : in String) return Subject_Name renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   function "="
     (Left  : in Ada.Strings.Unbounded.Unbounded_String;
      Right : in Ada.Strings.Unbounded.Unbounded_String) return Boolean renames
     Ada.Strings.Unbounded."=";

   function "<"
     (Left  : in Ada.Strings.Unbounded.Unbounded_String;
      Right : in Ada.Strings.Unbounded.Unbounded_String) return Boolean renames
     Ada.Strings.Unbounded."<";

   function Hash (Key : Ada.Strings.Unbounded.Unbounded_String) return
     Ada.Containers.Hash_Type renames Ada.Strings.Unbounded.Hash;

   function Length
     (Source : in Ada.Strings.Unbounded.Unbounded_String) return Natural renames
     Ada.Strings.Unbounded.Length;

   --  Type renames for commonly used JSON types from GNATCOLL.JSON
   subtype JSON_Array      is GNATCOLL.JSON.JSON_Array;
   subtype JSON_Value      is GNATCOLL.JSON.JSON_Value;
   subtype JSON_Value_Type is GNATCOLL.JSON.JSON_Value_Type;
   subtype UTF8_String     is GNATCOLL.JSON.UTF8_String;

   type File_Version is (GNAT_CE_2019, GNAT_CE_2020);
   --  Version information. Right now I only have access to the community
   --  releases of SPARK, so these are the only ones fully supported.

end SPAT;
