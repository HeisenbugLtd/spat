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

   --  Type renames for commonly used JSON types from GNATCOLL.JSON
   subtype JSON_Array      is GNATCOLL.JSON.JSON_Array;
   subtype JSON_Value      is GNATCOLL.JSON.JSON_Value;
   subtype JSON_Value_Type is GNATCOLL.JSON.JSON_Value_Type;
   subtype UTF8_String     is GNATCOLL.JSON.UTF8_String;

   --  Encapsulates names of recognized fields in JSON data.
   package Field_Names is

      Assumptions    : constant UTF8_String := "assumptions";
      Check_Tree     : constant UTF8_String := "check_tree";
      Column         : constant UTF8_String := "col";
      Entity         : constant UTF8_String := "entity";
      File           : constant UTF8_String := "file";
      Flow           : constant UTF8_String := "flow";
      Flow_Analysis  : constant UTF8_String := "flow analysis";
      Line           : constant UTF8_String := "line";
      Name           : constant UTF8_String := "name";
      Proof          : constant UTF8_String := "proof";
      Proof_Attempts : constant UTF8_String := "proof_attempts";
      Result         : constant UTF8_String := "result";
      Rule           : constant UTF8_String := "rule";
      Severity       : constant UTF8_String := "severity";
      Sloc           : constant UTF8_String := "sloc";
      Spark          : constant UTF8_String := "spark";
      Steps          : constant UTF8_String := "steps";
      Time           : constant UTF8_String := "time";
      Timings        : constant UTF8_String := "timings";

   end Field_Names;

end SPAT;
