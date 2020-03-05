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
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;

package SPAT is

   subtype Entity_Name   is Ada.Strings.Unbounded.Unbounded_String;
   subtype File_Name     is Ada.Strings.Unbounded.Unbounded_String;
   subtype Rule_Name     is Ada.Strings.Unbounded.Unbounded_String;
   subtype Severity_Name is Ada.Strings.Unbounded.Unbounded_String;

   --  Type renames for commonly used JSON types from GNATCOLL.JSON
   subtype JSON_Array      is GNATCOLL.JSON.JSON_Array;
   subtype JSON_Value      is GNATCOLL.JSON.JSON_Value;
   subtype JSON_Value_Type is GNATCOLL.JSON.JSON_Value_Type;
   subtype UTF8_String     is GNATCOLL.JSON.UTF8_String;

   --  Encapsulates names of recognized fields in JSON data.
   package Field_Names is

      Assumptions   : constant UTF8_String := "assumptions";
      Column        : constant UTF8_String := "col";
      Entity        : constant UTF8_String := "entity";
      File          : constant UTF8_String := "file";
      Flow          : constant UTF8_String := "flow";
      Flow_Analysis : constant UTF8_String := "flow analysis";
      Line          : constant UTF8_String := "line";
      Name          : constant UTF8_String := "name";
      Proof         : constant UTF8_String := "proof";
      Rule          : constant UTF8_String := "rule";
      Severity      : constant UTF8_String := "severity";
      Sloc          : constant UTF8_String := "sloc";
      Spark         : constant UTF8_String := "spark";
      Timings       : constant UTF8_String := "timings";

   end Field_Names;

end SPAT;
