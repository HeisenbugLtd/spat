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

limited with Ada.Containers;
limited with Ada.Strings.Unbounded.Hash;
with GNATCOLL.JSON;

package SPAT is

   subtype JSON_Data    is Ada.Strings.Unbounded.Unbounded_String;
   subtype Subject_Name is Ada.Strings.Unbounded.Unbounded_String;
   --  Type denoting some kind of name (i.e. file name, entity name, rule name
   --  etc. pp.)

   Null_Name : Subject_Name renames Ada.Strings.Unbounded.Null_Unbounded_String;
   --  Provide a renaming for the null string.

   type Prover_Steps is range -2 ** 63 .. 2 ** 63 - 1;
   --  Define our own type instead of using Long_Integer;

   pragma Compile_Time_Warning (Long_Integer'Size < Prover_Steps'Size,
                                "Long_Integer is less than 64 bit.");
   --  We use the Long_Integer version of GNATCOLL.JSON.Get to read values of
   --  this type, so the size of Long_Integer must be sufficient.

   ---------------------------------------------------------------------------
   --  Image function for Duration. Used by certain Image functions.
   ---------------------------------------------------------------------------
   function Image (Value : in Duration) return String;

   ---------------------------------------------------------------------------
   --  Image function for Duration and associated steps.
   ---------------------------------------------------------------------------
   function Image (Value : in Duration;
                   Steps : in Prover_Steps) return String;

   ---------------------------------------------------------------------------
   --  To_String
   ---------------------------------------------------------------------------
   function To_String (Source : in Subject_Name) return String renames
     Ada.Strings.Unbounded.To_String;

   ---------------------------------------------------------------------------
   --  To_Name
   ---------------------------------------------------------------------------
   function To_Name (Source : in String) return Subject_Name renames
     Ada.Strings.Unbounded.To_Unbounded_String;

   ---------------------------------------------------------------------------
   --  "="
   ---------------------------------------------------------------------------
   function "="
     (Left  : in Ada.Strings.Unbounded.Unbounded_String;
      Right : in Ada.Strings.Unbounded.Unbounded_String) return Boolean renames
     Ada.Strings.Unbounded."=";

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   function "<"
     (Left  : in Ada.Strings.Unbounded.Unbounded_String;
      Right : in Ada.Strings.Unbounded.Unbounded_String) return Boolean renames
     Ada.Strings.Unbounded."<";

   ---------------------------------------------------------------------------
   --  Hash
   ---------------------------------------------------------------------------
   function Hash (Key : Ada.Strings.Unbounded.Unbounded_String) return
     Ada.Containers.Hash_Type renames Ada.Strings.Unbounded.Hash;

   ---------------------------------------------------------------------------
   --  Length
   ---------------------------------------------------------------------------
   function Length
     (Source : in Ada.Strings.Unbounded.Unbounded_String) return Natural renames
     Ada.Strings.Unbounded.Length;

   --  Derived types for all kind of "names".
   type File_Name   is new Subject_Name; --  A file on disk.
   type Entity_Name is new Subject_Name; --  An Ada language entity.

   type SPARK_File_Name  is new File_Name; --  Name of a ".spark" file.
   type Source_File_Name is new File_Name; --  Name of an "Ada" source file.

   type Prover_Name is new Subject_Name;
   --  Name of a prover (e.g. "altergo", "Z3", ...)

   type Justification is new Subject_Name;
   --  Justification info (i.e. the "suppressed" field in a failed check).

   type Result_Name is new Subject_Name;
   --  A proof result (e.g. "Valid", "Unknown", "Timeout", ...)

   type Rule_Name is new Subject_Name;
   --  A verification condition rule (e.g. something like VC_RANGE_CHECK, ...)

   type Severity_Name is new Subject_Name;
   --  Severity of a message (e.g. "info", "medium", "error", ...)

   --  Type renames for commonly used JSON types from GNATCOLL.JSON
   subtype JSON_Array      is GNATCOLL.JSON.JSON_Array;
   subtype JSON_Value      is GNATCOLL.JSON.JSON_Value;
   subtype JSON_Value_Type is GNATCOLL.JSON.JSON_Value_Type;
   subtype UTF8_String     is GNATCOLL.JSON.UTF8_String;

   type File_Version is (GNAT_CE_2019, GNAT_CE_2020);
   --  Version information. Right now I only have access to the community
   --  releases of SPARK, so these are the only ones fully supported.

   --  Combine prover workload in a record capturing time and steps.
   type Time_And_Steps is
      record
         Time  : Duration;
         --  Time it took for a proof.
         Steps : Prover_Steps;
         --  Number of steps it took.
      end record;

   None : constant Time_And_Steps;

   ---------------------------------------------------------------------------
   --  Scaled
   --
   --  Implement prover specific steps scaling.
   --
   --  The current versions of SPARK put unscaled (raw) steps into the
   --  check_tree array.  This has been fixed in the development version of
   --  SPARK, so we will need means to figure out if scaling is needed or
   --  not.  If there is a valid proof in the file this can be achieved by
   --  comparing the steps in the check_tree array with the ones reported in
   --  the stats object, else we're out of luck.
   ---------------------------------------------------------------------------
   function Scaled (Prover    : in Prover_Name;
                    Raw_Steps : in Prover_Steps) return Prover_Steps;

private

   None : constant Time_And_Steps := Time_And_Steps'(Time  => 0.0,
                                                     Steps => 0);

end SPAT;
