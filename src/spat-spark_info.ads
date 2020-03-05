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
--  S.P.A.T. - Map information from JSON data into internal data structure.
--
--  Collect file contents.
--
------------------------------------------------------------------------------
with Ada.Containers.Generic_Array_Sort;
with Ada.Strings.Unbounded;
with GNATCOLL.JSON;

private with Ada.Containers.Hashed_Maps;
private with Ada.Containers.Vectors;
private with Ada.Strings.Unbounded.Hash;

package SPAT.Spark_Info is

   subtype Entity_Name   is Ada.Strings.Unbounded.Unbounded_String;
   subtype File_Name     is Ada.Strings.Unbounded.Unbounded_String;
   subtype Rule_Name     is Ada.Strings.Unbounded.Unbounded_String;
   subtype Severity_Name is Ada.Strings.Unbounded.Unbounded_String;

   --  Helper types.
   type String_Array is array (Positive range <>) of Ada.Strings.Unbounded.Unbounded_String;

   procedure Sort_By_Name is new
     Ada.Containers.Generic_Array_Sort
       (Index_Type   => Positive,
        Element_Type => Ada.Strings.Unbounded.Unbounded_String,
        Array_Type   => String_Array,
        "<"          => Ada.Strings.Unbounded."<");

   type T is tagged limited private;
   --  Binary representation of the information obtained from a .spark JSON
   --  file.

   procedure Map_SPARK_File (This :    out T;
                             Root : in     GNATCOLL.JSON.JSON_Value);
   --  Traverses through the JSON data given in Root and translates it into the
   --  data structure given in This.

   function List_All_Entities (This : in T) return String_Array;
   --  Returns a sorted list of all entities (source unit names) currently
   --  stored in This.

   --  Access functions.
   function Num_Flows (This : in T) return Ada.Containers.Count_Type;
   function Flow_Time (This : in T) return Duration;

   function Num_Proofs (This : in T) return Ada.Containers.Count_Type;
   function Proof_Time (This : in T) return Duration;


private

   --  Checks that the given JSON object contains name and line indicating a
   --  source code position (i.e. line of declaration of an entity).
   --  Returns True if so, False otherwise.
   function Ensure_File_Line
     (Object : in GNATCOLL.JSON.JSON_Value) return Boolean;

   --  Checks that the given JSON object contains name, line and column values
   --  indicating a source code position.
   --  Returns True if so, False otherwise.
   function Ensure_File_Line_Column
     (Object : in GNATCOLL.JSON.JSON_Value) return Boolean;

   --  Checks that the given JSON object contains a rule and a severity object.
   --  Returns True if so, False otherwise.
   function Ensure_Rule_Severity
     (Object : in GNATCOLL.JSON.JSON_Value) return Boolean;

   --  Information obtained from the timing section of a .spark file.
   type Timing_Item is
      record
         Proof : Duration; --  Total time the prover spent.
         Flow  : Duration; --  Total time of flow analysis.
      end record;

   Null_Timing_Item : constant Timing_Item := Timing_Item'(Proof => 0.0,
                                                           Flow  => 0.0);

   type Entity_Line is tagged
      record
         File : File_Name;
         Line : Natural;
      end record;

   not overriding function Create
     (Object : in GNATCOLL.JSON.JSON_Value) return Entity_Line with
     Pre => Ensure_File_Line (Object => Object);

   package Entity_Lines is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Entity_Line);

   type Entity_Location is new Entity_Line with
      record
         Column : Natural;
      end record;

   overriding function Create
     (Object : in GNATCOLL.JSON.JSON_Value) return Entity_Location with
     Pre => Ensure_File_Line_Column (Object => Object);

   not overriding function "<" (Left  : in Entity_Location;
                                Right : in Entity_Location) return Boolean;

   -- TODO: There does not seem to be a real need for distinguishing between
   --       flow and proof items, all handling could be same.
   type Flow_Item is new Entity_Location with
      record
         Rule     : Rule_Name;
         Severity : Severity_Name;
      end record;

   overriding function Create
     (Object : in GNATCOLL.JSON.JSON_Value) return Flow_Item with
     Pre => (Ensure_File_Line_Column (Object => Object) and then
             Ensure_Rule_Severity (Object => Object));

   package Flow_Items is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Flow_Item);

   package Flow_Items_By_Location is new
     Flow_Items.Generic_Sorting ("<" => "<");

   type Proof_Item is new Entity_Location with
      record
         Rule     : Rule_Name;
         Severity : Severity_Name;
      end record;

   overriding function Create
     (Object : in GNATCOLL.JSON.JSON_Value) return Proof_Item with
     Pre => (Ensure_File_Line_Column (Object => Object) and then
             Ensure_Rule_Severity (Object => Object));

   package Proof_Items is
     new Ada.Containers.Vectors (Index_Type   => Positive,
                                 Element_Type => Proof_Item);

   package Proof_Items_By_Location is new
     Proof_Items.Generic_Sorting ("<" => "<");

   type Analyzed_Entity is
      record
         Source_Lines : Entity_Lines.Vector;
         Flows        : Flow_Items.Vector;
         Proofs       : Proof_Items.Vector;
      end record;

   --  Type representing a source (file) entity.
   package Analyzed_Entities is new
     Ada.Containers.Hashed_Maps (Key_Type        => Entity_Name,
                                 Element_Type    => Analyzed_Entity,
                                 Hash            => Ada.Strings.Unbounded.Hash,
                                 Equivalent_Keys => Ada.Strings.Unbounded."=",
                                 "="             => "=");

   type T is tagged limited
      record
         Entities : Analyzed_Entities.Map := Analyzed_Entities.Empty_Map;
         Timings  : Timing_Item           := Null_Timing_Item;
      end record;

end SPAT.Spark_Info;
