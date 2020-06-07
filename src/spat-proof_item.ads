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
--  S.P.A.T. - Object representing a JSON "proof" object.
--
------------------------------------------------------------------------------

limited with Ada.Containers.Vectors;
with SPAT.Entity_Location;
with SPAT.Field_Names;
with SPAT.Preconditions;
with SPAT.Proof_Attempt.List;

package SPAT.Proof_Item is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object : in JSON_Value) return Boolean is
      (Entity_Location.Has_Required_Fields (Object => Object) and
       Preconditions.Ensure_Rule_Severity (Object => Object) and
       Preconditions.Ensure_Field (Object => Object,
                                   Field  => Field_Names.Check_Tree,
                                   Kind   => JSON_Array_Type));

   package Checks_Tree is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Proof_Attempt.List.T,
                             "="          => Proof_Attempt.List."=");

   package Checks_By_Duration is new
     Checks_Tree.Generic_Sorting ("<" => Proof_Attempt.List."<");

   type T is new Entity_Location.T with private;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   ---------------------------------------------------------------------------
   --  Slower_Than
   ---------------------------------------------------------------------------
   not overriding
   function Slower_Than (Left  : in T;
                         Right : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Has_Unproved_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Unproved_Attempts (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Rule
   ---------------------------------------------------------------------------
   not overriding
   function Rule (This : in T) return Subject_Name;

   ---------------------------------------------------------------------------
   --  Max_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Time (This : in T) return Duration;

   ---------------------------------------------------------------------------
   --  Total_Time
   ---------------------------------------------------------------------------
   not overriding
   function Total_Time (This : in T) return Duration;

   ---------------------------------------------------------------------------
   --  Check_Tree
   ---------------------------------------------------------------------------
   not overriding
   function Check_Tree (This : in T) return Checks_Tree.Vector;

   ---------------------------------------------------------------------------
   --  Suppressed
   ---------------------------------------------------------------------------
   not overriding
   function Suppressed (This : in T) return Subject_Name;

private

   type T is new Entity_Location.T with
      record
         Suppressed : Subject_Name;
         Rule       : Subject_Name;
         Severity   : Subject_Name;
         Check_Tree : Checks_Tree.Vector;
         Max_Time   : Duration; --  Longest time spent in proof (successful or not)
         Total_Time : Duration; --  Accumulated proof time.
      end record;

   ---------------------------------------------------------------------------
   --  Slower_Than
   ---------------------------------------------------------------------------
   not overriding
   function Slower_Than (Left  : in T;
                         Right : in T) return Boolean is
     (Left.Total_Time > Right.Total_Time);

   ---------------------------------------------------------------------------
   --  Max_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Time (This : in T) return Duration is
     (This.Max_Time);

   ---------------------------------------------------------------------------
   --  Rule
   ---------------------------------------------------------------------------
   not overriding
   function Rule (This : in T) return Subject_Name is
     (This.Rule);

   ---------------------------------------------------------------------------
   --  Total_Time
   ---------------------------------------------------------------------------
   not overriding
   function Total_Time (This : in T) return Duration is
     (This.Total_Time);

   ---------------------------------------------------------------------------
   --  Check_Tree
   ---------------------------------------------------------------------------
   not overriding
   function Check_Tree (This : in T) return Checks_Tree.Vector is
     (This.Check_Tree);

   ---------------------------------------------------------------------------
   --  Suppressed
   ---------------------------------------------------------------------------
   not overriding
   function Suppressed (This : in T) return Subject_Name is
     (This.Suppressed);

end SPAT.Proof_Item;
