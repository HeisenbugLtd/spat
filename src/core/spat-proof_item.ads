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

private with Ada.Tags;
with SPAT.Entity_Location;
with SPAT.Entity.Tree;
with SPAT.Field_Names;
with SPAT.Preconditions;
private with SPAT.Unique_Ids;

package SPAT.Proof_Item is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object  : in JSON_Value;
                                 Version : in File_Version) return Boolean is
      (Entity_Location.Has_Required_Fields (Object => Object) and
       Preconditions.Ensure_Rule_Severity (Object => Object) and
       Preconditions.Ensure_Field (Object      => Object,
                                   Field       => Field_Names.Check_Tree,
                                   Kind        => JSON_Array_Type,
                                   Is_Optional =>
                                     (case Version is
                                         when GNAT_CE_2019 => False,
                                         when GNAT_CE_2020 => True)));

   type T is new Entity_Location.T with private;

   ---------------------------------------------------------------------------
   --  Create
   --
   --  This is a dummy. The object returned by Create is empty. For the real
   --  "creation" of a proper object, you need to call Add_To_Tree.
   ---------------------------------------------------------------------------
   overriding
   function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object  => Object,
                                 Version => GNAT_CE_2019);

   ---------------------------------------------------------------------------
   --  "<"
   --
   --  Comparison operator for proof items.
   ---------------------------------------------------------------------------
   overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Add_To_Tree
   ---------------------------------------------------------------------------
   procedure Add_To_Tree (Object  : in     JSON_Value;
                          Version : in     File_Version;
                          Tree    : in out Entity.Tree.T;
                          Parent  : in     Entity.Tree.Cursor) with
     Pre => Has_Required_Fields (Object  => Object,
                                 Version => Version);

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   not overriding
   function Has_Unproved_Attempts (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : in T) return String;

   ---------------------------------------------------------------------------
   --  Is_Unjustified
   ---------------------------------------------------------------------------
   not overriding
   function Is_Unjustified (This : in T) return Boolean;

   ---------------------------------------------------------------------------
   --  Rule
   ---------------------------------------------------------------------------
   not overriding
   function Rule (This : in T) return Rule_Name;

   ---------------------------------------------------------------------------
   --  Max_Success_Steps
   ---------------------------------------------------------------------------
   not overriding
   function Max_Success_Steps (This : in T) return Prover_Steps;

   ---------------------------------------------------------------------------
   --  Max_Success_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Success_Time (This : in T) return Duration;

   ---------------------------------------------------------------------------
   --  Max_Steps
   ---------------------------------------------------------------------------
   not overriding
   function Max_Steps (This : in T) return Prover_Steps;

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
   --  Suppressed
   ---------------------------------------------------------------------------
   not overriding
   function Suppressed (This : in T) return Justification;

   ---------------------------------------------------------------------------
   --  Before
   ---------------------------------------------------------------------------
   function Before (Left  : in Entity.T'Class;
                    Right : in Entity.T'Class) return Boolean is
     (Proof_Item.T (Left) < Proof_Item.T (Right));

   package By_Duration is new Entity.Tree.Generic_Sorting (Before => Before);

   ---------------------------------------------------------------------------
   --  Sort_By_Duration
   ---------------------------------------------------------------------------
   procedure Sort_By_Duration (Tree   : in out Entity.Tree.T;
                               Parent : in     Entity.Tree.Cursor)
     renames By_Duration.Sort;

   type Checks_Sentinel is new Entity.T with private;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   function Has_Failed_Attempts (This : in Checks_Sentinel) return Boolean;

   ---------------------------------------------------------------------------
   --  Is_Unproved
   ---------------------------------------------------------------------------
   function Is_Unproved (This : in Checks_Sentinel) return Boolean;

private

   package Proof_Item_Ids is new Unique_Ids;

   type Checks_Sentinel is new Entity.T with
      record
         Has_Failed_Attempts : Boolean;
         Is_Unproved         : Boolean;
      end record;

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : in Checks_Sentinel) return String is
     (Ada.Tags.External_Tag (Checks_Sentinel'Class (This)'Tag) & ": " &
        "(Has_Failed_Attempts => " & This.Has_Failed_Attempts'Image &
        ", Is_Unproved => " & This.Is_Unproved'Image & ")");

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in Checks_Sentinel) return Boolean is
      (This.Has_Failed_Attempts);

   ---------------------------------------------------------------------------
   --  Is_Unproved
   ---------------------------------------------------------------------------
   not overriding
   function Is_Unproved (This : in Checks_Sentinel) return Boolean is
     (This.Is_Unproved);

   type Proof_Item_Sentinel is new Entity.T with null record;

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : in Proof_Item_Sentinel) return String is
     (Ada.Tags.External_Tag (Proof_Item_Sentinel'Class (This)'Tag) & ": ()");

   type T is new Entity_Location.T with
      record
         Suppressed            : Justification;
         Rule                  : Rule_Name;
         Severity              : Severity_Name;
         Max_Success           : Time_And_Steps;
         --  Longest time/steps for a successful proof.
         Max_Proof             : Time_And_Steps;
         --  Longest time/steps spent in proof (successful or not).
         Total_Time            : Duration;
         --  Accumulated proof time.
         Id                    : Proof_Item_Ids.Id;
         --  Id for stable sorting.
         Has_Failed_Attempts   : Boolean;
         Has_Unproved_Attempts : Boolean;
         Is_Unjustified        : Boolean;
      end record;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean is
      (This.Has_Failed_Attempts);

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : in T) return String is
     (To_String (This.Rule) & " " &
        Entity_Location.T (This).Image & " => " &
        (if This.Has_Unproved_Attempts
         then "--"
         else Image (Value => This.Max_Success_Time,
                     Steps => This.Max_Success_Steps)) & "/" &
        Image (Value => This.Max_Time,
               Steps => This.Max_Steps) & "/" &
        Image (Value => This.Total_Time));

   ---------------------------------------------------------------------------
   --  Is_Unjustified
   ---------------------------------------------------------------------------
   not overriding
   function Is_Unjustified (This : in T) return Boolean is
      (This.Is_Unjustified);

   ---------------------------------------------------------------------------
   --  Has_Unproved_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Unproved_Attempts (This : in T) return Boolean is
      (This.Has_Unproved_Attempts);

   ---------------------------------------------------------------------------
   --  Max_Steps
   ---------------------------------------------------------------------------
   not overriding
   function Max_Steps (This : in T) return Prover_Steps is
     (This.Max_Proof.Steps);

   ---------------------------------------------------------------------------
   --  Max_Success_Steps
   ---------------------------------------------------------------------------
   not overriding
   function Max_Success_Steps (This : in T) return Prover_Steps is
     (This.Max_Success.Steps);

   ---------------------------------------------------------------------------
   --  Max_Success_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Success_Time (This : in T) return Duration is
     (This.Max_Success.Time);

   ---------------------------------------------------------------------------
   --  Max_Time
   ---------------------------------------------------------------------------
   not overriding
   function Max_Time (This : in T) return Duration is
     (This.Max_Proof.Time);

   ---------------------------------------------------------------------------
   --  Rule
   ---------------------------------------------------------------------------
   not overriding
   function Rule (This : in T) return Rule_Name is
     (This.Rule);

   ---------------------------------------------------------------------------
   --  Suppressed
   ---------------------------------------------------------------------------
   not overriding
   function Suppressed (This : in T) return Justification is
     (This.Suppressed);

   ---------------------------------------------------------------------------
   --  Total_Time
   ---------------------------------------------------------------------------
   not overriding
   function Total_Time (This : in T) return Duration is
     (This.Total_Time);

end SPAT.Proof_Item;
