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

   type T is new Entity_Location.T with private;

   ---------------------------------------------------------------------------
   --  Create
   --
   --  This is a dummy. The object returned by Create is empty. For the real
   --  "creation" of a proper object, you need to call Add_To_Tree.
   ---------------------------------------------------------------------------
   overriding
   function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   ---------------------------------------------------------------------------
   --  Add_To_Tree
   ---------------------------------------------------------------------------
   procedure Add_To_Tree (Object : in     JSON_Value;
                          Tree   : in out Entity.Tree.T;
                          Parent : in     Entity.Tree.Cursor) with
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
   --  Suppressed
   ---------------------------------------------------------------------------
   not overriding
   function Suppressed (This : in T) return Subject_Name;

   ---------------------------------------------------------------------------
   --  Before
   ---------------------------------------------------------------------------
   function Before (Left  : in Entity.T'Class;
                    Right : in Entity.T'Class) return Boolean is
     (Proof_Item.T (Left).Total_Time > Proof_Item.T (Right).Total_Time);

   package By_Duration is new
     Entity.Tree.Generic_Sorting (Before => Before);

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
   --  Has_Unproved_Attempts
   ---------------------------------------------------------------------------
   function Is_Unproved (This : in Checks_Sentinel) return Boolean;

private

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
         Suppressed            : Subject_Name;
         Rule                  : Subject_Name;
         Severity              : Subject_Name;
         Max_Time              : Duration; --  Longest time spent in proof (successful or not)
         Total_Time            : Duration; --  Accumulated proof time.
         Has_Failed_Attempts   : Boolean;
         Has_Unproved_Attempts : Boolean;
      end record;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean is
      (This.Has_Failed_Attempts);

   ---------------------------------------------------------------------------
   --  Has_Unproved_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Unproved_Attempts (This : in T) return Boolean is
      (This.Has_Unproved_Attempts);

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
   --  Slower_Than
   ---------------------------------------------------------------------------
   not overriding
   function Slower_Than (Left  : in T;
                         Right : in T) return Boolean is
     (Left.Total_Time > Right.Total_Time);

   ---------------------------------------------------------------------------
   --  Suppressed
   ---------------------------------------------------------------------------
   not overriding
   function Suppressed (This : in T) return Subject_Name is
     (This.Suppressed);

   ---------------------------------------------------------------------------
   --  Total_Time
   ---------------------------------------------------------------------------
   not overriding
   function Total_Time (This : in T) return Duration is
     (This.Total_Time);

end SPAT.Proof_Item;
