------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Containers.Vectors;
with SPAT.Proof_Attempt.List;

package body SPAT.Proof_Item is

   package Checks_Lists is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Proof_Attempt.List.T,
                             "="          => Proof_Attempt.List."=");

   package Checks_By_Duration is new
     Checks_Lists.Generic_Sorting ("<" => Proof_Attempt.List."<");

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   --  FIXME: We should be able to sort by Max_Success_Time, too.
   overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean is
      use type Proof_Item_Ids.Id;
   begin
      --  First by total time.
      if Left.Total_Time /= Right.Total_Time then
         return Left.Total_Time > Right.Total_Time;
      end if;

      --  Total time does not differ, try max time.
      if Left.Max_Time /= Right.Max_Time then
         return Left.Max_Time > Right.Max_Time;
      end if;

      --  By Rule (i.e. VC_LOOP_INVARIANT, VC_PRECONDITION, etc. pp.)
      if Left.Rule /= Right.Rule then
         return Left.Rule < Right.Rule;
      end if;

      --  By Severity (i.e. "info", "warning", ...)
      if Left.Severity /= Right.Severity then
         --  TODO: We should get a list of severities and actually sort them by
         --        priority.   For now, textual is all we have.
         return Left.Severity < Right.Severity;
      end if;

      --  Locations are equal, go for last resort, the unique id.
      if Entity_Location."=" (X => Entity_Location.T (Left),
                              Y => Entity_Location.T (Right))
      then
         return Left.Id < Right.Id;
      end if;

      --  By location (i.e. file:line:column).
      return Entity_Location."<" (Left  => Entity_Location.T (Left),
                                  Right => Entity_Location.T (Right));
   end "<";

   ---------------------------------------------------------------------------
   --  Add_To_Tree
   ---------------------------------------------------------------------------
   procedure Add_To_Tree (Object  : in     JSON_Value;
                          Version : in     File_Version;
                          Tree    : in out Entity.Tree.T;
                          Parent  : in     Entity.Tree.Cursor)
   is
      pragma Unreferenced (Version); --  Only for precondition.

      --  Collect information about the timing of dependent attempts.
      Max_Proof   : Time_And_Steps := None;
      Max_Success : Time_And_Steps := None;
      Total_Time  : Duration := 0.0;

      Checks_List : Checks_Lists.Vector;
      Check_Tree  : constant JSON_Array :=
        Object.Get (Field => Field_Names.Check_Tree);
      Suppressed_Msg : constant Justification :=
        (if Object.Has_Field (Field => Field_Names.Suppressed)
         then
            Justification
              (Subject_Name'(Object.Get (Field => Field_Names.Suppressed)))
         else Justification (Null_Name)); --  FIXME: Missing type check.
   begin
      --  Walk along the check_tree array to find all proof attempts and their
      --  respective times.
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Check_Tree) loop
         declare
            Attempts : Proof_Attempt.List.T;
            Element  : constant JSON_Value   :=
                         GNATCOLL.JSON.Get (Arr   => Check_Tree,
                                            Index => I);
         begin
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Proof_Attempts,
                                          Kind   => JSON_Object_Type)
            then
               declare
                  Attempt_List : constant JSON_Value
                    := Element.Get (Field => Field_Names.Proof_Attempts);

                  ------------------------------------------------------------
                  --  Mapping_CB
                  ------------------------------------------------------------
                  procedure Mapping_CB (Name  : in UTF8_String;
                                        Value : in JSON_Value);

                  ------------------------------------------------------------
                  --  Mapping_CB
                  ------------------------------------------------------------
                  procedure Mapping_CB (Name  : in UTF8_String;
                                        Value : in JSON_Value) is
                  begin
                     if
                       Proof_Attempt.Has_Required_Fields (Object => Value)
                     then
                        declare
                           Attempt : constant Proof_Attempt.T :=
                                       Proof_Attempt.Create
                                         (Prover => Prover_Name (To_Name (Name)),
                                          Object => Value);
                           use type Proof_Attempt.Prover_Result;
                        begin
                           Attempts.Append (New_Item => Attempt);

                           Max_Proof :=
                             Time_And_Steps'
                               (Time  =>
                                  Duration'Max (Max_Proof.Time,
                                                Attempt.Time),
                                Steps =>
                                  Prover_Steps'Max (Max_Proof.Steps,
                                                    Attempt.Steps));

                           if Attempt.Result = Proof_Attempt.Valid then
                              Max_Success :=
                                Time_And_Steps'
                                  (Time  =>
                                     Duration'Max (Max_Success.Time,
                                                   Attempt.Time),
                                   Steps =>
                                     Prover_Steps'Max (Max_Success.Steps,
                                                       Attempt.Steps));
                           end if;

                           Total_Time := Total_Time + Attempt.Time;
                        end;
                     end if;
                  end Mapping_CB;
               begin
                  --  We use Map_JSON_Object here, because the prover name is
                  --  dynamic and potentially unknown to us, so we can't do a
                  --  lookup.
                  GNATCOLL.JSON.Map_JSON_Object (Val => Attempt_List,
                                                 CB  => Mapping_CB'Access);
               end;
            end if;

            --  Handle the "trivial_true" object (since GNAT_CE_2020.
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Transformations,
                                          Kind   => JSON_Object_Type)
            then
               declare
                  Transformation : constant JSON_Value
                    := Element.Get (Field => Field_Names.Transformations);
               begin
                  if
                    Transformation.Has_Field (Field => Field_Names.Trivial_True)
                  then
                     Attempts.Append (New_Item => Proof_Attempt.Trivial_True);
                     --  No timing updates needed here, as we assume 0.0 for
                     --  trivially true proofs.
                  end if;
               end;
            end if;

            --  If not empty, add the current check tree to our list.
            if not Attempts.Is_Empty then
               Attempts.Sort_By_Duration; --  FIXME: Potentially unstable.
               Checks_List.Append (New_Item => Attempts);
            end if;
         end;
      end loop;

      --  Sort checks list descending by duration.
      Checks_By_Duration.Sort (Container => Checks_List);

      declare
         PI_Node : Entity.Tree.Cursor;
      begin
         --  Allocate node for our object.
         Tree.Insert_Child
           (Parent   => Parent,
            Before   => Entity.Tree.No_Element,
            New_Item => Proof_Item_Sentinel'(Entity.T with null record),
            Position => PI_Node);

         --  Now insert the whole object into the tree.
         declare
            PA_Node : Entity.Tree.Cursor;
         begin
            for Check of Checks_List loop
               Tree.Insert_Child (Parent   => PI_Node,
                                  Before   => Entity.Tree.No_Element,
                                  New_Item =>
                                    Checks_Sentinel'
                                      (Entity.T with
                                       Has_Failed_Attempts => True,
                                       Is_Unproved         => True),
                                  Position => PA_Node);

               for Attempt of Check loop
                  Tree.Insert_Child (Parent   => PA_Node,
                                     Before   => Entity.Tree.No_Element,
                                     New_Item => Attempt);
               end loop;

               --  Replace the Checks_Sentinel with proper data.
               Tree.Replace_Element
                 (Position => PA_Node,
                  New_Item =>
                    Checks_Sentinel'
                      (Entity.T with
                       Has_Failed_Attempts => Check.Has_Failed_Attempts,
                       Is_Unproved         => Check.Is_Unproved));
            end loop;
         end;

         --  And finally replace the sentinel node with the full object.
         declare
            Has_Failed_Attempts : constant Boolean :=
              (for some Check of Checks_List => Check.Has_Failed_Attempts);
            Has_Unproved_Attempts : constant Boolean :=
              (for some Check of Checks_List => Check.Is_Unproved);
            Is_Unjustified : constant Boolean :=
              Has_Unproved_Attempts and then
              Suppressed_Msg = Justification (Null_Name);
         begin
            Tree.Replace_Element
              (Position => PI_Node,
               New_Item =>
                 T'(Entity_Location.Create (Object => Object) with
                      Suppressed            => Suppressed_Msg,
                      Rule                  =>
                        Rule_Name
                          (Subject_Name'(Object.Get
                                           (Field => Field_Names.Rule))),
                      Severity              =>
                        Severity_Name
                          (Subject_Name'(Object.Get
                                           (Field => Field_Names.Severity))),
                      Max_Success           =>
                        (if Has_Unproved_Attempts
                         then None
                         else Max_Success),
                      Max_Proof             => Max_Proof,
                      Total_Time            => Total_Time,
                      Id                    => Proof_Item_Ids.Next,
                      Has_Failed_Attempts   => Has_Failed_Attempts,
                      Has_Unproved_Attempts => Has_Unproved_Attempts,
                      Is_Unjustified        => Is_Unjustified));
         end;
      end;
   end Add_To_Tree;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding
   function Create (Object : in JSON_Value) return T is
     (raise Program_Error with
        "Create should not be called. Instead call Add_To_Tree.");

end SPAT.Proof_Item;
