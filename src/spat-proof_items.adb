------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Proof_Items is

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   function "<" (Left  : in Proof_Attempts.Vector;
                 Right : in Proof_Attempts.Vector) return Boolean
   is
      Left_Time  : Duration := 0.0;
      Right_Time : Duration := 0.0;
      --  FIXME: Proof_Attempts should have a field storing the max/accumulated
      --         time directly, so we don't need to recalculate it each time.
   begin
      for A of Left loop
         Left_Time := Left_Time + A.Time;
      end loop;

      for A of Right loop
         Right_Time := Right_Time + A.Time;
      end loop;

      return Left_Time > Right_Time;
   end "<";

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding function Create (Object : in JSON_Value) return T
   is
      Max_Time    : Duration := 0.0;
      Total_Time  : Duration := 0.0;
      Checks_List : Checks_Tree.Vector := Checks_Tree.Empty_Vector;
      Check_Tree  : constant JSON_Array :=
                      Object.Get (Field => Field_Names.Check_Tree);
   begin
      --  Walk along the check_tree array to find all proof attempts and their
      --  respective times.
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Check_Tree) loop
         declare
            Attempts : Proof_Attempts.Vector := Proof_Attempts.Empty_Vector;
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

                  procedure Mapping_CB (Name  : in UTF8_String;
                                        Value : in JSON_Value);

                  procedure Mapping_CB (Name  : in UTF8_String;
                                        Value : in JSON_Value) is
                  begin
                     if
                       Proof_Attempts.Has_Required_Fields (Object => Value)
                     then
                        declare
                           Attempt : constant Proof_Attempts.T :=
                                       Proof_Attempts.Create
                                         (Prover => To_Name (Name),
                                          Object => Value);
                        begin
                           Attempts.Append (New_Item => Attempt);

                           Max_Time   := Duration'Max (Max_Time, Attempt.Time);
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
                     Attempts.Append (New_Item => Proof_Attempts.Trivial_True);
                     --  No timing updates needed here, as we assume 0.0 for
                     --  trivially true proofs.
                  end if;
               end;
            end if;

            --  If not empty, add the current check tree to our list.
            if not Attempts.Is_Empty then
               Attempts.Sort_By_Duration;
               Checks_List.Append (New_Item => Attempts);
            end if;
         end;
      end loop;

      --  Sort checks list ascending by duration.
      Checks_By_Duration.Sort (Container => Checks_List);

      return
        (Entity_Locations.Create (Object => Object) with
         Suppressed => (if Object.Has_Field (Field => Field_Names.Suppressed)
                        then Object.Get (Field => Field_Names.Suppressed)
                        else Null_Name), --  FIXME: Missing type check.
         Rule       => Object.Get (Field => Field_Names.Rule),
         Severity   => Object.Get (Field => Field_Names.Severity),
         Check_Tree => Checks_List,
         Max_Time   => Max_Time,
         Total_Time => Total_Time);
   end Create;

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean is
   begin
      return (for some C of This.Check_Tree => C.Has_Failed_Attempts);
   end Has_Failed_Attempts;

   ---------------------------------------------------------------------------
   --  Has_Unproved_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Unproved_Attempts (This : in T) return Boolean is
   begin
      return (for some C of This.Check_Tree => C.Is_Unproved);
   end Has_Unproved_Attempts;

end SPAT.Proof_Items;
