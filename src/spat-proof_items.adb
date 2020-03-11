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
                  GNATCOLL.JSON.Map_JSON_Object (Val => Attempt_List,
                                                 CB  => Mapping_CB'Access);
                  Proof_Attempts.By_Duration.Sort (Container => Attempts);
               end;

               --  Add the current check tree to our list.
               Checks_List.Append (New_Item => Attempts);
            end if;
         end;
      end loop;

      --  Sort checks list ascending by duration.
      Checks_By_Duration.Sort (Container => Checks_List);

      return
        (Entity_Locations.Create (Object => Object) with
         Rule       => Object.Get (Field => Field_Names.Rule),
         Severity   => Object.Get (Field => Field_Names.Severity),
         Check_Tree => Checks_List,
         Max_Time   => Max_Time,
         Total_Time => Total_Time);
   end Create;

end SPAT.Proof_Items;
