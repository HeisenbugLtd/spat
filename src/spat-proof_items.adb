------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Proof_Items is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding function Create (Object : in JSON_Value) return T
   is
      Max_Time : Duration := 0.0;
   begin
      --  Walk along the check_tree array to find all proof attempts and their
      --  respective times.
      if
        Preconditions.Ensure_Field (Object => Object,
                                    Field  => Field_Names.Check_Tree,
                                    Kind   => JSON_Array_Type)
      then
         declare
            Check_Tree : constant JSON_Array :=
                           Object.Get (Field => Field_Names.Check_Tree);
         begin
            for I in 1 .. GNATCOLL.JSON.Length (Arr => Check_Tree) loop
               declare
                  Element : constant JSON_Value :=
                              GNATCOLL.JSON.Get (Arr   => Check_Tree,
                                                 Index => I);
               begin
                  if
                    Preconditions.Ensure_Field
                      (Object => Element,
                       Field  => Field_Names.Proof_Attempts,
                       Kind   => JSON_Object_Type)
                  then
                     declare
                        Proof_Attempts : constant JSON_Value
                          := Element.Get (Field => Field_Names.Proof_Attempts);

                        procedure Mapping_CB (Name  : in UTF8_String;
                                              Value : in JSON_Value);

                        procedure Mapping_CB (Name  : in UTF8_String;
                                              Value : in JSON_Value)
                        is
                           pragma Unreferenced (Name);
                        begin
                           if
                             Preconditions.Ensure_Field
                               (Object => Value,
                                Field  => Field_Names.Time,
                                Kind   => JSON_Float_Type)
                           then
                              Max_Time :=
                                Duration'Max
                                  (Max_Time,
                                   Duration
                                     (Float'(Value.Get (Field => Field_Names.Time))));
                           end if;
                        end Mapping_CB;
                     begin
                        GNATCOLL.JSON.Map_JSON_Object (Val => Proof_Attempts,
                                                       CB  => Mapping_CB'Access);
                     end;
                  end if;
               end;
            end loop;
         end;
      end if;

      return
        (Entity_Locations.Create (Object => Object) with
           Rule     => Object.Get (Field => Field_Names.Rule),
           Severity => Object.Get (Field => Field_Names.Severity),
           Max_Time => Max_Time);
   end Create;

end SPAT.Proof_Items;
