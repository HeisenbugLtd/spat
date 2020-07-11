------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Strings.Fixed;

package body SPAT.Timing_Item is

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   not overriding
   function Create (Object  : in JSON_Value;
                    Version : in File_Version) return T is
      Proof_Time : Duration;
   begin
      case Version is
         when GNAT_CE_2019 =>
            Proof_Time :=
              Duration (Object.Get_Long_Float (Field => Field_Names.Proof));
         when GNAT_CE_2020 =>
            declare
               --  Callback when mapping the timing object. If the name of the
               --  JSON value matches "gnatwhy3." we assume it's a timing value
               --  that should be added to the proof time.

               ---------------------------------------------------------------
               --  Add_Why3_Time
               ---------------------------------------------------------------
               procedure Add_Why3_Time (Name  : in UTF8_String;
                                        Value : in JSON_Value);

               ---------------------------------------------------------------
               --  Add_Why3_Time
               ---------------------------------------------------------------
               procedure Add_Why3_Time (Name  : in UTF8_String;
                                        Value : in JSON_Value) is
               begin
                  if
                    Ada.Strings.Fixed.Index
                      (Source  => Name,
                       Pattern => Field_Names.GNAT_Why3_Prefixed) = 1
                  then
                     Proof_Time := Proof_Time + Duration (Value.Get_Long_Float);
                  end if;
               end Add_Why3_Time;
            begin
               Proof_Time := 0.0;
               GNATCOLL.JSON.Map_JSON_Object (Val   => Object,
                                              CB    => Add_Why3_Time'Access);
            end;
      end case;

      return
        T'(Entity.T with
             Version => Version,
             Proof   => Proof_Time,
             Flow    =>
               Duration
                 (Object.Get_Long_Float (Field => Field_Names.Flow_Analysis)));
      --  TODO: Maybe accept integer values, too.
   end Create;

end SPAT.Timing_Item;
