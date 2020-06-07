------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Proof_Attempt is

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : JSON_Value;
                    Prover : Subject_Name) return T
   is
      Time_Field : constant JSON_Value :=
                     Object.Get (Field => Field_Names.Time);
   begin
      return T'(Prover => Prover,
                Result => Object.Get (Field => Field_Names.Result),
                Time   =>
                  (case Time_Field.Kind is
                      when JSON_Float_Type =>
                        Duration (Time_Field.Get_Long_Float),
                      when JSON_Int_Type   =>
                        Duration (Long_Long_Integer'(Time_Field.Get)),
                      when others          =>
                         raise Program_Error
                           with
                             "Fatal: Impossible Kind """ &
                             Time_Field.Kind'Image & """ of JSON object!"));
   end Create;

end SPAT.Proof_Attempt;
