------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Proof_Attempts is

   function Create (Object : JSON_Value;
                    Prover : Subject_Name) return T is
   begin
      return T'(Prover => Prover,
                Result => Object.Get (Field => Field_Names.Result),
                Time   => Duration (Float'(Object.Get (Field => Field_Names.Time))));
   end Create;

end SPAT.Proof_Attempts;
