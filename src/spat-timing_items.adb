------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Timing_Items is

   function Create (Object : in JSON_Value) return T is
   begin
      return
        T'(Proof =>
             Duration (Float'(Object.Get (Field => Field_Names.Proof))),
           Flow  =>
             Duration (Float'(Object.Get (Field => Field_Names.Flow_Analysis))));
   end Create;

end SPAT.Timing_Items;
