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
   --  Create
   ---------------------------------------------------------------------------
   overriding function Create (Object : in JSON_Value) return T is
     (Entity_Locations.Create (Object => Object) with
        Rule     => Object.Get (Field => Field_Names.Rule),
        Severity => Object.Get (Field => Field_Names.Severity));

end SPAT.Proof_Items;
