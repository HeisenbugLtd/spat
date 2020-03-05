------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Entity_Lines is

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : in JSON_Value) return T is
     (T'(File => Object.Get (Field => Field_Names.File),
         Line => Object.Get (Field => Field_Names.Line)));

end SPAT.Entity_Lines;
