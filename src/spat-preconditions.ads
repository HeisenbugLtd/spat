------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

------------------------------------------------------------------------------
--
--  SPARK Proof Analysis Tool
--
--  S.P.A.T. - Collection of checks that should succeed before creating
--             objects from the JSON data.
--
------------------------------------------------------------------------------

package SPAT.Preconditions is

   ---------------------------------------------------------------------------
   --  Ensure_Field
   --
   --  Check that the given JSON object contains an object named Field with
   --  type of Kind.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Field (Object : in JSON_Value;
                          Field  : in UTF8_String;
                          Kind   : in JSON_Value_Type) return Boolean;

   ---------------------------------------------------------------------------
   --  Ensure_Rule_Severity
   --
   --  Checks that the given JSON object contains a rule and a severity object.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Rule_Severity (Object : in JSON_Value) return Boolean;

end SPAT.Preconditions;
