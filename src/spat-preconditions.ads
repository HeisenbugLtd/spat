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

   type Accepted_Value_Types is array (JSON_Value_Type) of Boolean with
     Pack => True;

   Number_Kind : constant Accepted_Value_Types;

   ---------------------------------------------------------------------------
   --  Ensure_Field
   --
   --  Check that the given JSON object contains an object named Field with
   --  type of Kind.
   --  If the field is not found or the type is different from the expected
   --  one, a warning is issued, unless Is_Optional is True.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Field (Object      : in JSON_Value;
                          Field       : in UTF8_String;
                          Kind        : in JSON_Value_Type;
                          Is_Optional : in Boolean := False) return Boolean;

   ---------------------------------------------------------------------------
   --  Ensure_Field
   --
   --  Check that the given JSON object contains an object named Field with
   --  on of the types in Kind.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Field
     (Object        : in JSON_Value;
      Field         : in UTF8_String;
      Kinds_Allowed : in Accepted_Value_Types) return Boolean;

   ---------------------------------------------------------------------------
   --  Ensure_Rule_Severity
   --
   --  Checks that the given JSON object contains a rule and a severity object.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Rule_Severity (Object : in JSON_Value) return Boolean;

private

   --  Make JSON type enumeration literals directly visible.
   use all type JSON_Value_Type;

   Number_Kind : constant Accepted_Value_Types :=
                   Accepted_Value_Types'(JSON_Int_Type |
                                         JSON_Float_Type => True,
                                         others          => False);

end SPAT.Preconditions;
