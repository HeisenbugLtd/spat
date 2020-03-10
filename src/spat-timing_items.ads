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
--  S.P.A.T. - Object representing the "timing" JSON object.
--
------------------------------------------------------------------------------
with SPAT.Preconditions;

private package SPAT.Timing_Items is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   function Has_Required_Fields (Object : in JSON_Value) return Boolean is
      (Preconditions.Ensure_Field (Object => Object,
                                   Field  => Field_Names.Proof,
                                   Kind   => JSON_Float_Type) and then
       Preconditions.Ensure_Field (Object => Object,
                                   Field  => Field_Names.Flow_Analysis,
                                   Kind   => JSON_Float_Type));

   --  Information obtained from the timing section of a .spark file.
   type T is
      record
         Proof : Duration; --  Total time the prover spent.
         Flow  : Duration; --  Total time of flow analysis.
      end record;

   function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   None : constant T := T'(Proof => 0.0,
                           Flow  => 0.0);

end SPAT.Timing_Items;
