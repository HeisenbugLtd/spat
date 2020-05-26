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
with SPAT.Field_Names;
with SPAT.Preconditions;

private package SPAT.Timing_Items is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   function Has_Required_Fields (Object  : in JSON_Value;
                                 Version : in File_Version) return Boolean
   is
     ((case Version is
          when GNAT_CE_2019 =>
             Preconditions.Ensure_Field (Object => Object,
                                         Field  => Field_Names.Proof,
                                         Kind   => JSON_Float_Type),
          when GNAT_CE_2020 =>
             Preconditions.Ensure_Field (Object => Object,
                                         Field  => Field_Names.Run_VCs,
                                         Kind   => JSON_Float_Type) and then
             Preconditions.Ensure_Field (Object => Object,
                                         Field  => Field_Names.Register_VCs,
                                         Kind   => JSON_Float_Type) and then
             Preconditions.Ensure_Field (Object => Object,
                                         Field  => Field_Names.Schedule_VCs,
                                         Kind   => JSON_Float_Type)) and then
       Preconditions.Ensure_Field (Object => Object,
                                   Field  => Field_Names.Flow_Analysis,
                                   Kind   => JSON_Float_Type));

   --  Information obtained from the timing section of a .spark file.
   type T is
      record
         Proof : Duration; --  Total time the prover spent.
         Flow  : Duration; --  Total time of flow analysis.
      end record;

   function Create (Object  : in JSON_Value;
                    Version : in File_Version) return T with
     Pre => Has_Required_Fields (Object  => Object,
                                 Version => Version);

   None : constant T := T'(Proof => 0.0,
                           Flow  => 0.0);

end SPAT.Timing_Items;
