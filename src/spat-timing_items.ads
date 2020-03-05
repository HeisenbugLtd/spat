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
private package SPAT.Timing_Items is

   --  Information obtained from the timing section of a .spark file.
   type T is
      record
         Proof : Duration; --  Total time the prover spent.
         Flow  : Duration; --  Total time of flow analysis.
      end record;

   None : constant T := T'(Proof => 0.0,
                           Flow  => 0.0);

end SPAT.Timing_Items;
