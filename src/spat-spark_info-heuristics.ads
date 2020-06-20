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
--  S.P.A.T. - Heuristic stuff. Tries to find optimal prover configuration.
--
------------------------------------------------------------------------------

package SPAT.Spark_Info.Heuristics is

   ---------------------------------------------------------------------------
   --  Experimental feature.
   --
   --  Tries to find an "optimal" prover configuration.
   --
   --  NOTE: As of now, this implementation is highly inefficient.
   --
   --        It uses a lot of lookups where a proper data structure would have
   --        been able to prevent that.
   --        I just found it more important to get a working prototype, than a
   --        blazingly fast one which doesn't.
   ---------------------------------------------------------------------------
   procedure Find_Optimum (Info : in T); --   Out parameter not clear yet.

end SPAT.Spark_Info.Heuristics;
