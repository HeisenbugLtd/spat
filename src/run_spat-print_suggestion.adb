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
--  S.P.A.T. - Main program - separate Print_Suggestion
--
------------------------------------------------------------------------------

with SPAT.Log;

------------------------------------------------------------------------------
--  NOTE: As of now, this implementation is highly inefficient.
--
--        It uses a lot of lookups where a proper data structure would have
--        been able to prevent that.
--        I just found it more important to get a working prototype, than a
--        blazingly fast one which doesn't.
------------------------------------------------------------------------------
with SPAT.Strings;

separate (Run_SPAT)

------------------------------------------------------------------------------
--  Print_Suggestion
------------------------------------------------------------------------------
procedure Print_Suggestion (Info : in SPAT.Spark_Info.T) is
begin
   SPAT.Log.Message (Message => "Not implemented yet.");
   Info.ZZZ_Find_Optimum;
end Print_Suggestion;
