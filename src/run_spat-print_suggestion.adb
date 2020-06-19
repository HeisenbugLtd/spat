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
with Ada.Text_IO;

with SPAT.Strings;

separate (Run_SPAT)

------------------------------------------------------------------------------
--  Print_Suggestion
------------------------------------------------------------------------------
procedure Print_Suggestion (Info : in SPAT.Spark_Info.T)
is
begin
   raise Program_Error with "Not implemented yet.";
end Print_Suggestion;
