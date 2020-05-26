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
--  S.P.A.T. - Names of recognized fields in JSON data.
--
------------------------------------------------------------------------------
package SPAT.Field_Names is

   Assumptions    : constant UTF8_String := "assumptions";
   Check_Tree     : constant UTF8_String := "check_tree";
   Column         : constant UTF8_String := "col";
   Entity         : constant UTF8_String := "entity";
   File           : constant UTF8_String := "file";
   Flow           : constant UTF8_String := "flow";
   Flow_Analysis  : constant UTF8_String := "flow analysis";
   Line           : constant UTF8_String := "line";
   Name           : constant UTF8_String := "name";
   Proof          : constant UTF8_String := "proof";
   Proof_Attempts : constant UTF8_String := "proof_attempts";
   Result         : constant UTF8_String := "result";
   Rule           : constant UTF8_String := "rule";
   Severity       : constant UTF8_String := "severity";
   Sloc           : constant UTF8_String := "sloc";
   Spark          : constant UTF8_String := "spark";
   Steps          : constant UTF8_String := "steps";
   Time           : constant UTF8_String := "time";
   Timings        : constant UTF8_String := "timings";

   --  GNAT_CE_2019
   Translation_Of_Compilation_Unit : constant UTF8_String :=
     "translation of compilation unit";

   --  GNAT_CE_2020
   Register_VCs   : constant UTF8_String := "gnatwhy3.register_vcs";
   Run_VCs        : constant UTF8_String := "gnatwhy3.run_vcs";
   Schedule_VCs   : constant UTF8_String := "gnatwhy3.schedule_vcs";

end SPAT.Field_Names;
