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
--  S.P.A.T. - Test program
--
------------------------------------------------------------------------------

with Ada.Command_Line;
with SPAT.Command_Line;
with SPAT.File_Ops;    pragma Unreferenced (SPAT.File_Ops);
with SPAT.Spark_Files; pragma Unreferenced (SPAT.File_Ops);
with SPAT.Spark_Info; pragma Unreferenced (SPAT.File_Info);

procedure Main_SPAT_Test is
begin
   Ada.Command_Line.Set_Exit_Status (Code => (if SPAT.Command_Line.Parser.Parse
                                              then Ada.Command_Line.Success
                                              else Ada.Command_Line.Failure));
end Main_SPAT_Test;
