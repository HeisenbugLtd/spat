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
with SPAT;

procedure Main_SPAT_Test is
begin
  Ada.Command_Line.Set_Exit_Status (Code => Ada.Command_Line.Success);
end Main_SPAT_Test;
