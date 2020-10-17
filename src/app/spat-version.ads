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
--  S.P.A.T. - Version information
--
------------------------------------------------------------------------------

with GNAT.Compiler_Version;

package SPAT.Version is

   package Implementation is

      package Compiler_Info is new GNAT.Compiler_Version;

   end Implementation;

   Number   : constant String := "1.3.0";
   Compiler : constant String := Implementation.Compiler_Info.Version;

end SPAT.Version;
