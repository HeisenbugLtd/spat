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
--  S.P.A.T. - Command line parser
--
------------------------------------------------------------------------------

with Ada.Strings.Unbounded;
with GNATCOLL.Opt_Parse;

package SPAT.Command_Line is

   Parser : GNATCOLL.Opt_Parse.Argument_Parser :=
     GNATCOLL.Opt_Parse.Create_Argument_Parser
       (Help         => "Parses .spark files and outputs information about them.",
        Command_Name => "run_spat");

   --  Before using the below functions you should have called Parser.Parse and
   --  evaluated its return status.

   package Summary is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser => Parser,
                                    Short  => "-s",
                                    Long   => "--summary",
                                    Help   => "Print summary only");

   package Verbose is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser => Parser,
                                    Short  => "-v",
                                    Long   => "--verbose",
                                    Help   => "Verbose (tracing) output");

   package Directories is new
     GNATCOLL.Opt_Parse.Parse_Positional_Arg_List
       (Parser      => Parser,
        Name        => "directory",
        Help        => "directory to look for .spark files in",
        Allow_Empty => False,
        Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
        Convert     => Ada.Strings.Unbounded.To_Unbounded_String);

end SPAT.Command_Line;
