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

with GNATCOLL.Opt_Parse;
with SPAT.Spark_Info;

package SPAT.Command_Line is

   Parser : GNATCOLL.Opt_Parse.Argument_Parser :=
     GNATCOLL.Opt_Parse.Create_Argument_Parser
       (Help         => "Parses .spark files and outputs information about them.",
        Command_Name => "run_spat");

   --  Before using the below functions you should have called Parser.Parse and
   --  evaluated its return status.

   function Convert
     (Value : in String) return SPAT.Spark_Info.Sorting_Criterion;

   function Convert (Value : in String) return SPAT.Spark_Info.Sorting_Criterion
   is
     (if    Value in "=a" | "a" then SPAT.Spark_Info.Name
      elsif Value in "=t" | "t" then SPAT.Spark_Info.Time
      else  (raise GNATCOLL.Opt_Parse.Opt_Parse_Error with
                 "unknown parameter """ & Value & """"));

   --  Command line options (in order of importance/mode).

   --  Project file (mandatory).
   package Project is new
     GNATCOLL.Opt_Parse.Parse_Option
       (Parser      => Parser,
        Short       => "-P",
        Long        => "--project",
        Help        => "PROJECT = GNAT project file (.gpr) (mandatory!)",
        Arg_Type    => SPAT.Subject_Name,
        Default_Val => SPAT.Null_Name,
        Convert     => SPAT.To_Name);

   --  Summary mode.
   package Summary is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser => Parser,
                                    Short  => "-s",
                                    Long   => "--summary",
                                    Help   => "List summary (per file)");

   --  List mode.
   package List is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser => Parser,
                                    Short  => "-l",
                                    Long   => "--list",
                                    Help   => "List entities");

   --  Valid for summary and list mode.
   package Sort_By is new
     GNATCOLL.Opt_Parse.Parse_Option
       (Parser      => Parser,
        Short       => "-c",
        Long        => "--sort-by",
        Help        => "Sort output (SORT-BY: a = alphabetical, t = by time)",
        Arg_Type    => SPAT.Spark_Info.Sorting_Criterion,
        Convert     => Convert,
        Default_Val => SPAT.Spark_Info.None);

   --  Show failed prove attempts (only in list mode).
   package Failed_Only is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser   => Parser,
        Short    => "-f",
        Long     => "--failed",
        Help     => "Show failed attempts only (list mode)");

   --  Show unproven VCs (only in list mode).
   package Unproved_Only is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser   => Parser,
        Short    => "-u",
        Long     => "--unproved",
        Help     => "Show unproved VCs only (list mode)");

   package Details is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser   => Parser,
                                    Short    => "-d",
                                    Long     => "--details",
                                    Help     => "Show details for entities (list mode)");

   --  Verbose option (debug output).
   package Verbose is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser => Parser,
                                    Short  => "-v",
                                    Long   => "--verbose",
                                    Help   => "Verbose (tracing) output");

end SPAT.Command_Line;
