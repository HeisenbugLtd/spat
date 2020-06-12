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

   --  Report filter mode.
   --  Show all, show failed only, show unproved, show unjustified.
   --  Later ones imply earlier ones.
   type Report_Mode is (All_Proofs, Failed, Unproved, Unjustified, None);

   Parser : GNATCOLL.Opt_Parse.Argument_Parser :=
     GNATCOLL.Opt_Parse.Create_Argument_Parser
       (Help         => "Parses .spark files and outputs information about them.",
        Command_Name => "run_spat");

   --  Before using the below functions you should have called Parser.Parse and
   --  evaluated its return status.

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert
     (Value : in String) return SPAT.Spark_Info.Sorting_Criterion;

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert (Value : in String) return Report_Mode;

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert (Value : in String) return SPAT.Spark_Info.Sorting_Criterion
   is
     (if    Value = "a" then SPAT.Spark_Info.Name
      elsif Value = "t" then SPAT.Spark_Info.Time
      else  (raise GNATCOLL.Opt_Parse.Opt_Parse_Error with
                 "unknown parameter """ & Value & """"));

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert (Value : in String) return Report_Mode is
     (if    Value in "all"         | "a" then All_Proofs
      elsif Value in "failed"      | "f" then Failed
      elsif Value in "unproved"    | "u" then Unproved
      elsif Value in "unjustified" | "j" then Unjustified
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

   --  Report mode.
   package Report is new
     --  Any of the list modes.
     GNATCOLL.Opt_Parse.Parse_Option
       (Parser      => Parser,
        Short       => "-r",
        Long        => "--report-mode",
        Help        => "Report output (REPORT-MODE: a = all, f = failed, u = unproved, j = unjustified)",
        Arg_Type    => Report_Mode,
        Convert     => Convert,
        Default_Val => None);

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

   package Details is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser   => Parser,
                                    Short    => "-d",
                                    Long     => "--details",
                                    Help     => "Show details for entities (list mode)");

   package Version is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser  => Parser,
        Short   => "-V",
        Long    => "--version",
        Help    => "Show version information and exit");

end SPAT.Command_Line;
