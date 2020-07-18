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

with GNATCOLL.Opt_Parse.Extension;
with SPAT.Spark_Info;

package SPAT.Command_Line is

   --  Report filter mode.
   --  Show all, show failed only, show unproved, show unjustified.
   --  Later ones imply earlier ones.
   type Report_Mode is (All_Proofs, Failed, Unproved, Unjustified, None);

   --  Detail levels.
   type Detail_Level is (None, Level_1, Full);

   Parser : GNATCOLL.Opt_Parse.Argument_Parser :=
     GNATCOLL.Opt_Parse.Create_Argument_Parser
       (Help         => "Parses .spark files and outputs information about them.",
        Command_Name => "run_spat");

   --  Before using the below functions you should have called Parser.Parse and
   --  evaluated its return status.

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert (Value : in String) return Detail_Level;

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert (Value : in String) return Duration;

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
   function Convert (Value : in String) return Detail_Level is
     (if    Value = "1" then Level_1
      elsif Value in "" | "2" | "f" then Full
      else  (raise GNATCOLL.Opt_Parse.Opt_Parse_Error with
                 "unknown parameter """ & Value & """"));

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert (Value : in String) return SPAT.Spark_Info.Sorting_Criterion
   is
     (if    Value = "a" then SPAT.Spark_Info.Name
      elsif Value = "s" then SPAT.Spark_Info.Max_Success_Time
      elsif Value = "t" then SPAT.Spark_Info.Max_Time
      elsif Value = "p" then SPAT.Spark_Info.Max_Steps
      elsif Value = "q" then SPAT.Spark_Info.Max_Success_Steps
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
     --  Any of the report modes.
     GNATCOLL.Opt_Parse.Parse_Option
       (Parser      => Parser,
        Short       => "-r",
        Long        => "--report-mode",
        Help        =>
           "Output reporting mode (REPORT-MODE: a = all, f = failed, " &
           "u = unproved, j = unjustified [implies unproved])",
        Arg_Type    => Report_Mode,
        Convert     => Convert,
        Default_Val => None);

   package Suggest is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser  => Parser,
        Short   => "-g",
        Long    => "--suggest",
        Help    => "Show suggestion for an optimal prover configuration");

   package Entity_Filter is new
     GNATCOLL.Opt_Parse.Parse_Option_List
       (Parser     => Parser,
        Short      => "-e",
        Long       => "--entity",
        Help       =>
           "Filter output by ENTITY (regular expression), this option can " &
           " be specified multiple times",
        Accumulate => True,
        Arg_Type   => Subject_Name,
        Convert    => GNATCOLL.Opt_Parse.Convert);

   --  Valid for summary and report mode.
   package Sort_By is new
     GNATCOLL.Opt_Parse.Parse_Option
       (Parser      => Parser,
        Short       => "-c",
        Long        => "--sort-by",
        Help        =>
           "Sorting criterion (SORT-BY: a = alphabetical, " &
           "s = by minimum time for successful proof, " &
           "t = by maximum proof time, " &
           "p = by minimum steps for successful proof, " &
           "q = by maximum steps)",
        Arg_Type    => SPAT.Spark_Info.Sorting_Criterion,
        Convert     => Convert,
        Default_Val => SPAT.Spark_Info.None);

   package Cut_Off is new
     GNATCOLL.Opt_Parse.Parse_Option
       (Parser      => Parser,
        Short       => "-p",
        Long        => "--cut-off",
        Help        =>
           "Cut off point, do not show entities with proof times less than " &
           "that (CUT-OFF: <numeral>[s|ms])",
        Arg_Type    => Duration,
        Convert     => Convert,
        Default_Val => 0.0);

   package Details is new
     GNATCOLL.Opt_Parse.Extension.Parse_Option_With_Default
       (Parser      => Parser,
        Short       => "-d",
        Long        => "--details",
        Help        =>
           "Show details for entities (report mode) (DETAILS: [1|2|f] for " &
           "level 1, 2 and full details. Please note that 2 and f are " &
           "currently equivalent.)",
        Arg_Type    => Detail_Level,
        Convert     => Convert,
        Default_Val => None);

   package Version is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser  => Parser,
        Short   => "-V",
        Long    => "--version",
        Help    => "Show version information and exit");

   package Raw_Mode is new
     GNATCOLL.Opt_Parse.Parse_Flag
       (Parser  => Parser,
        Short   => "-R",
        Long    => "--raw",
        Help    => "Output timings in raw format (for script use)");

end SPAT.Command_Line;
