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
--  S.P.A.T. - Heuristic stuff. Tries to find optimal prover configuration.
--
------------------------------------------------------------------------------

with SPAT.GPR_Support;
with Ada.Containers.Vectors;

package SPAT.Spark_Info.Heuristics is

   type Workloads is
      record
         --  The name of the prover.
         Success_Time : Duration;
         --  accumulated time of successful attempts
         Failed_Time  : Duration;
         --  accumulated time of failed attempts
         Max_Success  : Time_And_Steps;
         --  maximum time/steps for a successful attempt
      end record;

   type Prover_Data is
      record
         Name     : Prover_Name;
         Workload : Workloads;
      end record;

   package Prover_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Prover_Data);

   type File_Data is
      record
         Name    : Source_File_Name;
         Provers : Prover_Vectors.Vector;
      end record;

   package File_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => File_Data);

   ---------------------------------------------------------------------------
   --  Find_Optimum
   --
   --  This is a highly experimental feature.
   --
   --  Tries to find an "optimal" prover configuration.
   ---------------------------------------------------------------------------
   function Find_Optimum
     (Info     : in T;
      File_Map : in GPR_Support.SPARK_Source_Maps.Map)
      return File_Vectors.Vector;

end SPAT.Spark_Info.Heuristics;
