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

with Ada.Containers.Vectors;

package SPAT.Spark_Info.Heuristics is

   type Times is
      record
         --  The name of the prover.
         Success     : Duration;
         --  accumulated time of successful attempts
         Failed      : Duration;
         --  accumulated time of failed attempts
         Max_Success : Duration;
         --  maximum time for a successful attempt
         Max_Steps   : Prover_Steps;
         --  maximum number of steps for a successful proof
      end record;

   type Prover_Data is
      record
         Name : Subject_Name;
         Time : Times;
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
   function Find_Optimum (Info : in T) return File_Vectors.Vector;

end SPAT.Spark_Info.Heuristics;
