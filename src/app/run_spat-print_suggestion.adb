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
--  S.P.A.T. - Main program - separate Print_Suggestion
--
------------------------------------------------------------------------------

with Ada.Strings.Fixed;
with SPAT.Log;
with SPAT.Spark_Info.Heuristics;
with SPAT.Strings;

separate (Run_SPAT)

------------------------------------------------------------------------------
--  Print_Suggestion
------------------------------------------------------------------------------
procedure Print_Suggestion
  (Info     : in SPAT.Spark_Info.T;
   File_Map : in SPAT.GPR_Support.SPARK_Source_Maps.Map)
is
   Indent  : constant String := "   ";
   Results : SPAT.Spark_Info.Heuristics.File_Vectors.Vector;
   use type SPAT.Spark_Info.Heuristics.Prover_Vectors.Cursor;
begin
   SPAT.Log.Warning
     (Message => "You requested a suggested prover configuration.");
   SPAT.Log.Warning (Message => "This feature is highly experimental.");
   SPAT.Log.Warning (Message => "Please consult the documentation.");

   Results := SPAT.Spark_Info.Heuristics.Find_Optimum (Info     => Info,
                                                       File_Map => File_Map);

   SPAT.Log.Message (Message => "");
   SPAT.Log.Message (Message => "package Prove is");

   For_Each_File :
   for File of Results loop
      Find_Minima :
      declare
         Min_Steps   : SPAT.Prover_Steps := 0;
         Min_Timeout : Duration          := 0.0;
      begin
         if not File.Provers.Is_Empty then
            SPAT.Log.Message
              (Message =>
                 Indent & "for Proof_Switches (""" &
                 SPAT.To_String (Source => File.Name) & """) use (""",
               New_Line => False);

            SPAT.Log.Message (Message  => "--prover=",
                              New_Line => False);

            For_Each_Prover :
            for Prover in File.Provers.Iterate loop
               Min_Steps :=
                 SPAT.Prover_Steps'Max
                   (File.Provers (Prover).Workload.Max_Success.Steps,
                    Min_Steps);
               Min_Timeout :=
                 Duration'Max (File.Provers (Prover).Workload.Max_Success.Time,
                               Min_Timeout);

               SPAT.Log.Message
                 (Message  => SPAT.To_String (File.Provers (Prover).Name),
                  New_Line => False);

               if Prover /= File.Provers.Last then
                  SPAT.Log.Message (Message  => ",",
                                    New_Line => False);
               end if;
            end loop For_Each_Prover;

            SPAT.Log.Message (Message  => """, ",
                              New_Line => False);

            SPAT.Log.Message
              (Message  =>
                 """--steps=" &
                 Ada.Strings.Fixed.Trim (Source => Min_Steps'Image,
                                         Side   => Ada.Strings.Both) &
                 """",
               New_Line => False);

            SPAT.Log.Message (Message  => ", ",
                              New_Line => False);

            SPAT.Log.Message
              (Message  =>
                 """--timeout=" &
                 Ada.Strings.Fixed.Trim
                   (Source => Integer'Image (Integer (Min_Timeout + 0.5)),
                    Side   => Ada.Strings.Both) &
                 """",
               New_Line => False);

            SPAT.Log.Message (Message => ");");
         else
            SPAT.Log.Message
              (Message =>
                 Indent & "--  """ & SPAT.To_String (Source => File.Name) &
                 """ --  no data found.");
         end if;
      end Find_Minima;
   end loop For_Each_File;

   SPAT.Log.Message (Message => "end Prove;");
   SPAT.Log.Message (Message => "");
end Print_Suggestion;
