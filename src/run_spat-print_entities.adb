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
--  S.P.A.T. - Main program - separate Print_Entities
--
------------------------------------------------------------------------------

with Ada.Text_IO;

with SPAT.Entity.Tree;
with SPAT.Log;
with SPAT.Proof_Attempt;
with SPAT.Proof_Item;
with SPAT.Strings;

separate (Run_SPAT)

------------------------------------------------------------------------------
--  Print_Entities
------------------------------------------------------------------------------
procedure Print_Entities (Info    : in SPAT.Spark_Info.T;
                          Sort_By : in SPAT.Spark_Info.Sorting_Criterion)
is
   Entities   : constant SPAT.Strings.List'Class :=
                  Info.List_All_Entities (Sort_By => Sort_By);

   use type Ada.Text_IO.Count;
   use type SPAT.Subject_Name;

   Second_Column : constant Ada.Text_IO.Count := Entities.Max_Length + 2;
   Mode          : constant SPAT.Command_Line.Report_Mode :=
     SPAT.Command_Line.Report.Get;
   use all type SPAT.Command_Line.Report_Mode;
begin
   for Entity of Entities loop
      if
        (case Mode is
            when None        => False,
            when All_Proofs  => True,
            when Failed      => Info.Has_Failed_Attempts (Entity => Entity),
            when Unproved    => Info.Has_Unproved_Attempts (Entity => Entity),
            when Unjustified =>
              Info.Has_Unproved_Attempts (Entity => Entity) and then
              Info.Has_Unjustified_Attempts (Entity => Entity))
      then
         SPAT.Log.Message (Message  => SPAT.To_String (Source => Entity),
                           New_Line => False);
         Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                              To   => Second_Column);
         SPAT.Log.Message
           (Message =>
              "=> " & Image (Value => Info.Max_Proof_Time (Entity => Entity)) &
              "/" & Image (Value => Info.Total_Proof_Time (Entity => Entity)));

         if SPAT.Command_Line.Details.Get then
            for PI_Position in Info.Proof_Tree (Entity => Entity) loop
               declare
                  The_Proof : SPAT.Proof_Item.T'Class renames
                    SPAT.Proof_Item.T'Class
                      (SPAT.Entity.Tree.Element (Position => PI_Position));
               begin
                  if
                    (case Mode is
                        when None        => False,
                        when All_Proofs  => True,
                        when Failed      => The_Proof.Has_Failed_Attempts,
                        when Unproved    => The_Proof.Has_Unproved_Attempts,
                        when Unjustified =>
                          The_Proof.Has_Unproved_Attempts and then
                          The_Proof.Is_Unjustified)
                  then
                     SPAT.Log.Message
                       (Message =>
                          "`-" & SPAT.To_String (The_Proof.Rule) & " " &
                          The_Proof.Image & " => " &
                          Image (The_Proof.Max_Time) & "/" &
                          Image (The_Proof.Total_Time));

                     for Check_Position in
                       Info.Iterate_Children (Entity   => Entity,
                                              Position => PI_Position)
                     loop
                        declare
                           The_Check : SPAT.Proof_Item.Checks_Sentinel'Class renames
                             SPAT.Proof_Item.Checks_Sentinel'Class
                               (SPAT.Entity.Tree.Element (Position => Check_Position));
                        begin
                           if
                             (case Mode is
                                 when None        => False,
                                 when All_Proofs  => True,
                                 when Failed      => The_Check.Has_Failed_Attempts,
                                 when Unproved |
                                      Unjustified => The_Check.Is_Unproved)
                           then
                              Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                                                   To   => 2);
                              SPAT.Log.Message (Message  => "`",
                                                New_Line => False);

                              for Attempt_Position in
                                Info.Iterate_Children (Entity   => Entity,
                                                       Position => Check_Position)
                              loop
                                 declare
                                    The_Attempt : SPAT.Proof_Attempt.T'Class renames
                                      SPAT.Proof_Attempt.T'Class
                                        (SPAT.Entity.Tree.Element
                                           (Position => Attempt_Position));
                                 begin
                                    Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                                                         To   => 3);
                                    SPAT.Log.Message
                                      (Message =>
                                         "-" & SPAT.To_String (The_Attempt.Prover) & ": " &
                                         Image (The_Attempt.Time) &
                                         " (" & SPAT.To_String (The_Attempt.Result) & ")");
                                 end;
                              end loop;
                           end if;
                        end;
                     end loop;

                     if The_Proof.Suppressed /= SPAT.Null_Name then
                        SPAT.Log.Message
                          (Message =>
                             "Justified with: """ &
                             SPAT.To_String (The_Proof.Suppressed) & """.");
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end if;
   end loop;
end Print_Entities;
