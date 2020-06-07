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

with SPAT.Log;
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
   Failed_Only   : constant Boolean := SPAT.Command_Line.Failed_Only.Get;
   Unproved_Only : constant Boolean := SPAT.Command_Line.Unproved_Only.Get;
   Report_All    : constant Boolean := not (Failed_Only or Unproved_Only);

begin
   for Entity of Entities loop
      if
        Report_All                                    or else
        (Failed_Only and then
         Info.Has_Failed_Attempts (Entity => Entity)) or else
        (Unproved_Only and then
         Info.Has_Unproved_Attempts (Entity => Entity))
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
            for P of Info.Proof_List (Entity => Entity) loop
               if
                 Report_All                                   or else
                 (Failed_Only and then P.Has_Failed_Attempts) or else
                 (Unproved_Only and then P.Has_Unproved_Attempts)
               then
                  SPAT.Log.Message
                    (Message =>
                       "`-" & SPAT.To_String (P.Rule) & " " & P.Image &
                       " => " & Image (P.Max_Time) & "/" &
                       Image (P.Total_Time));

                  for Check of P.Check_Tree loop
                     if
                       Report_All                                       or else
                       (Failed_Only and then Check.Has_Failed_Attempts) or else
                       (Unproved_Only and then Check.Is_Unproved)
                     then
                        Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                                             To   => 2);
                        SPAT.Log.Message (Message  => "`",
                                          New_Line => False);

                        for A of Check loop
                           Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                                                To   => 3);
                           SPAT.Log.Message
                             (Message =>
                                "-" & SPAT.To_String (A.Prover) & ": " &
                                Image (A.Time) &
                                " (" & SPAT.To_String (A.Result) & ")");
                        end loop;
                     end if;
                  end loop;

                  if P.Suppressed /= SPAT.Null_Name then
                     SPAT.Log.Message
                       (Message =>
                          "Justified with: """ & SPAT.To_String (P.Suppressed) &
                          """.");
                  end if;
               end if;
            end loop;
         end if;
      end if;
   end loop;
end Print_Entities;
