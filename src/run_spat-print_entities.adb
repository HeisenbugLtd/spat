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
procedure Print_Entities (Info          : in SPAT.Spark_Info.T;
                          Sort_By       : in SPAT.Spark_Info.Sorting_Criterion;
                          Cut_Off       : in Duration;
                          Entity_Filter : in Reg_Exp_List.Vector)
is
   Entities : constant SPAT.Strings.Entity_Names :=
     Info.List_All_Entities (Sort_By => Sort_By);

   use type Ada.Text_IO.Count;
   use type SPAT.Subject_Name;

   Second_Column : constant Ada.Text_IO.Count := Entities.Max_Length + 2;
   Mode          : constant SPAT.Command_Line.Report_Mode :=
     SPAT.Command_Line.Report.Get;
   Detail_Level  : constant SPAT.Command_Line.Detail_Level :=
     SPAT.Command_Line.Details.Get;

   type Filtered is
      record
         Omitted_Entities : Natural;
         Omitted_VCs      : Natural;
      end record;

   function Has_Omitted (F : in Filtered) return Boolean is
     (F.Omitted_Entities /= 0 or F.Omitted_VCs /= 0);

   function Image (F : in Filtered) return String is
      (F.Omitted_Entities'Image & " entities, and" &
       F.Omitted_VCs'Image & " VCs within the above results");

   None_Filtered : constant Filtered := Filtered'(Omitted_Entities => 0,
                                                  Omitted_VCs      => 0);

   Cut_Off_Filter : Filtered := None_Filtered;

   use all type SPAT.Command_Line.Detail_Level;
   use all type SPAT.Command_Line.Report_Mode;

   ---------------------------------------------------------------------------
   --  Should_Show_Entity
   --
   --  Applies given filters (cut off point, report mode, entity filter
   --  regexp) and returns True if the entity matches this filter and should
   --  be shown.
   ---------------------------------------------------------------------------
   function Should_Show_Entity (Entity : in SPAT.Entity_Name) return Boolean;

   ---------------------------------------------------------------------------
   --  Should_Show_Proof
   --
   --  Applies given filters (cut off point, report mode) and returns True if
   --  the given Proof matches the filters and should be shown.
   ---------------------------------------------------------------------------
   function Should_Show_Proof
     (The_Proof : in SPAT.Proof_Item.T'Class) return Boolean;

   ---------------------------------------------------------------------------
   --  Should_Show_Check
   --
   --  Applies filters (report mode) and returns True if the given Check
   --  matches the filters and should be shown.
   ---------------------------------------------------------------------------
   function Should_Show_Check
     (The_Check : in SPAT.Proof_Item.Checks_Sentinel'Class) return Boolean;

   ---------------------------------------------------------------------------
   --  Should_Show_Check
   ---------------------------------------------------------------------------
   function Should_Show_Check
     (The_Check : in SPAT.Proof_Item.Checks_Sentinel'Class) return Boolean is
     (case Mode is
         when None        => raise Program_Error,
         when All_Proofs  => True,
         when Failed      => The_Check.Has_Failed_Attempts,
         when Unproved |
              Unjustified => The_Check.Is_Unproved);

   ---------------------------------------------------------------------------
   --  Should_Show_Entity
   ---------------------------------------------------------------------------
   function Should_Show_Entity (Entity : in SPAT.Entity_Name) return Boolean is
   begin
      if Info.Max_Proof_Time (Entity => Entity) < Cut_Off then
         Cut_Off_Filter.Omitted_Entities := Cut_Off_Filter.Omitted_Entities + 1;
         return False;
      end if;

      --  Check report mode
      case Mode is
         when None        =>
            return False; --  Show nothing.
         when All_Proofs  =>
            null; -- Fall through.
         when Failed      =>
            if not Info.Has_Failed_Attempts (Entity => Entity) then
               --  No failed attempts, don't show.
               return False;
            end if;
         when Unproved    =>
            if not Info.Has_Unproved_Attempts (Entity => Entity) then
               --  No unproved attempts, don't show.
               return False;
            end if;
         when Unjustified =>
            if
              not Info.Has_Unproved_Attempts (Entity => Entity) or else
              not Info.Has_Unjustified_Attempts (Entity => Entity)
            then
               --  Either no unproved VCs, or all unproved ones are justified.
               return False;
            end if;
      end case;

      --  Finally check the filter expression.
      return (Entity_Filter.Is_Empty or else
                (for some Expression of Entity_Filter =>
                   GNAT.Regexp.Match (S => SPAT.To_String (Entity),
                                      R => Expression)));
   end Should_Show_Entity;

   ---------------------------------------------------------------------------
   --  Should_Show_Proof
   ---------------------------------------------------------------------------
   function Should_Show_Proof
     (The_Proof : in SPAT.Proof_Item.T'Class) return Boolean is
   begin
      if The_Proof.Max_Time < Cut_Off then
         --  Below cut off point, don't show.
         Cut_Off_Filter.Omitted_VCs := Cut_Off_Filter.Omitted_VCs + 1;
         return False;
      end if;

      return (case Mode is
                 when None        => raise Program_Error,
                 when All_Proofs  => True,
                 when Failed      => The_Proof.Has_Failed_Attempts,
                 when Unproved    => The_Proof.Has_Unproved_Attempts,
                 when Unjustified => (The_Proof.Has_Unproved_Attempts and then
                                      The_Proof.Is_Unjustified));
   end Should_Show_Proof;

begin --  Print_Entities
   for Entity of Entities loop
      if Should_Show_Entity (Entity => Entity) then
         SPAT.Log.Message (Message  => SPAT.To_String (Source => Entity),
                           New_Line => False);
         Ada.Text_IO.Set_Col (File => Ada.Text_IO.Standard_Output,
                              To   => Second_Column);
         SPAT.Log.Message
           (Message =>
              "=> " &
              (if Info.Has_Unproved_Attempts (Entity => Entity)
               then "--" -- Useless if nothing is proven.
               else SPAT.Image (Value => Info.Max_Success_Proof_Time (Entity => Entity))) &
              "/" & SPAT.Image (Value => Info.Max_Proof_Time (Entity => Entity)) &
              "/" & SPAT.Image (Value => Info.Total_Proof_Time (Entity => Entity)));

         if Detail_Level > SPAT.Command_Line.None then
            for PI_Position in Info.Proof_Tree (Entity => Entity) loop
               declare
                  The_Proof : SPAT.Proof_Item.T'Class renames
                    SPAT.Proof_Item.T'Class
                      (SPAT.Entity.Tree.Element (Position => PI_Position));
               begin
                  if Should_Show_Proof (The_Proof => The_Proof) then
                     SPAT.Log.Message (Message => "`-" & The_Proof.Image);

                     if Detail_Level > Level_1 then
                        for Check_Position in
                          Info.Iterate_Children (Entity   => Entity,
                                                 Position => PI_Position)
                        loop
                           declare
                              The_Check : SPAT.Proof_Item.Checks_Sentinel'Class renames
                                SPAT.Proof_Item.Checks_Sentinel'Class
                                  (SPAT.Entity.Tree.Element
                                     (Position => Check_Position));
                           begin
                              if Should_Show_Check (The_Check => The_Check) then
                                 Ada.Text_IO.Set_Col
                                   (File => Ada.Text_IO.Standard_Output,
                                    To   => 2);
                                 SPAT.Log.Message (Message  => "`",
                                                   New_Line => False);

                                 for Attempt_Position in
                                   Info.Iterate_Children
                                     (Entity   => Entity,
                                      Position => Check_Position)
                                 loop
                                    declare
                                       The_Attempt : SPAT.Proof_Attempt.T'Class renames
                                         SPAT.Proof_Attempt.T'Class
                                           (SPAT.Entity.Tree.Element
                                              (Position => Attempt_Position));
                                    begin
                                       Ada.Text_IO.Set_Col
                                         (File => Ada.Text_IO.Standard_Output,
                                          To   => 3);
                                       SPAT.Log.Message
                                         (Message => "-" & The_Attempt.Image);
                                    end;
                                 end loop;
                              end if;
                           end;
                        end loop;
                     end if;

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

   if SPAT.Log.Debug_Enabled and then Has_Omitted (F => Cut_Off_Filter) then
      SPAT.Log.Debug
        (Message =>
           "Omitted results below cut-off point (" &
           SPAT.Image (Cut_Off) & "):" &
           Image (F => Cut_Off_Filter) & ".");
   end if;
end Print_Entities;
