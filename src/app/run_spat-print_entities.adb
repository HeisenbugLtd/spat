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

with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;
with SPAT.Entity.Tree;
with SPAT.Log;
with SPAT.Proof_Attempt;
with SPAT.Proof_Item;
with SPAT.Strings;
with SPAT.String_Tables;

separate (Run_SPAT)

------------------------------------------------------------------------------
--  Print_Entities
------------------------------------------------------------------------------
procedure Print_Entities (Info          : in SPAT.Spark_Info.T;
                          Sort_By       : in SPAT.Spark_Info.Sorting_Criterion;
                          Cut_Off       : in Duration;
                          Entity_Filter : in Reg_Exp_List.Vector)
is
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

   Mode          : constant SPAT.Command_Line.Report_Mode :=
     SPAT.Command_Line.Report.Get;
   Detail_Level  : constant SPAT.Command_Line.Detail_Level :=
     SPAT.Command_Line.Details.Get;

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

   use all type SPAT.Command_Line.Detail_Level;

   Entities : constant SPAT.Strings.Entity_Names :=
     Info.List_All_Entities (Sort_By => Sort_By);

   package Output_Columns is new SPAT.String_Tables (Columns => 4);

   ---------------------------------------------------------------------------
   --  Split_Into
   ---------------------------------------------------------------------------
   procedure Split_Into (Source   : in     String;
                         Split_By : in     String;
                         Target   :    out Output_Columns.Row);

   ---------------------------------------------------------------------------
   --  Split_Into
   ---------------------------------------------------------------------------
   procedure Split_Into (Source   : in     String;
                         Split_By : in     String;
                         Target   :    out Output_Columns.Row)
   is
      Split_At : constant Natural :=
        Ada.Strings.Fixed.Index (Source  => Source,
                                 Pattern => Split_By);
   begin
      if Split_At > Source'First then
         Target :=
           (1 =>
              SPAT.To_Name
                (Source =>
                   Ada.Strings.Fixed.Trim
                     (Source => Source (Source'First .. Split_At - 1),
                      Side   => Ada.Strings.Both)),
            2 => SPAT.To_Name (Source => Split_By),
            3 =>
              SPAT.To_Name
                (Source =>
                   Ada.Strings.Fixed.Trim
                     (Source =>
                        Source (Split_At + Split_By'Length .. Source'Last),
                      Side   => Ada.Strings.Both)),
            4 => SPAT.Null_Name);
      else
         Target := (1      => SPAT.To_Name (Source => Source),
                    others => SPAT.Null_Name);
      end if;
   end Split_Into;

   Output_List : Output_Columns.Row_Vectors.Vector;
   Current_Row : Output_Columns.Row;
begin --  Print_Entities
   for Entity of Entities loop
      if Should_Show_Entity (Entity => Entity) then
         Current_Row :=
           (1 => SPAT.Subject_Name (Entity),
            2 => SPAT.To_Name (" => "),
            3 => SPAT.Null_Name,
            4 =>
              SPAT.To_Name
                (Source =>
                   (if Info.Has_Unproved_Attempts (Entity => Entity)
                    then "--" -- Useless if nothing is proven.
                    else
                      SPAT.Image
                        (Value => Info.Max_Success_Proof_Time (Entity => Entity),
                         Steps => Info.Max_Success_Proof_Steps (Entity => Entity))) &
                      "/" &
                      SPAT.Image
                        (Value => Info.Max_Proof_Time (Entity => Entity),
                         Steps => Info.Max_Proof_Steps (Entity => Entity)) &
                      "/" &
                      SPAT.Image
                        (Value => Info.Total_Proof_Time (Entity => Entity))));
         Output_List.Append (New_Item => Current_Row);

         if Detail_Level > SPAT.Command_Line.None then
            for PI_Position in Info.Proof_Tree (Entity => Entity) loop
               declare
                  The_Proof : SPAT.Proof_Item.T'Class renames
                    SPAT.Proof_Item.T'Class
                      (SPAT.Entity.Tree.Element (Position => PI_Position));
                  use type SPAT.Justification;
               begin
                  if Should_Show_Proof (The_Proof => The_Proof) then
                     Split_Into (Source   => "`-" & The_Proof.Image,
                                 Split_By => " => ",
                                 Target   => Current_Row);
                     Output_List.Append (New_Item => Current_Row);

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
                                 Current_Row := (1      => SPAT.To_Name (" `"),
                                                 others => SPAT.Null_Name);

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
                                       Ada.Strings.Unbounded.Append
                                         (Source => Current_Row (1),
                                          New_Item => "-");
                                       Ada.Strings.Unbounded.Append
                                         (Source   => Current_Row (1),
                                          New_Item => The_Attempt.Image);
                                       Output_List.Append
                                         (New_Item => Current_Row);
                                       Current_Row :=
                                         (1      => SPAT.To_Name ("  "),
                                          others => SPAT.Null_Name);
                                    end;
                                 end loop;
                              end if;
                           end;
                        end loop;
                     end if;

                     if
                       The_Proof.Suppressed /= SPAT.Justification (SPAT.Null_Name)
                     then
                        Current_Row :=
                          (1 =>
                             SPAT.To_Name
                               (Source =>
                                  "Justified with: """ &
                                  SPAT.To_String (The_Proof.Suppressed) & """."),
                           others => SPAT.Null_Name);
                        Output_List.Append (New_Item => Current_Row);
                     end if;
                  end if;
               end;
            end loop;
         end if;
      end if;
   end loop;

   Output_Columns.Put (Item => Output_List);

   if SPAT.Log.Debug_Enabled and then Has_Omitted (F => Cut_Off_Filter) then
      SPAT.Log.Debug
        (Message =>
           "Omitted results below cut-off point (" &
           SPAT.Image (Cut_Off) & "):" &
           Image (F => Cut_Off_Filter) & ".");
   end if;
end Print_Entities;
