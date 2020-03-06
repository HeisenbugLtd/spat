------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Text_IO;

with SPAT.Preconditions;

package body SPAT.Spark_Info is

   use type Ada.Containers.Count_Type;
   use type Analyzed_Entities.Cursor;

   --  Make JSON type enumeration literals directly visible.
   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Subprogram prototypes.
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Map_Assumptions_Elements
   ---------------------------------------------------------------------------
   procedure Map_Assumptions_Elements (This : in out T;
                                       Root : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Entities
   ---------------------------------------------------------------------------
   procedure Map_Entities (This : in out T;
                           Root : in     JSON_Value) with
     Pre => (Preconditions.Ensure_Field (Object => Root,
                                         Field  => Field_Names.Name,
                                         Kind   => JSON_String_Type) and then
             Preconditions.Ensure_Field (Object => Root,
                                         Field  => Field_Names.Sloc,
                                         Kind   => JSON_Array_Type));

   ---------------------------------------------------------------------------
   --  Map_Flow_Elements
   ---------------------------------------------------------------------------
   procedure Map_Flow_Elements (This : in out T;
                                Root : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Proof_Elements
   ---------------------------------------------------------------------------
   procedure Map_Proof_Elements (This : in out T;
                                 Root : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Sloc_Elements
   ---------------------------------------------------------------------------
   procedure Map_Sloc_Elements (This   : in out T;
                                Add_To : in out Entity_Lines.Vector;
                                Root   : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Spark_Elements
   ---------------------------------------------------------------------------
   procedure Map_Spark_Elements (This : in out T;
                                 Root : in     JSON_Array);

   ---------------------------------------------------------------------------
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This : in out T;
                          Root : in     JSON_Value);

   ---------------------------------------------------------------------------
   --  Subprogram implementations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Flow_Time
   ---------------------------------------------------------------------------
   function Flow_Time (This : in T) return Duration is
     (This.Timings.Flow);

   ---------------------------------------------------------------------------
   --  List_All_Entities
   ---------------------------------------------------------------------------
   function List_All_Entities (This : in T) return String_Array is
   begin
      return Result : String_Array (1 .. Natural (This.Entities.Length)) do
         declare
            Current_Index : Positive := Result'First;
         begin
            for Index in This.Entities.Iterate loop
               Result (Current_Index) :=
                 Analyzed_Entities.Key (Position => Index);
               Current_Index := Current_Index + 1;
            end loop;
         end;

         Sort_By_Name (Container => Result);
      end return;
   end List_All_Entities;

   ---------------------------------------------------------------------------
   --  Map_Assumptions_Elements
   ---------------------------------------------------------------------------
   procedure Map_Assumptions_Elements (This : in out T;
                                       Root : in     JSON_Array) is
   begin
      --  TODO: Add all elements from the "assumptions" array.
      null;
   end Map_Assumptions_Elements;

   ---------------------------------------------------------------------------
   --  Map_Entities
   ---------------------------------------------------------------------------
   procedure Map_Entities (This : in out T;
                           Root : in     JSON_Value)
   is
      Obj_Name : constant Entity_Name := Root.Get (Field => Field_Names.Name);
      Slocs    : constant JSON_Array  := Root.Get (Field => Field_Names.Sloc);
      Index    :          Analyzed_Entities.Cursor :=
        This.Entities.Find (Key => Obj_Name);
   begin
      if Index = Analyzed_Entities.No_Element then
         declare
            Element        : Analyzed_Entity;
            Dummy_Inserted : Boolean;
         begin
            This.Entities.Insert (Key      => Obj_Name,
                                  New_Item => Element,
                                  Position => Index,
                                  Inserted => Dummy_Inserted);
         end;
      end if;

      This.Map_Sloc_Elements
        (Root   => Slocs,
         Add_To => This.Entities (Index).Source_Lines);
   end Map_Entities;

   ---------------------------------------------------------------------------
   --  Map_Flow_Elements
   ---------------------------------------------------------------------------
   procedure Map_Flow_Elements (This : in out T;
                                Root : in     JSON_Array) is
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Element : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                                Index => I);
         begin
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Entity,
                                          Kind   => JSON_Object_Type)
            then
               declare
                  Source_Entity : constant JSON_Value :=
                    Element.Get (Field => Field_Names.Entity);
               begin
                  --  The name referenced here should match a name we already
                  --  have in the hash table.
                  if
                    Preconditions.Ensure_Field (Object => Source_Entity,
                                                Field  => Field_Names.Name,
                                                Kind   => JSON_String_Type)
                  then
                     declare
                        The_Key : constant Entity_Name :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Preconditions.Ensure_File_Line_Column
                               (Object => Element) and then
                             Preconditions.Ensure_Rule_Severity
                               (Object => Element)
                           then
                              This.Entities (Update_At).Flows.Append
                                (New_Item =>
                                   Flow_Items.Create (Object => Element));
                           end if;
                        else
                           Ada.Text_IO.Put_Line
                             (File => Ada.Text_IO.Standard_Error,
                              Item =>
                                "Warning (Flow): """ & To_String (The_Key) &
                                """ not found in index.");
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Sort flows by file name:line:column.
      for E of This.Entities loop
         Flow_Items.By_Location.Sort (Container => E.Flows);
      end loop;
   end Map_Flow_Elements;

   ---------------------------------------------------------------------------
   --  Map_Proof_Elements
   ---------------------------------------------------------------------------
   procedure Map_Proof_Elements (This : in out T;
                                 Root : in     JSON_Array) is
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Element : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                                Index => I);
         begin
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Entity,
                                          Kind   => JSON_Object_Type)
            then
               declare
                  Source_Entity : constant JSON_Value :=
                    Element.Get (Field => Field_Names.Entity);
               begin
                  --  The name referenced here should match a name we already
                  --  have in the hash table.
                  if
                    Preconditions.Ensure_Field (Object => Source_Entity,
                                                Field  => Field_Names.Name,
                                                Kind   => JSON_String_Type)
                  then
                     declare
                        The_Key : constant Entity_Name :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Preconditions.Ensure_File_Line_Column
                               (Object => Element) and then
                             Preconditions.Ensure_Rule_Severity
                               (Object => Element)
                           then
                              This.Entities (Update_At).Proofs.Append
                                (New_Item =>
                                   Proof_Items.Create (Object => Element));
                           end if;
                        else
                           Ada.Text_IO.Put_Line
                             (File => Ada.Text_IO.Standard_Error,
                              Item =>
                                "Warning (Flow): """ & To_String (The_Key) &
                                """ not found in index.");
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;

      --  Sort proofs by file name:line:column.
      for E of This.Entities loop
         Proof_Items.By_Location.Sort (Container => E.Proofs);
      end loop;
   end Map_Proof_Elements;

   ---------------------------------------------------------------------------
   --  Map_Sloc_Elements
   ---------------------------------------------------------------------------
   procedure Map_Sloc_Elements (This   : in out T;
                                Add_To : in out Entity_Lines.Vector;
                                Root   : in     JSON_Array)
   is
      pragma Unreferenced (This);
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Sloc : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                             Index => I);
         begin
            if Preconditions.Ensure_File_Line (Object => Sloc) then
               Add_To.Append (New_Item => Entity_Lines.Create (Object => Sloc));
            end if;
         end;
      end loop;
   end Map_Sloc_Elements;

   ---------------------------------------------------------------------------
   --  Map_Spark_Elements
   ---------------------------------------------------------------------------
   procedure Map_Spark_Elements (This : in out T;
                                 Root : in     JSON_Array)
   is
      Length : constant Natural := GNATCOLL.JSON.Length (Arr => Root);
   begin
      This.Entities.Reserve_Capacity
        (Capacity => Ada.Containers.Count_Type (Length));

      for I in 1 .. Length loop
         declare
            Element : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                                Index => I);
         begin
            if
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Name,
                                          Kind   => JSON_String_Type) and then
              Preconditions.Ensure_Field (Object => Element,
                                          Field  => Field_Names.Sloc,
                                          Kind   => JSON_Array_Type)
            then
               This.Map_Entities (Root => Element);
            end if;
         end;
      end loop;
   end Map_Spark_Elements;

   ---------------------------------------------------------------------------
   --  Map_Spark_File
   ---------------------------------------------------------------------------
   procedure Map_Spark_File (This :    out T;
                             Root : in     JSON_Value) is
   begin
      This.Entities.Clear;
      This.Timings := Timing_Items.None;

      --  If I understand the .spark file format correctly, this should
      --  establish the table of all known analysis elements.
      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Spark,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Spark_Elements
           (Root => Root.Get (Field => Field_Names.Spark));
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Flow,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Flow_Elements
           (Root => Root.Get (Field => Field_Names.Flow));
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Proof,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Proof_Elements
           (Root => Root.Get (Field => Field_Names.Proof));
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Assumptions,
                                    Kind   => JSON_Array_Type)
      then
         This.Map_Assumptions_Elements
           (Root => Root.Get (Field => Field_Names.Assumptions));
      end if;

      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Timings,
                                    Kind   => JSON_Object_Type)
      then
         This.Map_Timings (Root => Root.Get (Field => Field_Names.Timings));
      end if;
   end Map_Spark_File;

   ---------------------------------------------------------------------------
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This : in out T;
                          Root : in     JSON_Value) is
   begin
      if
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Proof,
                                    Kind   => JSON_Float_Type) and then
        Preconditions.Ensure_Field (Object => Root,
                                    Field  => Field_Names.Flow_Analysis,
                                    Kind   => JSON_Float_Type)
      then
         This.Timings :=
           Timing_Items.T'
             (Duration (Float'(Root.Get (Field => Field_Names.Proof))),
              Duration (Float'(Root.Get (Field =>
                                           Field_Names.Flow_Analysis))));
      else
         This.Timings := Timing_Items.None;
      end if;
   end Map_Timings;

   ---------------------------------------------------------------------------
   --  Max_Proof_Time
   ---------------------------------------------------------------------------
   function Max_Proof_Time (This    : in T;
                            Element : in Entity_Name) return Duration
   is
      Max_Time : Duration := 0.0;
   begin
      for P of This.Entities (Element).Proofs loop
         Max_Time := Duration'Max (Max_Time, P.Max_Time);
      end loop;

      return Max_Time;
   end Max_Proof_Time;

   ---------------------------------------------------------------------------
   --  Num_Flows
   ---------------------------------------------------------------------------
   function Num_Flows (This : in T) return Ada.Containers.Count_Type is
      Result : Ada.Containers.Count_Type := 0;
   begin
      for E of This.Entities loop
         Result := Result + E.Flows.Length;
      end loop;

      return Result;
   end Num_Flows;

   ---------------------------------------------------------------------------
   --  Num_Proofs
   ---------------------------------------------------------------------------
   function Num_Proofs (This : in T) return Ada.Containers.Count_Type is
      Result : Ada.Containers.Count_Type := 0;
   begin
      for E of This.Entities loop
         Result := Result + E.Proofs.Length;
      end loop;

      return Result;
   end Num_Proofs;

   ---------------------------------------------------------------------------
   --  Proof_Time
   ---------------------------------------------------------------------------
   function Proof_Time (This : in T) return Duration is
     (This.Timings.Proof);

   ---------------------------------------------------------------------------
   --  Total_Proof_Time
   ---------------------------------------------------------------------------
   function Total_Proof_Time (This    : in T;
                              Element : in Entity_Name) return Duration
   is
      Total_Time : Duration := 0.0;
   begin
      for P of This.Entities (Element).Proofs loop
         Total_Time := Duration'Max (Total_Time, P.Total_Time);
      end loop;

      return Total_Time;
   end Total_Proof_Time;

end SPAT.Spark_Info;
