------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Containers.Generic_Array_Sort;
with Ada.Directories;
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
                          File : in     Subject_Name;
                          Root : in     JSON_Value);

   ---------------------------------------------------------------------------
   --  Subprogram implementations
   ---------------------------------------------------------------------------

   ---------------------------------------------------------------------------
   --  Flow_Time
   ---------------------------------------------------------------------------
   function Flow_Time (This : in T;
                       File : in Subject_Name) return Duration is
     (This.Files (File).Flow);

   ---------------------------------------------------------------------------
   --  List_All_Entities
   ---------------------------------------------------------------------------
   function List_All_Entities
     (This    : in T;
      Sort_By : in Sorting_Criterion := Name) return String_Array is
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

         case Sort_By is
            when None =>
               null;

            when Name =>
               declare
                  procedure Sort_By_Name is new
                    Ada.Containers.Generic_Array_Sort
                      (Index_Type   => Positive,
                       Element_Type => Subject_Name,
                       Array_Type   => String_Array,
                       "<"          => "<");
               begin
                  Sort_By_Name (Container => Result);
               end;

            when Time =>
               declare
                  function "<" (Left  : in Subject_Name;
                                Right : in Subject_Name) return Boolean;

                  function "<" (Left  : in Subject_Name;
                                Right : in Subject_Name) return Boolean is
                     (This.Total_Proof_Time (Element => Left) >
                          This.Total_Proof_Time (Element => Right));

                  procedure Sort_By_Time is new
                    Ada.Containers.Generic_Array_Sort (Index_Type   => Positive,
                                                       Element_Type => Subject_Name,
                                                       Array_Type   => String_Array,
                                                       "<"          => "<");
               begin
                  Sort_By_Time (Container => Result);
               end;
         end case;
      end return;
   end List_All_Entities;

   ---------------------------------------------------------------------------
   --  List_All_Files
   ---------------------------------------------------------------------------
   function List_All_Files
     (This    : in T;
      Sort_By : in Sorting_Criterion := None) return String_Array is
   begin
      return Result : String_Array (1 .. Natural (This.Files.Length)) do
         declare
            Current_Index : Positive := Result'First;
         begin
            for Index in This.Files.Iterate loop
               Result (Current_Index) :=
                 File_Timings.Key (Position => Index);
               Current_Index := Current_Index + 1;
            end loop;
         end;

         case Sort_By is
            when None =>
               null; -- Do not sort anything

            when Name =>
               declare
                  function "<" (Left  : in Subject_Name;
                                Right : in Subject_Name) return Boolean is
                    (Ada.Directories.Base_Name
                       (Name => To_String (Source => Left)) <
                     Ada.Directories.Base_Name
                       (Name => To_String (Source => Right)));

                  procedure Sort_By_Name is new
                    Ada.Containers.Generic_Array_Sort
                      (Index_Type   => Positive,
                       Element_Type => Subject_Name,
                       Array_Type   => String_Array);
               begin
                  Sort_By_Name (Container => Result);
               end;

            when Time =>
               declare
                  function "<" (Left  : in Subject_Name;
                                Right : in Subject_Name) return Boolean;

                  function "<" (Left  : in Subject_Name;
                                Right : in Subject_Name) return Boolean is
                    ((This.Proof_Time (File => Left) +
                        This.Flow_Time (File => Left)) >
                     (This.Proof_Time (File => Right) +
                        This.Flow_Time (File => Right)));

                  procedure Sort_By_Time is new
                    Ada.Containers.Generic_Array_Sort (Index_Type   => Positive,
                                                       Element_Type => Subject_Name,
                                                       Array_Type   => String_Array,
                                                       "<"          => "<");
               begin
                  Sort_By_Time (Container => Result);
               end;
         end case;
      end return;
   end List_All_Files;

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
      Obj_Name : constant Subject_Name := Root.Get (Field => Field_Names.Name);
      Slocs    : constant JSON_Array   := Root.Get (Field => Field_Names.Sloc);
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
                        The_Key : constant Subject_Name :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Flow_Items.Has_Required_Fields (Object => Element)
                           then
                              This.Entities (Update_At).Flows.Append
                                (New_Item =>
                                   Flow_Items.Create (Object => Element));
                           end if;
                        else
                           Ada.Text_IO.Put_Line
                             (File => Ada.Text_IO.Standard_Error,
                              Item =>
                                "Warning (Flow): """ &
                                To_String (Source => The_Key) &
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
                        The_Key : constant Subject_Name :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           if
                             Proof_Items.Has_Required_Fields (Object => Element)
                           then
                              This.Entities (Update_At).Proofs.Append
                                (New_Item =>
                                   Proof_Items.Create (Object => Element));
                           end if;
                        else
                           Ada.Text_IO.Put_Line
                             (File => Ada.Text_IO.Standard_Error,
                              Item =>
                                "Warning (Flow): """ &
                                To_String (Source => The_Key) &
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
            if Entity_Lines.Has_Required_Fields (Object => Sloc) then
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
   procedure Map_Spark_File (This : in out T;
                             File : in     Subject_Name;
                             Root : in     JSON_Value) is
   begin
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
         This.Map_Timings (File => File,
                           Root => Root.Get (Field => Field_Names.Timings));
      end if;
   end Map_Spark_File;

   ---------------------------------------------------------------------------
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This : in out T;
                          File : in     Subject_Name;
                          Root : in     JSON_Value) is
   begin
      if
        Timing_Items.Has_Required_Fields (Object => Root)
      then
         This.Files.Insert (Key      => File,
                            New_Item => Timing_Items.Create (Object => Root));
      else
         This.Files.Insert (Key      => File,
                            New_Item => Timing_Items.None);
      end if;
   end Map_Timings;

   ---------------------------------------------------------------------------
   --  Max_Proof_Time
   ---------------------------------------------------------------------------
   function Max_Proof_Time (This    : in T;
                            Element : in Subject_Name) return Duration
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
   function Proof_Time (This : in T;
                        File : in Subject_Name) return Duration is
     (This.Files (File).Proof);

   ---------------------------------------------------------------------------
   --  Total_Proof_Time
   ---------------------------------------------------------------------------
   function Total_Proof_Time (This    : in T;
                              Element : in Subject_Name) return Duration
   is
      Total_Time : Duration := 0.0;
   begin
      for P of This.Entities (Element).Proofs loop
         Total_Time := Duration'Max (Total_Time, P.Total_Time);
      end loop;

      return Total_Time;
   end Total_Proof_Time;

end SPAT.Spark_Info;
