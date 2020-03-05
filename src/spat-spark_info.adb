------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Text_IO;

package body SPAT.Spark_Info is

   use type Ada.Containers.Count_Type;
   use type Ada.Strings.Unbounded.Unbounded_String;
   use type Analyzed_Entities.Cursor;
   use type GNATCOLL.JSON.JSON_Value_Type;

   --  Make JSON type enumeration literals directly visible.
   use all type GNATCOLL.JSON.JSON_Value_Type;

   --  Type renames for commonly used JSON types from GNATCOLL.JSON
   subtype JSON_Array      is GNATCOLL.JSON.JSON_Array;
   subtype JSON_Value      is GNATCOLL.JSON.JSON_Value;
   subtype JSON_Value_Type is GNATCOLL.JSON.JSON_Value_Type;
   subtype UTF8_String     is GNATCOLL.JSON.UTF8_String;

   ---------------------------------------------------------------------------
   -- "<"
   --
   --  Compares to locations and returns True for the one which comes first,
   --  either by file name, line number and then column number.
   ---------------------------------------------------------------------------
   function "<" (Left  : in Entity_Location;
                 Right : in Entity_Location) return Boolean is
   begin
      if Left.File = Right.File then
         if Left.Line = Right.Line then
            return Left.Column < Right.Column;
         end if;

         return Left.Line < Right.Line;
      end if;

      return Left.File < Right.File;
   end "<";

   ---------------------------------------------------------------------------
   --  Ensure_Field
   --
   --  Check that the given JSON object contains an object named Field with
   --  type of Kind.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Field (Object : in JSON_Value;
                          Field  : in UTF8_String;
                          Kind   : in JSON_Value_Type) return Boolean is
   begin
      if not Object.Has_Field (Field => Field) then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "Warning: Expected field """ & Field & """ not present!");

         return False;
      end if;

      if Object.Get (Field => Field).Kind /= Kind then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item =>
              "Warning: Field """ & Field & """ not of expected type """ &
              Kind'Image & """!");

         return False;
      end if;

      return True;
   end Ensure_Field;

   --  Encapsulates names of recognized fields in JSON data.
   package Field_Names is

      Assumptions   : constant UTF8_String := "assumptions";
      Column        : constant UTF8_String := "col";
      Entity        : constant UTF8_String := "entity";
      File          : constant UTF8_String := "file";
      Flow          : constant UTF8_String := "flow";
      Flow_Analysis : constant UTF8_String := "flow analysis";
      Line          : constant UTF8_String := "line";
      Name          : constant UTF8_String := "name";
      Proof         : constant UTF8_String := "proof";
      Rule          : constant UTF8_String := "rule";
      Severity      : constant UTF8_String := "severity";
      Sloc          : constant UTF8_String := "sloc";
      Spark         : constant UTF8_String := "spark";
      Timings       : constant UTF8_String := "timings";

   end Field_Names;

   ---------------------------------------------------------------------------
   --  Ensure_File_Line
   ---------------------------------------------------------------------------
   function Ensure_File_Line (Object : in JSON_Value) return Boolean is
     (Ensure_Field (Object => Object,
                    Field  => Field_Names.File,
                    Kind   => JSON_String_Type) and then
      Ensure_Field (Object => Object,
                    Field  => Field_Names.Line,
                    Kind   => JSON_Int_Type));

   ---------------------------------------------------------------------------
   --  Ensure_File_Line_Column
   ---------------------------------------------------------------------------
   function Ensure_File_Line_Column (Object : in JSON_Value) return Boolean is
     (Ensure_File_Line (Object => Object) and then
      Ensure_Field (Object   => Object,
                    Field    => Field_Names.Column,
                    Kind     => JSON_Int_Type));

   ---------------------------------------------------------------------------
   --  Ensure_Rule_Severity
   ---------------------------------------------------------------------------
   function Ensure_Rule_Severity (Object : in JSON_Value) return Boolean is
     (Ensure_Field (Object => Object,
                    Field  => Field_Names.Rule,
                    Kind   => JSON_String_Type) and then
      Ensure_Field (Object => Object,
                    Field  => Field_Names.Severity,
                    Kind   => JSON_String_Type));

   ---------------------------------------------------------------------------
   --  Flow_Time
   ---------------------------------------------------------------------------
   function Flow_Time (This : in T) return Duration is
     (This.Timings.Flow);

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
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : in JSON_Value) return Entity_Line is
     (Entity_Line'(File => Object.Get (Field => Field_Names.File),
                   Line => Object.Get (Field => Field_Names.Line)));

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : in JSON_Value) return Entity_Location is
     (Entity_Line'(Create (Object => Object)) with
        Column => Object.Get (Field => Field_Names.Column));

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : in JSON_Value) return Flow_Item is
     (Entity_Location'(Create (Object => Object)) with
        Rule     => Object.Get (Field => Field_Names.Rule),
        Severity => Object.Get (Field => Field_Names.Severity));

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object : in JSON_Value) return Proof_Item is
     (Entity_Location'(Create (Object => Object)) with
        Rule     => Object.Get (Field => Field_Names.Rule),
        Severity => Object.Get (Field => Field_Names.Severity));

   ---------------------------------------------------------------------------
   --  Map_Sloc_Elements
   ---------------------------------------------------------------------------
   procedure Map_Sloc_Elements (This   : in out T;
                                Add_To : in out Entity_Lines.Vector;
                                Root   : in     JSON_Array) is
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Sloc : constant JSON_Value := GNATCOLL.JSON.Get (Arr   => Root,
                                                             Index => I);
         begin
            if Ensure_File_Line (Object => Sloc) then
               Add_To.Append (New_Item => Create (Object => Sloc));
            end if;
         end;
      end loop;
   end Map_Sloc_Elements;

   ---------------------------------------------------------------------------
   --  Map_Entities
   ---------------------------------------------------------------------------
   procedure Map_Entities (This : in out T;
                           Root : in     JSON_Value) with
     Pre => (Ensure_Field (Object => Root,
                           Field  => Field_Names.Name,
                           Kind   => JSON_String_Type) and then
             Ensure_Field (Object => Root,
                           Field  => Field_Names.Sloc,
                           Kind   => JSON_Array_Type));

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
   --  Map_SPARK_Elements
   ---------------------------------------------------------------------------
   procedure Map_SPARK_Elements (This : in out T;
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
              Ensure_Field (Object => Element,
                            Field  => Field_Names.Name,
                            Kind   => JSON_String_Type) and then
              Ensure_Field (Object => Element,
                            Field  => Field_Names.Sloc,
                            Kind   => JSON_Array_Type)
            then
               This.Map_Entities (Root => Element);
            end if;
         end;
      end loop;
   end Map_SPARK_Elements;

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
              Ensure_Field (Object => Element,
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
                    Ensure_Field (Object => Source_Entity,
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
                             Ensure_File_Line_Column (Object => Element) and then
                             Ensure_Rule_Severity (Object => Element)
                           then
                              This.Entities (Update_At).Flows.Append
                                (New_Item => Create (Object => Element));
                           end if;
                        else
                           Ada.Text_IO.Put_Line
                             (File => Ada.Text_IO.Standard_Error,
                              Item =>
                                "Warning (Flow): """ &
                                Ada.Strings.Unbounded.To_String (The_Key) &
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
         Flow_Items_By_Location.Sort (Container => E.Flows);
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
              Ensure_Field (Object => Element,
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
                    Ensure_Field (Object => Source_Entity,
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
                             Ensure_File_Line_Column (Object => Element) and then
                             Ensure_Rule_Severity (Object => Element)
                           then
                              This.Entities (Update_At).Proofs.Append
                                (New_Item => Create (Object => Element));
                           end if;
                        else
                           Ada.Text_IO.Put_Line
                             (File => Ada.Text_IO.Standard_Error,
                              Item =>
                                "Warning (Flow): """ &
                                Ada.Strings.Unbounded.To_String (The_Key) &
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
         Proof_Items_By_Location.Sort (Container => E.Proofs);
      end loop;
   end Map_Proof_Elements;

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
   --  Map_Timings
   ---------------------------------------------------------------------------
   procedure Map_Timings (This : in out T;
                          Root : in     JSON_Value) is
   begin
      if
        Ensure_Field (Object => Root,
                      Field  => Field_Names.Proof,
                      Kind   => JSON_Float_Type) and then
        Ensure_Field (Object => Root,
                      Field  => Field_Names.Flow_Analysis,
                      Kind   => JSON_Float_Type)
      then
         This.Timings :=
           Timing_Item'
             (Duration (Float'(Root.Get (Field => Field_Names.Proof))),
              Duration (Float'(Root.Get (Field =>
                                           Field_Names.Flow_Analysis))));
      else
         This.Timings := Null_Timing_Item;
      end if;
   end Map_Timings;

   ---------------------------------------------------------------------------
   --  Map_SPARK_File
   ---------------------------------------------------------------------------
   procedure Map_SPARK_File (This :    out T;
                             Root : in     JSON_Value) is
   begin
      This.Entities.Clear;
      This.Timings := Null_Timing_Item;

      --  If I understand the .spark file format correctly, this should
      --  establish the table of all known analysis elements.
      if
        Ensure_Field (Object => Root,
                      Field  => Field_Names.Spark,
                      Kind   => JSON_Array_Type)
      then
         This.Map_Spark_Elements
           (Root => Root.Get (Field => Field_Names.Spark));
      end if;

      if
        Ensure_Field (Object => Root,
                      Field  => Field_Names.Flow,
                      Kind   => JSON_Array_Type)
      then
         This.Map_Flow_Elements
           (Root => Root.Get (Field => Field_Names.Flow));
      end if;

      if
        Ensure_Field (Object => Root,
                      Field  => Field_Names.Proof,
                      Kind   => JSON_Array_Type)
      then
         This.Map_Proof_Elements
           (Root => Root.Get (Field => Field_Names.Proof));
      end if;

      if
        Ensure_Field (Object => Root,
                      Field  => Field_Names.Assumptions,
                      Kind   => JSON_Array_Type)
      then
         This.Map_Assumptions_Elements
           (Root => Root.Get (Field => Field_Names.Assumptions));
      end if;

      if
        Ensure_Field (Object => Root,
                      Field  => Field_Names.Timings,
                      Kind   => JSON_Object_Type)
      then
         This.Map_Timings (Root => Root.Get (Field => Field_Names.Timings));
      end if;
   end Map_SPARK_File;

end SPAT.Spark_Info;
