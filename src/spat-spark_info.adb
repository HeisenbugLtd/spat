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

   function "<" (Left  : in Flow_Item;
                 Right : in Flow_Item) return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Left.Where.File = Right.Where.File then
         if Left.Where.Line = Right.Where.Line then
            return Left.Where.Column < Right.Where.Column;
         end if;

         return Left.Where.Line < Right.Where.Line;
      end if;

      return Left.Where.File < Right.Where.File;
   end "<";

   function "<" (Left  : in Proof_Item;
                 Right : in Proof_Item) return Boolean
   is
      use type Ada.Strings.Unbounded.Unbounded_String;
   begin
      if Left.Where.File = Right.Where.File then
         if Left.Where.Line = Right.Where.Line then
            return Left.Where.Column < Right.Where.Column;
         end if;

         return Left.Where.Line < Right.Where.Line;
      end if;

      return Left.Where.File < Right.Where.File;
   end "<";

   function Ensure_Field
     (Obj   : in GNATCOLL.JSON.JSON_Value;
      Field : in GNATCOLL.JSON.UTF8_String;
      Kind  : in GNATCOLL.JSON.JSON_Value_Type) return Boolean
   is
      use type GNATCOLL.JSON.JSON_Value_Type;
   begin
      if not Obj.Has_Field (Field => Field) then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "Warning: Expected field """ & Field & """ not present!");

         return False;
      end if;

      if Obj.Get (Field => Field).Kind /= Kind then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item =>
              "Warning: Field """ & Field & """ not of expected type """ &
              Kind'Image & """!");

         return False;
      end if;

      return True;
   end Ensure_Field;

   --

   function Flow_Time (This : in T) return Duration is
     (This.Timings.Flow);

   function Num_Flows (This : in T) return Natural is
      Result : Natural := 0;
   begin
      for E of This.Entities loop
         Result := Result + Natural (E.Flows.Length);
      end loop;

      return Result;
   end Num_Flows;

   function Num_Proofs (This : in T) return Natural is
      Result : Natural := 0;
   begin
      for E of This.Entities loop
         Result := Result + Natural (E.Proofs.Length);
      end loop;

      return Result;
   end Num_Proofs;

   function Proof_Time (This : in T) return Duration is
     (This.Timings.Proof);

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

   --

   package Field_Names is

      Assumptions   : constant GNATCOLL.JSON.UTF8_String := "assumptions";
      Column        : constant GNATCOLL.JSON.UTF8_String := "col";
      Entity        : constant GNATCOLL.JSON.UTF8_String := "entity";
      File          : constant GNATCOLL.JSON.UTF8_String := "file";
      Flow          : constant GNATCOLL.JSON.UTF8_String := "flow";
      Flow_Analysis : constant GNATCOLL.JSON.UTF8_String := "flow analysis";
      Line          : constant GNATCOLL.JSON.UTF8_String := "line";
      Name          : constant GNATCOLL.JSON.UTF8_String := "name";
      Proof         : constant GNATCOLL.JSON.UTF8_String := "proof";
      Rule          : constant GNATCOLL.JSON.UTF8_String := "rule";
      Severity      : constant GNATCOLL.JSON.UTF8_String := "severity";
      Sloc          : constant GNATCOLL.JSON.UTF8_String := "sloc";
      Spark         : constant GNATCOLL.JSON.UTF8_String := "spark";
      Timings       : constant GNATCOLL.JSON.UTF8_String := "timings";

   end Field_Names;

   procedure Map_Sloc_Elements (This      : in out T;
                                Update_At : in     Analyzed_Entities.Cursor;
                                Root      : in     GNATCOLL.JSON.JSON_Array) is
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Sloc : constant GNATCOLL.JSON.JSON_Value :=
                     GNATCOLL.JSON.Get (Arr   => Root,
                                        Index => I);
         begin
            if
              Ensure_Field (Obj   => Sloc,
                            Field => Field_Names.File,
                            Kind  => GNATCOLL.JSON.JSON_String_Type) and then
              Ensure_Field (Obj   => Sloc,
                            Field => Field_Names.Line,
                            Kind  => GNATCOLL.JSON.JSON_Int_Type)
            then
               --  Update element.
               This.Entities (Update_At).Locations.Append
                 (New_Item =>
                    File_Line_Item'
                      (File_Name   => Sloc.Get (Field => Field_Names.File),
                       Line_Number => Sloc.Get (Field => Field_Names.Line)));
            end if;
         end;
      end loop;
   end Map_Sloc_Elements;

   procedure Map_Entities (This : in out T;
                           Root : in     GNATCOLL.JSON.JSON_Value) is
   begin
      if
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Name,
                      Kind  => GNATCOLL.JSON.JSON_String_Type) and then
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Sloc,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         declare
            Obj_Name  : constant Ada.Strings.Unbounded.Unbounded_String :=
              Root.Get (Field => Field_Names.Name);
            Slocs     : constant GNATCOLL.JSON.JSON_Array :=
              Root.Get (Field => Field_Names.Sloc);
            Update_At :          Analyzed_Entities.Cursor :=
              This.Entities.Find (Key => Obj_Name);

            use type Analyzed_Entities.Cursor;
         begin
            if Update_At = Analyzed_Entities.No_Element then
               declare
                  Element        : Analyzed_Entity;
                  Dummy_Inserted : Boolean;
               begin
                  This.Entities.Insert (Key      => Obj_Name,
                                        New_Item => Element,
                                        Position => Update_At,
                                        Inserted => Dummy_Inserted);
               end;
            end if;

            This.Map_Sloc_Elements (Root      => Slocs,
                                    Update_At => Update_At);
         end;
      end if;
   end Map_Entities;

   procedure Map_SPARK_Elements (This : in out T;
                                 Root : in     GNATCOLL.JSON.JSON_Array)
   is
      Length : constant Natural := GNATCOLL.JSON.Length (Arr => Root);
   begin
      This.Entities.Reserve_Capacity
        (Capacity => Ada.Containers.Count_Type (Length));

      for I in 1 .. Length loop
         This.Map_Entities (Root => GNATCOLL.JSON.Get (Arr   => Root,
                                                       Index => I));
      end loop;
   end Map_SPARK_Elements;

   procedure Map_Flow_Elements (This : in out T;
                                Root : in     GNATCOLL.JSON.JSON_Array) is
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Element : constant GNATCOLL.JSON.JSON_Value :=
                        GNATCOLL.JSON.Get (Arr   => Root,
                                           Index => I);
         begin
            if
              Ensure_Field (Obj   => Element,
                            Field => Field_Names.Entity,
                            Kind  => GNATCOLL.JSON.JSON_Object_Type)
            then
               declare
                  Source_Entity : constant GNATCOLL.JSON.JSON_Value :=
                         	    Element.Get (Field => Field_Names.Entity);
               begin
                  --  The name referenced here should match a name we already
                  --  have in the hash table.
                  if
                    Ensure_Field (Obj   => Source_Entity,
                                  Field => Field_Names.Name,
                                  Kind  => GNATCOLL.JSON.JSON_String_Type)
                  then
                     declare
                        The_Key : constant Ada.Strings.Unbounded.Unbounded_String :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);

                        use type Analyzed_Entities.Cursor;
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           declare
                              The_Item : Flow_Item;
                           begin
                              --  TODO: Add flow information into hash table.
                              if
                                Ensure_Field (Obj   => Element,
                                              Field => Field_Names.File,
                                              Kind  => GNATCOLL.JSON.JSON_String_Type) and then
                                Ensure_Field (Obj   => Element,
                                              Field => Field_Names.Line,
                                              Kind  => GNATCOLL.JSON.JSON_Int_Type) and then
                                Ensure_Field (Obj   => Element,
                                              Field => Field_Names.Column,
                                              Kind  => GNATCOLL.JSON.JSON_Int_Type)
                              then
                                 The_Item.Where :=
                                   Entity_Location'(File   => Element.Get (Field => Field_Names.File),
                                                    Line   => Element.Get (Field => Field_Names.Line),
                                                    Column => Element.Get (Field => Field_Names.Column));

                                 if
                                   Ensure_Field
                                     (Obj   => Element,
                                      Field => Field_Names.Rule,
                                      Kind  => GNATCOLL.JSON.JSON_String_Type)
                                 then
                                    The_Item.Rule :=
                                      Element.Get (Field => Field_Names.Rule);
                                 end if;

                                 if
                                   Ensure_Field
                                     (Obj   => Element,
                                      Field => Field_Names.Severity,
                                      Kind  => GNATCOLL.JSON.JSON_String_Type)
                                 then
                                    The_Item.Severity :=
                                      Element.Get (Field => Field_Names.Severity);
                                 end if;

                                 This.Entities (Update_At).Flows.Append
                                   (New_Item => The_Item);
                              end if;
                           end;
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

   procedure Map_Proof_Elements (This : in out T;
                                 Root : in     GNATCOLL.JSON.JSON_Array) is
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Element : constant GNATCOLL.JSON.JSON_Value :=
                        GNATCOLL.JSON.Get (Arr   => Root,
                                           Index => I);
         begin
            if
              Ensure_Field (Obj   => Element,
                            Field => Field_Names.Entity,
                            Kind  => GNATCOLL.JSON.JSON_Object_Type)
            then
               declare
                  Source_Entity : constant GNATCOLL.JSON.JSON_Value :=
                         	    Element.Get (Field => Field_Names.Entity);
               begin
                  --  The name referenced here should match a name we already
                  --  have in the hash table.
                  if
                    Ensure_Field (Obj   => Source_Entity,
                                  Field => Field_Names.Name,
                                  Kind  => GNATCOLL.JSON.JSON_String_Type)
                  then
                     declare
                        The_Key : constant Ada.Strings.Unbounded.Unbounded_String :=
                          Source_Entity.Get (Field => Field_Names.Name);
                        Update_At : constant Analyzed_Entities.Cursor :=
                          This.Entities.Find (Key => The_Key);

                        use type Analyzed_Entities.Cursor;
                     begin
                        if Update_At /= Analyzed_Entities.No_Element then
                           declare
                              The_Item : Proof_Item;
                           begin
                              --  TODO: Add flow information into hash table.
                              if
                                Ensure_Field (Obj   => Element,
                                              Field => Field_Names.File,
                                              Kind  => GNATCOLL.JSON.JSON_String_Type) and then
                                Ensure_Field (Obj   => Element,
                                              Field => Field_Names.Line,
                                              Kind  => GNATCOLL.JSON.JSON_Int_Type) and then
                                Ensure_Field (Obj   => Element,
                                              Field => Field_Names.Column,
                                              Kind  => GNATCOLL.JSON.JSON_Int_Type)
                              then
                                 The_Item.Where :=
                                   Entity_Location'(File   => Element.Get (Field => Field_Names.File),
                                                    Line   => Element.Get (Field => Field_Names.Line),
                                                    Column => Element.Get (Field => Field_Names.Column));

                                 if
                                   Ensure_Field
                                     (Obj   => Element,
                                      Field => Field_Names.Rule,
                                      Kind  => GNATCOLL.JSON.JSON_String_Type)
                                 then
                                    The_Item.Rule :=
                                      Element.Get (Field => Field_Names.Rule);
                                 end if;

                                 if
                                   Ensure_Field
                                     (Obj   => Element,
                                      Field => Field_Names.Severity,
                                      Kind  => GNATCOLL.JSON.JSON_String_Type)
                                 then
                                    The_Item.Severity :=
                                      Element.Get (Field => Field_Names.Severity);
                                 end if;

                                 This.Entities (Update_At).Proofs.Append
                                   (New_Item => The_Item);
                              end if;
                           end;
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

   procedure Map_Assumptions_Elements
     (This : in out T;
      Root : in     GNATCOLL.JSON.JSON_Array) is
   begin
      --  TODO: Add all elements from the "assumptions" array.
      null;
   end Map_Assumptions_Elements;

   procedure Map_Timings (This : in out T;
                          Root : in     GNATCOLL.JSON.JSON_Value) is
   begin
      if
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Proof,
                      Kind  => GNATCOLL.JSON.JSON_Float_Type) and then
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Flow_Analysis,
                      Kind  => GNATCOLL.JSON.JSON_Float_Type)
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

   procedure Map_SPARK_File (This :    out T;
                             Root : in     GNATCOLL.JSON.JSON_Value) is
   begin
      This.Entities.Clear;
      This.Timings := Null_Timing_Item;

      --  If I understand the .spark file format correctly, this should
      --  establish the table of all known analysis elements.
      if
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Spark,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Spark_Elements
           (Root => Root.Get (Field => Field_Names.Spark));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Flow,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Flow_Elements
           (Root => Root.Get (Field => Field_Names.Flow));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Proof,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Proof_Elements
           (Root => Root.Get (Field => Field_Names.Proof));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.Assumptions,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Assumptions_Elements
           (Root => Root.Get (Field => Field_Names.Assumptions));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Field_Names.TImings,
                      Kind  => GNATCOLL.JSON.JSON_Object_Type)
      then
         This.Map_Timings (Root => Root.Get (Field => Field_Names.Timings));
      end if;
   end Map_SPARK_File;

end SPAT.Spark_Info;
