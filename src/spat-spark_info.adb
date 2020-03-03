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

   function Ensure_Field (Obj   : in GNATCOLL.JSON.JSON_Value;
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
            Item => "Warning: Field """ & Field & """ not of expected type """ & Kind'Image & """!");

         return False;
      end if;

      return True;
   end Ensure_Field;

   --

   function Flow_Time (This : in T) return Duration is
     (This.Timings.Flow);

   function Proof_Time (This : in T) return Duration is
     (This.Timings.Proof);

   --

   procedure Map_Sloc_Elements (This      : in out T;
                                Update_At : in     Source_Entity_Lists.Cursor;
                                Root      : in     GNATCOLL.JSON.JSON_Array)
   is
      File : constant GNATCOLL.JSON.UTF8_String := "file";
      Line : constant GNATCOLL.JSON.UTF8_String := "line";
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Sloc : constant GNATCOLL.JSON.JSON_Value :=
                     GNATCOLL.JSON.Get (Arr   => Root,
                                        Index => I);
         begin
            if
              Ensure_Field (Obj   => Sloc,
                            Field => File,
                            Kind  => GNATCOLL.JSON.JSON_String_Type) and then
              Ensure_Field (Obj   => Sloc,
                            Field => Line,
                            Kind  => GNATCOLL.JSON.JSON_Int_Type)
            then
               Insert_New_Location :
               declare
                  procedure Update_Location (Key     : in     String;
                                             Element : in out Source_Entity)
                  is
                     pragma Unreferenced (Key);
                  begin
                     Element.Locations.Append
                       (New_Item =>
                          Line_Location'(File_Name   => Sloc.Get (Field => File),
                                         Line_Number => Sloc.Get (Field => Line)));
                  end Update_Location;
               begin
                  --  Update element.
                  This.Source_Entity.Update_Element
                    (Position => Update_At,
                     Process  => Update_Location'Access);
               end Insert_New_Location;
            end if;
         end;
      end loop;
   end Map_Sloc_Elements;

   procedure Map_Entities (This : in out T;
                           Root : in     GNATCOLL.JSON.JSON_Value)
   is
      Name : constant GNATCOLL.JSON.UTF8_String := "name";
      Sloc : constant GNATCOLL.JSON.UTF8_String := "sloc";
   begin
      if
        Ensure_Field (Obj   => Root,
                      Field => Name,
                      Kind  => GNATCOLL.JSON.JSON_String_Type) and then
        Ensure_Field (Obj   => Root,
                      Field => Sloc,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         declare
            Obj_Name : constant String                     := Root.Get (Field => Name);
            Slocs    : constant GNATCOLL.JSON.JSON_Array   := Root.Get (Field => Sloc);
            C        :          Source_Entity_Lists.Cursor :=
              This.Source_Entity.Find (Key => Obj_Name);

            use type Source_Entity_Lists.Cursor;
         begin
            if C = Source_Entity_Lists.No_Element then
               declare
                  Element        : Source_Entity;
                  Dummy_Inserted : Boolean;
               begin
                  This.Source_Entity.Insert (Key      => Obj_Name,
                                             New_Item => Element,
                                             Position => C,
                                             Inserted => Dummy_Inserted);
               end;
            end if;

            This.Map_Sloc_Elements (Root      => Slocs,
                                    Update_At => C);
         end;
      end if;
   end Map_Entities;

   procedure Map_SPARK_Elements (This : in out T;
                                 Root : in     GNATCOLL.JSON.JSON_Array)
   is
      Length : constant Natural := GNATCOLL.JSON.Length (Arr => Root);
   begin
      This.Source_Entity.Reserve_Capacity
        (Capacity => Ada.Containers.Count_Type (Length));

      for I in 1 .. Length loop
         This.Map_Entities (Root => GNATCOLL.JSON.Get (Arr   => Root,
                                                       Index => I));
      end loop;
   end Map_SPARK_Elements;

   procedure Map_Flow_Elements (This : in out T;
                                Root : in     GNATCOLL.JSON.JSON_Array)
   is
      Entity : constant String := "entity";
      Name   : constant String := "name";
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Arr => Root) loop
         declare
            Element : constant GNATCOLL.JSON.JSON_Value :=
                        GNATCOLL.JSON.Get (Arr   => Root,
                                           Index => I);
         begin
            if
              Ensure_Field (Obj   => Element,
                            Field => Entity,
                            Kind  => GNATCOLL.JSON.JSON_Object_Type)
            then
               declare
                  Source_Entity : constant GNATCOLL.JSON.JSON_Value :=
                         	    Element.Get (Field => Entity);
               begin
                  --  The name referenced here should match a name we already
                  --  have in the hash table.
                  if
                    Ensure_Field (Obj   => Source_Entity,
                                  Field => Name,
                                  Kind  => GNATCOLL.JSON.JSON_String_Type)
                  then
                     declare
                        The_Key : constant String :=
                                    Source_Entity.Get (Field => Name);
                     begin
                        if This.Source_Entity.Contains (Key => The_Key) then
                           --  TODO: Add flow information into hash table.
                           null;
                        else
                           Ada.Text_IO.Put_Line
                             (File => Ada.Text_IO.Standard_Error,
                              Item => "Warning (Flow): """ & The_Key & """ not in hash map.");
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Map_Flow_Elements;

   procedure Map_Proof_Elements (This : in out T;
                                 Root : in     GNATCOLL.JSON.JSON_Array) is
   begin
      --  TODO: Add all elements from the "proof" array.
      null;
   end Map_Proof_Elements;

   procedure Map_Assumptions_Elements (This : in out T;
                                       Root : in     GNATCOLL.JSON.JSON_Array) is
   begin
      --  TODO: Add all elements from the "assumptions" array.
      null;
   end Map_Assumptions_Elements;

   procedure Map_Timings (This : in out T;
                          Root : in     GNATCOLL.JSON.JSON_Value)
   is
      Proof         : constant GNATCOLL.JSON.UTF8_String := "proof";
      Flow_Analysis : constant GNATCOLL.JSON.UTF8_String := "flow analysis";
   begin
      if
        Ensure_Field (Obj   => Root,
                      Field => Proof,
                      Kind  => GNATCOLL.JSON.JSON_Float_Type) and then
        Ensure_Field (Obj   => Root,
                      Field => Flow_Analysis,
                      Kind  => GNATCOLL.JSON.JSON_Float_Type)
      then
         This.Timings :=
           Timing_Info'(Duration (Float'(Root.Get (Field => Proof))),
                        Duration (Float'(Root.Get (Field => Flow_Analysis))));
      else
         This.Timings := Null_Timing_Info;
      end if;
   end Map_Timings;

   procedure Map_SPARK_File (This :    out T;
                             Root : in     GNATCOLL.JSON.JSON_Value)
   is
      Assumptions : constant GNATCOLL.JSON.UTF8_String := "assumptions";
      Flow        : constant GNATCOLL.JSON.UTF8_String := "flow";
      Proof       : constant GNATCOLL.JSON.UTF8_String := "proof";
      Spark       : constant GNATCOLL.JSON.UTF8_String := "spark";
      Timings     : constant GNATCOLL.JSON.UTF8_String := "timings";
   begin
      This.Source_Entity.Clear;
      This.Timings := Null_Timing_Info;

      --  If I understand the .spark file format correctly, this should
      --  establish the table of all known analysis elements.
      if
        Ensure_Field (Obj   => Root,
                      Field => Spark,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Spark_Elements (Root => Root.Get (Field => Spark));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Flow,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Flow_Elements (Root => Root.Get (Field => Flow));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Proof,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Proof_Elements (Root => Root.Get (Field => Proof));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Assumptions,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Assumptions_Elements (Root => Root.Get (Field => Assumptions));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => TImings,
                      Kind  => GNATCOLL.JSON.JSON_Object_Type)
      then
         This.Map_Timings (Root => Root.Get (Field => Timings));
      end if;
   end Map_SPARK_File;

end SPAT.Spark_Info;
