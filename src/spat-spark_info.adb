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
         Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
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
      for I in 1 .. GNATCOLL.JSON.Length (Root) loop
         declare
            Sloc : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Get (Root, I);
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

   procedure Map_Locations (This : in out T;
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
              This.Source_Entity.Find (Key => Name);

            use type Source_Entity_Lists.Cursor;
         begin
            if C = Source_Entity_Lists.No_Element then
               declare
                  Element        : Source_Entity;
                  Dummy_Inserted : Boolean;
               begin
                  This.Source_Entity.Insert (Key      => Name,
                                             New_Item => Element,
                                             Position => C,
                                             Inserted => Dummy_Inserted);
               end;
            end if;

            This.Map_Sloc_Elements (Root      => Slocs,
                                    Update_At => C);
         end;
      end if;
   end Map_Locations;

   procedure Map_Timings (This           : in out T;
                          Timings_Object : in     GNATCOLL.JSON.JSON_Value)
   is
      Proof         : constant GNATCOLL.JSON.UTF8_String := "proof";
      Flow_Analysis : constant GNATCOLL.JSON.UTF8_String := "flow analysis";
   begin
      if
        Ensure_Field (Obj   => Timings_Object,
                      Field => Proof,
                      Kind  => GNATCOLL.JSON.JSON_Float_Type) and then
        Ensure_Field (Obj   => Timings_Object,
                      Field => Flow_Analysis,
                      Kind  => GNATCOLL.JSON.JSON_Float_Type)
      then
         This.Timings :=
           Timing_Info'(Duration (Float'(Timings_Object.Get (Field => Proof))),
                        Duration (Float'(Timings_Object.Get (Field => Flow_Analysis))));
      else
         This.Timings := Null_Timing_Info;
      end if;
   end Map_Timings;

   procedure Map_SPARK_Elements (This : in out T;
                                 Root : in     GNATCOLL.JSON.JSON_Array) is
      Length : constant Natural := GNATCOLL.JSON.Length (Root);
   begin
      This.Source_Entity.Reserve_Capacity
        (Capacity => Ada.Containers.Count_Type (Length));

      for I in 1 .. Length loop
         This.Map_Locations (Root => GNATCOLL.JSON.Get (Root, I));
      end loop;
   end Map_SPARK_Elements;

   procedure Map_JSON (This :    out T;
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

      if
        Ensure_Field (Obj   => Root,
                      Field => Spark,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         This.Map_Spark_Elements (Root.Get (Field => Spark));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Flow,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         null;
         --  This.Map_Flow_Elements (Root.Get (Field => Flow));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Proof,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         null;
         --  This.Map_Proof_Elements (Root.Get (Field => Proof));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => Assumptions,
                      Kind  => GNATCOLL.JSON.JSON_Array_Type)
      then
         null;
         --  This.Map_Assumptions_Elements (Root.Get (Field => Assumptions));
      end if;

      if
        Ensure_Field (Obj   => Root,
                      Field => TImings,
                      Kind  => GNATCOLL.JSON.JSON_Object_Type)
      then
         This.Map_Timings (Timings_Object => Root.Get (Field => Timings));
      end if;
   end Map_JSON;

end SPAT.Spark_Info;
