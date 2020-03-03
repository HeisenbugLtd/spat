------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Spark_Info is

   function Proof_Time (Info : in T) return Duration is
     (Info.Timings.Proof);

   function Flow_Time (Info : in T) return Duration is
     (Info.Timings.Flow);

   procedure Parse_Locations (Info : in out T;
                              Root : in     GNATCOLL.JSON.JSON_Value)
   is
      Name  : constant String                     := Root.Get (Field => "name");
      Slocs : constant GNATCOLL.JSON.JSON_Array   := Root.Get (Field => "sloc");
      C     : constant Source_Entity_Lists.Cursor := Info.Source_Entity.Find (Key => Name);
   begin
      for I in 1 .. GNATCOLL.JSON.Length (Slocs) loop
         declare
            Sloc : GNATCOLL.JSON.JSON_Value := GNATCOLL.JSON.Get (Slocs, I);
            use type Source_Entity_Lists.Cursor;
         begin
            Insert_New_Location :
            declare
               New_Location : constant Line_Location :=
                 Line_Location'(File_Name   => Sloc.Get ("file"),
                                Line_Number => Sloc.Get ("line"));

               procedure Update_Location (Key     : in     String;
                                          Element : in out Source_Entity)
               is
                  pragma Unreferenced (Key);
               begin
                  Element.Locations.Append (New_Item => New_Location);
               end Update_Location;
            begin
               if C = Source_Entity_Lists.No_Element then
                  --  New entry.
                  declare
                     Element : Source_Entity;
                  begin
                     Update_Location (Key     => Name, --  Unused.
                                      Element => Element);
                     Info.Source_Entity.Insert (Key      => Name,
                                                New_Item => Element);
                  end;
               else
                  --  Update element.
                  Info.Source_Entity.Update_Element
                    (Position => C,
                     Process  => Update_Location'Access);
               end if;
            end Insert_New_Location;
         end;
      end loop;
   end Parse_Locations;

   procedure Parse_JSON (This : in out T;
                         Root : in     GNATCOLL.JSON.JSON_Value)
   is
      SPARK   : constant GNATCOLL.JSON.JSON_Array := Root.Get (Field => "spark");
      Timings : constant GNATCOLL.JSON.JSON_Value := Root.Get (Field => "timings");
      Length  : constant Natural                  := GNATCOLL.JSON.Length (SPARK);
   begin
      This.Source_Entity.Clear;
      This.Source_Entity.Reserve_Capacity
        (Capacity => Ada.Containers.Count_Type (Length));

      for S in 1 .. Length loop
         This.Parse_Locations (Root => GNATCOLL.JSON.Get (SPARK, S));
      end loop;

      This.Timings.Proof := Duration (Float'(Timings.Get (Field => "proof")));
      This.Timings.Flow := Duration (Float'(Timings.Get (Field => "flow analysis")));
   end Parse_JSON;

end SPAT.Spark_Info;
