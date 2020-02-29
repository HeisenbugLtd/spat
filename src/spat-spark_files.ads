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
--  S.P.A.T. - Read .spark files
--
--  Collect file contents.
--
------------------------------------------------------------------------------
with SPAT.File_Ops;

with Ada.Containers.Indefinite_Hashed_Maps;
with Ada.Strings.Hash;
with GNATCOLL.JSON;

package SPAT.Spark_Files is

   package File_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => String, --  file name
      Element_Type    => GNATCOLL.JSON.Read_Result,
      Hash            => Ada.Strings.Hash,
      Equivalent_Keys => Standard."=",
      "="             => GNATCOLL.JSON."=");

   No_Element : File_Maps.Cursor renames File_Maps.No_Element;
   subtype Cursor is File_Maps.Cursor;

   type SPARK_Data is new File_Maps.Map with private;
   --  Stores all data collected from SPARK files for analysis.

   procedure Read_Files (This  : in out SPARK_Data;
                         Names : in     File_Ops.File_List'Class);
   --  Reads the list of files, and parses and stores their content in
   --  This.
   --  TODO: Error management/reporting.

   function Element (C : in Cursor) return GNATCOLL.JSON.Read_Result
                     renames File_Maps.Element;
   function Key (C : in Cursor) return String renames File_Maps.Key;
   function Next (C : in Cursor) return Cursor renames File_Maps.Next;
   procedure Next (C : in out Cursor) renames File_Maps.Next;

private

   type SPARK_Data is new File_Maps.Map with null record;

end SPAT.Spark_Files;
