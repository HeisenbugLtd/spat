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

with Ada.Containers.Hashed_Maps;

package SPAT.Spark_Files is

   --  .spark files are stored in a Hash map with the file name as key and the
   --  JSON reading result as value.
   package File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => File_Name,
      Element_Type    => GNATCOLL.JSON.Read_Result,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.JSON."=");

   --
   --  Some renames for commonly used File_Maps.Cursor operations.
   --
   No_Element : File_Maps.Cursor renames File_Maps.No_Element;
   subtype Cursor is File_Maps.Cursor;

   function Key (C : in Cursor) return File_Name renames File_Maps.Key;

   --
   --  Data collection and operations defined on it.
   --

   type SPARK_Data is new File_Maps.Map with private;
   --  Stores all data collected from SPARK files for analysis.

   procedure Read_Files (This  : in out SPARK_Data;
                         Names : in     File_Ops.File_List'Class);
   --  Reads the list of files, and parses and stores their content in This.

private

   type SPARK_Data is new File_Maps.Map with null record;

end SPAT.Spark_Files;
