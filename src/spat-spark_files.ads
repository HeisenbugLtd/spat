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

with Ada.Containers.Hashed_Maps;
with SPAT.File_Lists;

package SPAT.Spark_Files is

   --  .spark files are stored in a Hash map with the file name as key and the
   --  JSON reading result as value.
   package File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Subject_Name,
      Element_Type    => GNATCOLL.JSON.Read_Result,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.JSON."=");

   --
   --  Some renames for commonly used File_Maps.Cursor operations.
   --
   No_Element : File_Maps.Cursor renames File_Maps.No_Element;
   subtype Cursor is File_Maps.Cursor;

   function Key (C : in Cursor) return Subject_Name renames File_Maps.Key;

   --
   --  Data collection and operations defined on it.
   --

   type T is new File_Maps.Map with private;
   --  Stores all data collected from SPARK files for analysis.

   procedure Read (This  : in out T;
                   Names : in     File_Lists.T'Class);
   --  Reads the list of files, and parses and stores their content in This.

private

   type T is new File_Maps.Map with null record;

end SPAT.Spark_Files;
