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
--  Please note that Read is parallelized and uses background threads, hence
--  you need to call Shutdown once you're done, otherwise the application will
--  hang.
--
------------------------------------------------------------------------------

limited with Ada.Containers.Hashed_Maps;
limited with SPAT.Strings;

package SPAT.Spark_Files is

   --  .spark files are stored in a Hash map with the file name as key and the
   --  JSON reading result as value.
   package File_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => SPARK_File_Name,
      Element_Type    => GNATCOLL.JSON.Read_Result,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => GNATCOLL.JSON."=");

   --
   --  Some renames for commonly used File_Maps.Cursor operations.
   --
   No_Element : File_Maps.Cursor renames File_Maps.No_Element;
   subtype Cursor is File_Maps.Cursor;

   ---------------------------------------------------------------------------
   --  Key
   ---------------------------------------------------------------------------
   function Key (C : in Cursor) return SPARK_File_Name renames File_Maps.Key;

   --
   --  Data collection and operations defined on it.
   --

   type T is new File_Maps.Map with private;
   --  Stores all data collected from SPARK files for analysis.

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   not overriding
   procedure Read (This  : in out T;
                   Names : in     Strings.SPARK_File_Names);
   --  Reads the list of files, and parses and stores their content in This.

   ---------------------------------------------------------------------------
   --  Num_Workers
   --
   --  Report the number of tasks used for parallel file reads.
   ---------------------------------------------------------------------------
   function Num_Workers return Positive;

   ---------------------------------------------------------------------------
   --  Shutdown
   --
   --  Terminates all worker tasks.
   ---------------------------------------------------------------------------
   procedure Shutdown;

private

   type T is new File_Maps.Map with null record;

end SPAT.Spark_Files;
