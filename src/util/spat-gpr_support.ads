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
--  S.P.A.T. - GNAT project file (.gpr) support.
--
------------------------------------------------------------------------------

limited with Ada.Containers.Hashed_Maps;
limited with GNATCOLL.VFS;

package SPAT.GPR_Support is

   package SPARK_Source_Maps is new
     Ada.Containers.Hashed_Maps (Key_Type        => SPARK_File_Name,
                                 Element_Type    => Source_File_Name,
                                 Hash            => SPAT.Hash,
                                 Equivalent_Keys => "=",
                                 "="             => "=");

   ---------------------------------------------------------------------------
   --  Get_SPARK_Files
   --
   --  Retrieve all (existing) .spark files from the project.
   ---------------------------------------------------------------------------
   function Get_SPARK_Files
     (GPR_File : GNATCOLL.VFS.Filesystem_String) return SPARK_Source_Maps.Map;

end SPAT.GPR_Support;
