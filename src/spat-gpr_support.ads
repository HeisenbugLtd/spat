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
limited with GNATCOLL.VFS;
limited with SPAT.File_Ops;

package SPAT.GPR_Support is

   ---------------------------------------------------------------------------
   --  Get_SPARK_Files
   --
   --  Retrieve all (existing) .spark files from the project.
   ---------------------------------------------------------------------------
   function Get_SPARK_Files
     (GPR_File : GNATCOLL.VFS.Filesystem_String) return File_Ops.File_List;

end SPAT.GPR_Support;
