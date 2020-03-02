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
--  S.P.A.T. - File Operations
--
--  Collect file names for analysis.
--
------------------------------------------------------------------------------
with Ada.Containers.Indefinite_Vectors;

package SPAT.File_Ops is

   package File_Lists is
     new Ada.Containers.Indefinite_Vectors (Index_Type   => Positive,
                                            Element_Type => String);

   type File_List is new File_Lists.Vector with private;

   not overriding procedure Add_Files
     (This      : in out File_List;
      Directory : in     String;
      Extension : in     String := "spark");
   --  Recursively searches for files given in the given directory with the
   --  given extension and stores them in This.
   --
   --  TODO: Accept a list of directories to search in.

private

   type File_List is new File_Lists.Vector with null record;

end SPAT.File_Ops;
