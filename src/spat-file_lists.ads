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
--  S.P.A.T. - Declares an object containing a list of files.
--
------------------------------------------------------------------------------

with Ada.Containers.Vectors;

package SPAT.File_Lists is

   package Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Subject_Name);

   type T is new Vectors.Vector with private;

private

   type T is new Vectors.Vector with null record;

end SPAT.File_Lists;
