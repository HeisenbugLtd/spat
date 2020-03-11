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
--  S.P.A.T. - String list and table support (for output formatting).
--
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Text_IO;

package SPAT.Strings is

   package List_Vectors is new
     Ada.Containers.Vectors (Index_Type   => Positive,
                             Element_Type => Subject_Name,
                             "="          => "=");

   --  A one dimensional list of strings.
   type List is new List_Vectors.Vector with null record;

   ---------------------------------------------------------------------------
   --  Max_Length
   --
   --  Returns the length of the longest string in the list.
   ---------------------------------------------------------------------------
   not overriding
   function Max_Length (Source : in List) return Ada.Text_IO.Count;

end SPAT.Strings;
