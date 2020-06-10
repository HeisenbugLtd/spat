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

limited with Ada.Containers.Bounded_Vectors;
limited with Ada.Text_IO;

package SPAT.Strings is

   package Implementation is

      package Vectors is new
        Ada.Containers.Bounded_Vectors (Index_Type   => Positive,
                                        Element_Type => Subject_Name,
                                        "="          => "=");

   end Implementation;

   --  A one dimensional list of strings.
   type List is new Implementation.Vectors.Vector with private;

   ---------------------------------------------------------------------------
   --  Max_Length
   --
   --  Returns the length of the longest string in the list.
   ---------------------------------------------------------------------------
   not overriding
   function Max_Length (Source : in List) return Ada.Text_IO.Count;

private

   type List is new Implementation.Vectors.Vector with null record;

end SPAT.Strings;
