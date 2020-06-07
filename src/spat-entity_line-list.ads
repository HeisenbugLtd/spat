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
--  S.P.A.T. - A list of Entity_Line objects.
--
------------------------------------------------------------------------------

with Ada.Containers.Vectors;

package SPAT.Entity_Line.List is

   package Implementation is

      package Vectors is
        new Ada.Containers.Vectors (Index_Type   => Positive,
                                    Element_Type => T);

   end Implementation;

   subtype T is Implementation.Vectors.Vector;

end SPAT.Entity_Line.List;
