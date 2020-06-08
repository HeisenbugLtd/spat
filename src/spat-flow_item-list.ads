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
--  S.P.A.T. - Object representing a list of JSON "flow" objects.
--
------------------------------------------------------------------------------

limited with Ada.Containers.Vectors;

private package SPAT.Flow_Item.List is

   package Implementation is

      package Vectors is
        new Ada.Containers.Vectors (Index_Type   => Positive,
                                    Element_Type => T);

   end Implementation;

   type T is new Implementation.Vectors.Vector with private;

   ---------------------------------------------------------------------------
   --  Sort_By_Location
   ---------------------------------------------------------------------------
   not overriding
   procedure Sort_By_Location (This : in out T);

private

   type T is new Implementation.Vectors.Vector with null record;

end SPAT.Flow_Item.List;
