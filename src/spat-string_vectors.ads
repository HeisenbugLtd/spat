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
--  S.P.A.T. - Utility generic package to instantiate type safe string lists.
--
------------------------------------------------------------------------------

with Ada.Containers.Bounded_Vectors;
with Ada.Text_IO;

generic
   type Element_Type is private;

   with function "=" (Left  : in Element_Type;
                      Right : in Element_Type) return Boolean is <>;

   with function Length (Source : in Element_Type) return Natural is <>;
package SPAT.String_Vectors is

   package Base_Vectors is new
     Ada.Containers.Bounded_Vectors (Index_Type   => Positive,
                                     Element_Type => Element_Type,
                                     "="          => "=");

   type List is new Base_Vectors.Vector with private;

   Empty : constant List;

   ---------------------------------------------------------------------
   --  Max_Length
   --
   --  Returns the length of the longest string in the list.
   ---------------------------------------------------------------------
   not overriding
   function Max_Length (Source : in List) return Ada.Text_IO.Count;

private

   type List is new Base_Vectors.Vector with null record;

   Empty : constant List := (Base_Vectors.Empty_Vector with null record);

end SPAT.String_Vectors;
