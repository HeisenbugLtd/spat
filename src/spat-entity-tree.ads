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
--  S.P.A.T. - A tree holding descendant objects of Entity.T.
--
------------------------------------------------------------------------------

with Ada.Containers.Indefinite_Multiway_Trees;

package SPAT.Entity.Tree is

   package Implementation is

      package Trees is new
        Ada.Containers.Indefinite_Multiway_Trees (Element_Type => T'Class);

   end Implementation;

   type T is new Implementation.Trees.Tree with private;

private

   type T is new Implementation.Trees.Tree with null record;

end SPAT.Entity.Tree;
