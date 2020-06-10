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
--  S.P.A.T. - Abstract object representing some kind of Entity.
--
------------------------------------------------------------------------------

package SPAT.Entity is

   type T is abstract tagged private;

   ---------------------------------------------------------------------------
   --  Image
   --
   --  Supposed to return the text (human readable) form of an entity.
   ---------------------------------------------------------------------------
   function Image (This : in T) return String is abstract;

private

   type T is abstract tagged null record;

end SPAT.Entity;
