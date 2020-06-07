------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Flow_Item.List is

   package By_Location is new
     Implementation.Vectors.Generic_Sorting ("<" => "<");

   ---------------------------------------------------------------------------
   --  Sort_By_Location
   ---------------------------------------------------------------------------
   not overriding
   procedure Sort_By_Location (This : in out T) is
   begin
      By_Location.Sort (Container => Implementation.Vectors.Vector (This));
   end Sort_By_Location;

end SPAT.Flow_Item.List;
