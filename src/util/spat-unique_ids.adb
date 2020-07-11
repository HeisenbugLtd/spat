------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Unique_Ids is

   Current_Id : aliased Id := Id'First; --  Start at zero.

   ---------------------------------------------------------------------------
   --  Next
   ---------------------------------------------------------------------------
   function Next return Id is
     (GNATCOLL.Atomic.Sync_Add_And_Fetch (Ptr   => Current_Id'Access,
                                          Value => 1));

end SPAT.Unique_Ids;
