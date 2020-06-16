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
--  S.P.A.T. - Provide atomic ids (counters) to facilitate stable sorting.
--             Each instantiation provides its own local counter.
--
------------------------------------------------------------------------------

with GNATCOLL.Atomic;

generic

package SPAT.Unique_Ids is

   --  Sometimes entries are identical which makes sorting unstable. To
   --  counter the issue we add a unique id to each entry which serves as a
   --  last ditch sorting criterion, making two entries always distinct.
   --  CAREFUL: This approach only works if the order of elements being
   --           inserted does not change between runs (I'm thinking further
   --           parallelization here). But to make sure this works in a
   --           tasking context anyway we use atomic increments to generate
   --           our ids.
   --           Luckily GNATCOLL already serves us, so we don't need to wrap
   --           it into our own protected type (inefficient) or revert to
   --           compiler/target specific intrinsics.

   subtype Id is GNATCOLL.Atomic.Atomic_Counter;
   --  Our id type.

   --------------------------------------------------------------------------
   --  Next
   --
   --  Returns the next available id.
   --  Id is a modular type, so it wraps around instead of overflow, but we
   --  should never be able to exhaust an Id's range, anyway.
   --------------------------------------------------------------------------
   function Next return Id with
     Volatile_Function => True;

end SPAT.Unique_Ids;
