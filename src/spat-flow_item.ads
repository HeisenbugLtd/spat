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
--  S.P.A.T. - Object representing a JSON "flow" object.
--
------------------------------------------------------------------------------

with SPAT.Entity.Tree;
with SPAT.Entity_Location;
with SPAT.Preconditions;

package SPAT.Flow_Item is

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object : in JSON_Value) return Boolean is
     (Entity_Location.Has_Required_Fields (Object => Object) and
      Preconditions.Ensure_Rule_Severity (Object => Object));

   type T is new Entity_Location.T with private;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding
   function Create (Object : in JSON_Value) return T with
     Pre => Has_Required_Fields (Object => Object);

   ---------------------------------------------------------------------------
   --  Sort_By_Location
   ---------------------------------------------------------------------------
   procedure Sort_By_Location (This   : in out Entity.Tree.T;
                               Parent : in     Entity.Tree.Cursor);

private

   type T is new Entity_Location.T with
      record
         Rule     : Subject_Name;
         Severity : Subject_Name;
      end record;

end SPAT.Flow_Item;
