------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with SPAT.Field_Names;
with SPAT.Flow_Item.List;

package body SPAT.Flow_Item is

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding function Create (Object : in JSON_Value) return T is
     (Entity_Location.Create (Object => Object) with
        Rule     => Object.Get (Field => Field_Names.Rule),
        Severity => Object.Get (Field => Field_Names.Severity));

   ---------------------------------------------------------------------------
   --  Sort_By_Location
   ---------------------------------------------------------------------------
   procedure Sort_By_Location (This   : in out Entity.Tree.T;
                               Parent : in     Entity.Tree.Cursor) is
      The_List : List.T;
      use type Entity.Tree.Cursor;
   begin
      if Parent = Entity.Tree.No_Element then
         --  No elements to sort.
         --  TODO: If we have only one item in the tree, we should probably
         --        bail out, too, because then there's nothing to sort.
         return;
      end if;

      --  Copy the tree's children into The_List.
      for C in This.Iterate_Children (Parent => Parent) loop
         The_List.Append
           (New_Item => Flow_Item.T (Entity.Tree.Element (Position => C)));
         --  If the children do not contain elements of Flow_Item.T then this
         --  will raise an exception.
      end loop;

      --  Sort the list.
      The_List.Sort_By_Location;

      --  Update the elements in the Tree.
      --  FIXME: For now this works, because the structure here is just a list,
      --         if it were an actual tree we would mix up parent and children.
      declare
         Position : Entity.Tree.Cursor :=
           Entity.Tree.First_Child (Position => Parent);
      begin
         for E of The_List loop
            This.Replace_Element (Position => Position,
                                  New_Item => E);
            Entity.Tree.Next_Sibling (Position);
         end loop;

         --  At this point we should have no children left.
         pragma Assert (Position = Entity.Tree.No_Element);
      end;

   end Sort_By_Location;

end SPAT.Flow_Item;
