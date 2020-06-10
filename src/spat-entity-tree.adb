------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Containers.Vectors;

package body SPAT.Entity.Tree is

   package body Generic_Sorting is

      package Cursor_Lists is new
        Ada.Containers.Vectors (Index_Type   => Positive,
                                Element_Type => Cursor,
                                "="          => "=");

      procedure Sort (Tree   : in out T;
                      Parent : in     Cursor) is
         The_List : Cursor_Lists.Vector;
         Num_Children : constant Ada.Containers.Count_Type :=
           Entity.Tree.Child_Count (Parent => Parent);
         use type Ada.Containers.Count_Type;
      begin
         if Num_Children < 2 then
            --  No elements to sort.
            return;
         end if;

         The_List.Reserve_Capacity (Capacity => Num_Children);

         --  Copy the tree's cursor into The_List.
         for C in Tree.Iterate_Children (Parent => Parent) loop
            The_List.Append (New_Item => C);
         end loop;

         --  Sort the list with our tree cursors.
         declare
            function Before (Left  : in Cursor;
                             Right : in Cursor) return Boolean is
              (Before -- from the generic instance
                 (Left  => Entity.Tree.Element (Position => Left),
                  Right => Entity.Tree.Element (Position => Right)));

            package Sorting is new
              Cursor_Lists.Generic_Sorting ("<" => Before);
         begin
            Sorting.Sort (Container => The_List);
         end;

         --  Now rearrange the subtree according to our sorting order.
         for C of The_List loop
            Tree.Splice_Subtree (Parent   => Parent,
                                 Before   => Entity.Tree.No_Element,
                                 Position => C);
         end loop;
      end Sort;

   end Generic_Sorting;

end SPAT.Entity.Tree;
