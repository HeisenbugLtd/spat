------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Strings is

   ---------------------------------------------------------------------------
   --  Max_Length
   --
   --  Returns the length of the longest string in the array.
   ---------------------------------------------------------------------------
   not overriding
   function Max_Length (Source : in List) return Ada.Text_IO.Count is
      Result : Ada.Text_IO.Count := 0;
   begin
      for S of Source loop
         Result :=
           Ada.Text_IO.Count'Max (Result,
                                  Ada.Text_IO.Count (Length (Source => S)));
      end loop;

      return Result;
   end Max_Length;

end SPAT.Strings;
