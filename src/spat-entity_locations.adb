------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Strings.Fixed;

package body SPAT.Entity_Locations is

   ---------------------------------------------------------------------------
   --  "<"
   --
   --  Compares to locations and returns True for the one which comes first,
   --  either by file name, line number and then column number.
   ---------------------------------------------------------------------------
   function "<" (Left  : in T;
                 Right : in T) return Boolean is
   begin
      if Left.File = Right.File then
         if Left.Line = Right.Line then
            return Left.Column < Right.Column;
         end if;

         return Left.Line < Right.Line;
      end if;

      return Left.File < Right.File;
   end "<";

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding function Create (Object : in JSON_Value) return T is
     (Entity_Lines.Create (Object => Object) with
        Column => Object.Get (Field => Field_Names.Column));

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding function Image (This : T) return String is
   begin
      return
        Entity_Lines.T (This).Image & ":" &
        Ada.Strings.Fixed.Trim (Source => This.Column'Image,
                                Side   => Ada.Strings.Both);
   end Image;

end SPAT.Entity_Locations;
