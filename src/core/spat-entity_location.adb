------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Strings.Fixed;

package body SPAT.Entity_Location is

   ---------------------------------------------------------------------------
   --  "<"
   --
   --  Compares to locations and returns True for the one which comes first,
   --  either by file name, line number and then column number.
   ---------------------------------------------------------------------------
   not overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean is
   begin
      if Left.Source_File = Right.Source_File then
         if Left.Source_Line = Right.Source_Line then
            return Left.Column < Right.Column;
         end if;

         return Left.Source_Line < Right.Source_Line;
      end if;

      return Left.Source_File < Right.Source_File;
   end "<";

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   overriding
   function Create (Object : in JSON_Value) return T is
     (Entity_Line.Create (Object => Object) with
        Column => Object.Get (Field => Field_Names.Column));

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : T) return String is
   begin
      return
        Entity_Line.T (This).Image & ":" &
        Ada.Strings.Fixed.Trim (Source => This.Column'Image,
                                Side   => Ada.Strings.Both);
   end Image;

end SPAT.Entity_Location;
