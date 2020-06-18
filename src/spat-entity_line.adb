------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Strings.Fixed;

package body SPAT.Entity_Line is

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   not overriding
   function Create (Object : in JSON_Value) return T is
     (T'(Entity.T with
           File =>
             Source_File_Name
               (Subject_Name'(Object.Get (Field => Field_Names.File))),
           Line => Object.Get (Field => Field_Names.Line)));

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   overriding
   function Image (This : T) return String is
   begin
      return
        To_String (This.File) & ":" &
        Ada.Strings.Fixed.Trim (Source => This.Line'Image,
                                Side   => Ada.Strings.Both);
   end Image;

end SPAT.Entity_Line;
