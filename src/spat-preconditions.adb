------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with SPAT.Field_Names;
with SPAT.Log;

package body SPAT.Preconditions is

   ---------------------------------------------------------------------------
   --  Ensure_Field
   ---------------------------------------------------------------------------
   function Ensure_Field (Object      : in JSON_Value;
                          Field       : in UTF8_String;
                          Kind        : in JSON_Value_Type;
                          Is_Optional : in Boolean := False) return Boolean is
   begin
      if not Object.Has_Field (Field => Field) then
         if not Is_Optional then
            Log.Warning
              (Message => "Expected field """ & Field & """ not present!");
         end if;

         return False;
      end if;

      if Object.Get (Field => Field).Kind /= Kind then
         if not Is_Optional then
            Log.Warning
              (Message =>
                 "Field """ & Field & """ not of expected type """ & Kind'Image &
                 """!");
         end if;

         return False;
      end if;

      return True;
   end Ensure_Field;

   ---------------------------------------------------------------------------
   --  Ensure_Field
   ---------------------------------------------------------------------------
   function Ensure_Field
     (Object        : in JSON_Value;
      Field         : in UTF8_String;
      Kinds_Allowed : in Accepted_Value_Types) return Boolean is
   begin
      if not Object.Has_Field (Field => Field) then
         Log.Warning
           (Message => "Expected field """ & Field & """ not present!");

         return False;
      end if;

      declare
         Field_Kind : constant JSON_Value_Type :=
                        Object.Get (Field => Field).Kind;
      begin
         if not Kinds_Allowed (Field_Kind) then
            Log.Warning
              (Message =>
                 "Field """ & Field & """ has unacceptable kind """ &
                 Field_Kind'Image & """!");

            return False;
         end if;
      end;

      return True;
   end Ensure_Field;

   ---------------------------------------------------------------------------
   --  Ensure_Rule_Severity
   ---------------------------------------------------------------------------
   function Ensure_Rule_Severity (Object : in JSON_Value) return Boolean is
     (Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Rule,
                                  Kind   => JSON_String_Type) and
      Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Severity,
                                  Kind   => JSON_String_Type));

end SPAT.Preconditions;
