------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Command_Line is

   ---------------------------------------------------------------------------
   --  Convert
   ---------------------------------------------------------------------------
   function Convert (Value : in String) return Duration is
      Digits_Index : Natural := Value'First;
      Result       : Duration;
   begin
      pragma Assert (Value'Length > 0);
      --  We should not get called at all with an empty string.

      --  We want to support specifications like "1.2ms", or "20s", so to do
      --  that we first extract the first part of the string that seems to be
      --  some kind of a number (i.e. contains digits or dots). We do not
      --  include "-" as a valid character, because a negative cutoff point does
      --  not make sense.
      while
        Digits_Index <= Value'Last and then
        Value (Digits_Index) in '0' .. '9' | '.'
      loop
         Digits_Index := Digits_Index + 1;
      end loop;

      Convert_To_Duration :
      declare
         Number : constant String := Value (Value'First .. Digits_Index - 1);
         Unit   : constant String :=
           (if Digits_Index > Value'Last
            then "" -- empty string
            else Value (Digits_Index .. Value'Last));
      begin
         Result := Duration'Value (Number);

         --  Conversion seems to have been successful, now see the rest of the
         --  string to see if we need to scale it.
         --  For now we only supported "ms" and "s" (which is the default).
         if Unit in "" | "s" then
            null;
         elsif Unit = "ms" then
            Result := Result / 1000.0;
         else
            raise Constraint_Error;
         end if;
      exception
         when others =>
            --  Translate any occuring error into a parse error.
            raise
              GNATCOLL.Opt_Parse.Opt_Parse_Error with
                ("invalid value """ & Value & """, " &
                   "try something like ""1.2ms"" or ""20s"" instead.");
      end Convert_To_Duration;

      return Result;
   end Convert;

end SPAT.Command_Line;
