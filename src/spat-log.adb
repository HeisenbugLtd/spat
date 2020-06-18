------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Text_IO;

with GNAT.Traceback.Symbolic;
with GNATCOLL.Opt_Parse;
with SPAT.Command_Line;

package body SPAT.Log is

   --  Verbose option (debug output).
   package Verbose is new
     GNATCOLL.Opt_Parse.Parse_Flag (Parser => SPAT.Command_Line.Parser,
                                    Short  => "-v",
                                    Long   => "--verbose",
                                    Help   => "Verbose (tracing) output");

   ---------------------------------------------------------------------------
   --  Debug
   ---------------------------------------------------------------------------
   procedure Debug (Message  : in String;
                    New_Line : in Boolean := True) is
   begin
      if Verbose.Get then
         Log.Message (Message  => "Debug: " & Message,
                      New_Line => New_Line);
      end if;
   end Debug;

   ---------------------------------------------------------------------------
   --  Debug_Enabled
   --
   --  Returns True if Debug would output something.
   ---------------------------------------------------------------------------
   function Debug_Enabled return Boolean is
     (Verbose.Get);

   ---------------------------------------------------------------------------
   --  Dump_Exception
   ---------------------------------------------------------------------------
   procedure Dump_Exception (E       : in Ada.Exceptions.Exception_Occurrence;
                             Message : in String;
                             File    : in String := "") is
   begin
      Error (Message => Message);
      Error (Message => "Please file a bug report.");

      if File'Length > 0 then
         Error (Message => "If possible, include the file ");
         Error (Message => """" & File & """");
         Error (Message => "and the following stack trace in your report.");
      else
         Error
           (Message =>
              "Please include the following stack trace in your report.");
      end if;

      Error (Message => Ada.Exceptions.Exception_Information (X => E));

      --  This only works, if the binder is invoked with the "-E" option.
      Error (Message => GNAT.Traceback.Symbolic.Symbolic_Traceback (E => E));
   end Dump_Exception;

   ---------------------------------------------------------------------------
   --  Error
   ---------------------------------------------------------------------------
   procedure Error (Message : in String) is
   begin
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => "Error: " & Message);
   end Error;

   ---------------------------------------------------------------------------
   --  Message
   ---------------------------------------------------------------------------
   procedure Message (Message  : in String;
                      New_Line : in Boolean := True) is
   begin
      Ada.Text_IO.Put (File => Ada.Text_IO.Standard_Output,
                       Item => Message);

      if New_Line then
         Ada.Text_IO.New_Line (File => Ada.Text_IO.Standard_Output);
      end if;
   end Message;

   ---------------------------------------------------------------------------
   --  Warning
   ---------------------------------------------------------------------------
   procedure Warning (Message : in String) is
   begin
      Log.Message (Message => "Warning: " & Message);
   end Warning;

end SPAT.Log;
