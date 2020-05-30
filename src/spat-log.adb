------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Text_IO;

with SPAT.Command_Line;

package body SPAT.Log is

   ---------------------------------------------------------------------------
   --  Debug
   --
   --  Print a debug message to Standard_Output if the command line parameter
   --  --verbose is set.
   ---------------------------------------------------------------------------
   procedure Debug (Message  : in String;
                    New_Line : in Boolean := True) is
   begin
      if Command_Line.Verbose.Get then
         Log.Message (Message  => "Debug: " & Message,
                      New_Line => New_Line);
      end if;
   end Debug;

   ---------------------------------------------------------------------------
   --  Error
   --
   --  Print a error message to Standard_Error.
   ---------------------------------------------------------------------------
   procedure Error (Message : in String) is
   begin
      Ada.Text_IO.Put_Line (File => Ada.Text_IO.Standard_Error,
                            Item => "Error: " & Message);
   end Error;

   ---------------------------------------------------------------------------
   --  Message
   --
   --  Print a message to Standard_Output.
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
   --
   --  Print a warning message to Standard_Error.
   ---------------------------------------------------------------------------
   procedure Warning (Message : in String) is
   begin
      Log.Message (Message => "Warning: " & Message);
   end Warning;

end SPAT.Log;
