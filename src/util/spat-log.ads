------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

------------------------------------------------------------------------------
--
--  SPARK Proof Analysis Tool
--
--  S.P.A.T. - Message output utilities
--
------------------------------------------------------------------------------

with Ada.Exceptions;

package SPAT.Log is

   ---------------------------------------------------------------------------
   --  Warning
   --
   --  Print a warning message to Standard_Error.
   ---------------------------------------------------------------------------
   procedure Warning (Message : in String);

   ---------------------------------------------------------------------------
   --  Error
   --
   --  Print a error message to Standard_Error.
   ---------------------------------------------------------------------------
   procedure Error (Message : in String);

   ---------------------------------------------------------------------------
   --  Message
   --
   --  Print a message to Standard_Output.
   ---------------------------------------------------------------------------
   procedure Message (Message  : in String;
                      New_Line : in Boolean := True);

   ---------------------------------------------------------------------------
   --  Debug
   --
   --  Print a debug message to Standard_Output if the command line parameter
   --  --verbose is set.
   ---------------------------------------------------------------------------
   procedure Debug (Message  : in String;
                    New_Line : in Boolean := True);

   ---------------------------------------------------------------------------
   --  Debug_Enabled
   --
   --  Returns True if Debug would output something.
   ---------------------------------------------------------------------------
   function Debug_Enabled return Boolean;

   ---------------------------------------------------------------------------
   --  Dump_Exception
   --
   --  Print the error Message followed by exception information to
   --  Standard_Error.
   --
   --  The (symbolic) stacktrace only works if the binder option "-E" has been
   --  enabled when compiling the program.
   --
   --  The optional parameter File can be used to also print a file name, if
   --  the error may have been related to a file.
   ---------------------------------------------------------------------------
   procedure Dump_Exception (E       : in Ada.Exceptions.Exception_Occurrence;
                             Message : in String;
                             File    : in String := "");

end SPAT.Log;
