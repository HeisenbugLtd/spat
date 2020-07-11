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
--  S.P.A.T. - String tables
--
--  Supports table like output of strings
--
------------------------------------------------------------------------------

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

generic
   Columns : in Positive;
package SPAT.String_Tables is

   type Row is array (1 .. Columns) of Ada.Strings.Unbounded.Unbounded_String;

   package Row_Vectors is new Ada.Containers.Vectors (Index_Type   => Positive,
                                                      Element_Type => Row,
                                                      "="          => "=");

   ---------------------------------------------------------------------------
   --  Put
   --
   --  Print given table as formatted output into given text file.
   ---------------------------------------------------------------------------
   procedure Put (File : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
                  Item : in Row_Vectors.Vector);

end SPAT.String_Tables;
