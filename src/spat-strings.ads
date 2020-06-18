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
--  S.P.A.T. - Bounded string list and table support (for output formatting).
--
------------------------------------------------------------------------------

with SPAT.String_Vectors;

package SPAT.Strings is

   package Implementation is

      --  Provide instantiations of the bounded vectors for our different string
      --  types.

      package Subjects is new String_Vectors (Element_Type => Subject_Name,
                                              "="    => "=",
                                              Length => Length);

      package Entities is new String_Vectors (Element_Type => Entity_Name,
                                              "="          => "=",
                                              Length       => Length);

      package SPARK_File_Names is new
        String_Vectors (Element_Type => SPARK_File_Name,
                        "="          => "=",
                        Length       => Length);

   end Implementation;

   --  A one dimensional list of strings.
   type Subject_Names    is new Implementation.Subjects.List   with private;
   type Entity_Names     is new Implementation.Entities.List   with private;
   type SPARK_File_Names is new
     Implementation.SPARK_File_Names.List with private;

   Empty_Subjects : constant Subject_Names;
   Empty_Names    : constant Entity_Names;
   Empty_Files    : constant SPARK_File_Names;

private

   type Subject_Names    is new Implementation.Subjects.List   with null record;
   type Entity_Names     is new Implementation.Entities.List   with null record;
   type SPARK_File_Names is new
     Implementation.SPARK_File_Names.List with null record;

   Empty_Subjects : constant Subject_Names :=
     (Implementation.Subjects.Empty with null record);

   Empty_Names    : constant Entity_Names :=
     (Implementation.Entities.Empty with null record);

   Empty_Files    : constant SPARK_File_Names :=
     (Implementation.SPARK_File_Names.Empty with null record);

end SPAT.Strings;
