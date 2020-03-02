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
--  S.P.A.T. - Main program
--
------------------------------------------------------------------------------

with Ada.Command_Line;
with Ada.Directories;
with Ada.Real_Time;
with Ada.Strings.Unbounded;
with Ada.Text_IO;

with GNATCOLL.Opt_Parse;
with SI_Units.Metric;
with SI_Units.Names;
with SPAT.File_Ops;
with SPAT.Spark_Files;

procedure Run_SPAT is

   function Image is new
     SI_Units.Metric.Fixed_Image (Item        => Duration,
                                  Default_Aft => 3,
                                  Unit        => SI_Units.Names.Second);

   package CL_Args is

      Parser : GNATCOLL.Opt_Parse.Argument_Parser :=
        GNATCOLL.Opt_Parse.Create_Argument_Parser (Help         => "",
                                                   Command_Name => "run_spat");

      package Dir_Args is new
        GNATCOLL.Opt_Parse.Parse_Positional_Arg_List
          (Parser      => Parser,
           Name        => "directory",
           Help        => "directory to look for .spark files in",
           Allow_Empty => False,
           Arg_Type    => Ada.Strings.Unbounded.Unbounded_String,
           Convert     => Ada.Strings.Unbounded.To_Unbounded_String);

      package Verbose_Flag is new
        GNATCOLL.Opt_Parse.Parse_Flag (Parser => Parser,
                                       Short  => "-v",
                                       Long   => "--verbose",
                                       Help   => "verbose output");

   end CL_Args;

   use type Ada.Real_Time.Time;
begin
   if CL_Args.Parser.Parse then
      declare
         File_List  : SPAT.File_Ops.File_List;
         SPARK_Data : SPAT.Spark_Files.SPARK_Data;
         Start_Time : Ada.Real_Time.Time;
         Verbose    : constant Boolean := CL_Args.Verbose_Flag.Get;
      begin
         for Dir of CL_Args.Dir_Args.Get loop
            declare
               Search_Dir : constant String := Ada.Strings.Unbounded.To_String (Dir);
            begin
               if Verbose then
                  Ada.Text_IO.Put ("Searching for "".spark"" files in """ & Search_Dir & """...");
                  Start_Time := Ada.Real_Time.Clock;
               end if;

               File_List.Add_Files (Directory => Search_Dir,
                                    Extension => "spark");

               if Verbose then
                  Ada.Text_IO.Put_Line
                    ("completed in " &
                       Image (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Start_Time)) &
                       "," & File_List.Length'Image & " files found.");
               end if;
            exception
               when Ada.Directories.Name_Error =>
                  Ada.Text_IO.Put_Line
                    (File => Ada.Text_IO.Standard_Error,
                     Item => "Directory """ & Search_Dir & """ not found!");
            end;
         end loop;

         if not File_List.Is_Empty then
            if Verbose then
               Ada.Text_IO.Put ("Parsing "".spark"" files...");
               Start_Time := Ada.Real_Time.Clock;
            end if;

            SPARK_Data.Read_Files (Names => File_List);

            if Verbose then
               Ada.Text_IO.Put_Line
                 ("completed in " &
                    Image (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Start_Time)) & ".");
            end if;
         end if;

         declare
            C : SPAT.Spark_Files.Cursor := SPARK_Data.First;
            use type SPAT.SPARK_Files.Cursor;
         begin
            while C /= SPAT.Spark_Files.No_Element loop
               Ada.Text_IO.Put_Line ("""" & SPAT.Spark_Files.Key (C) & """ => " &
                                       SPAT.SPARK_Files.Element (C).Success'Image);

               SPAT.Spark_Files.Next (C);
            end loop;
         end;
      end;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   else
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   end if;
end Run_SPAT;
