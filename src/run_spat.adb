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
with Ada.IO_Exceptions;
with Ada.Real_Time;
with Ada.Text_IO;

with SI_Units.Metric;
with SI_Units.Names;
with SPAT.File_Ops;
with SPAT.Spark_Files;

procedure Run_SPAT is

   function Image is new
     SI_Units.Metric.Fixed_Image (Item        => Duration,
                                  Default_Aft => 3,
                                  Unit        => SI_Units.Names.Second);

   File_List  : SPAT.File_Ops.File_List;
   SPARK_Data : SPAT.Spark_Files.SPARK_Data;
   Start_Time : Ada.Real_Time.Time;

   use type Ada.Real_Time.Time;
begin
   --  TODO: Proper command line argument handling.
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line
        ("Syntax: "
         & Ada.Directories.Simple_Name (Name => Ada.Command_Line.Command_Name)
         & " <directory>");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      declare
         Search_Dir : constant String := Ada.Command_Line.Argument (1);
      begin
         Ada.Text_IO.Put ("Searching for "".spark"" files...");
         Start_Time := Ada.Real_Time.Clock;
         File_List.Add_Files (Directory => Search_Dir,
                              Extension => "spark");
         Ada.Text_IO.Put_Line
           ("completed in " &
              Image (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Start_Time)));

         Ada.Text_IO.Put ("Parsing "".spark"" files...");
         Start_Time := Ada.Real_Time.Clock;
         SPARK_Data.Read_Files (Names => File_List);
         Ada.Text_IO.Put_Line
           ("completed in " &
              Image (Ada.Real_Time.To_Duration (Ada.Real_Time.Clock - Start_Time)));
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Ada.Text_IO.Put_Line
              (File => Ada.Text_IO.Standard_Error,
               Item => "Directory """ & Search_Dir & """ not found!");
      end;

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

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;
end Run_SPAT;
