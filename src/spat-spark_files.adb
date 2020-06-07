------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.IO_Exceptions;
with Ada.Text_IO;

with SPAT.File_Lists;
with SPAT.Log;

package body SPAT.Spark_Files is

   ---------------------------------------------------------------------------
   --  Parse_File
   ---------------------------------------------------------------------------
   function Parse_File
     (Name : in Subject_Name) return GNATCOLL.JSON.Read_Result;

   ---------------------------------------------------------------------------
   --  Parse_File
   ---------------------------------------------------------------------------
   function Parse_File (Name : in Subject_Name) return GNATCOLL.JSON.Read_Result
   is
      JSON_File    : Ada.Text_IO.File_Type;
      File_Content : JSON_Data;
   begin
      Ada.Text_IO.Open (File => JSON_File,
                        Mode => Ada.Text_IO.In_File,
                        Name => To_String (Source => Name));

      while not Ada.Text_IO.End_Of_File (File => JSON_File) loop
         Ada.Strings.Unbounded.Append
           (Source   => File_Content,
            New_Item => Ada.Text_IO.Get_Line (File => JSON_File));
      end loop;

      Ada.Text_IO.Close (File => JSON_File);

      return GNATCOLL.JSON.Read (Strm => File_Content);
   end Parse_File;

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   procedure Read (This  : in out T;
                   Names : in     File_Lists.T'Class)
   is
      use type File_Maps.Cursor;
   begin
      --  Clear any old remnants and reserve capacity for the number of
      --  files we will be trying to add, so we avoid rehashing during
      --  the loop.
      This.Clear;
      This.Reserve_Capacity (Capacity => Names.Length);

      for Name of Names loop
         if This.Find (Key => Name) = File_Maps.No_Element then
            Log.Debug
              (Message => "Parsing """ & To_String (Source => Name) & """...");

            begin
               This.Insert (Key      => Name,
                            New_Item => Parse_File (Name => Name));
            exception
               when Ada.IO_Exceptions.Name_Error =>
                  Log.Error
                    (Message =>
                       "Could not read """ & To_String (Source => Name) & """!");
            end;
         else
            --  Skip file, we already got that one.
            null;
         end if;
      end loop;
   end Read;

end SPAT.Spark_Files;
