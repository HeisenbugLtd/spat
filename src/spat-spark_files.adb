with Ada.IO_Exceptions;
with Ada.Text_IO;
with SPAT.Command_Line;

package body SPAT.Spark_Files is

   function Parse_File
     (Name : in Subject_Name) return GNATCOLL.JSON.Read_Result;

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

   procedure Read (This  : in out T;
                   Names : in     File_Ops.File_List'Class)
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
            if SPAT.Command_Line.Verbose.Get then
               Ada.Text_IO.Put_Line
                 (File => Ada.Text_IO.Standard_Output,
                  Item => "Parsing """ & To_String (Source => Name) & """...");
            end if;

            begin
               This.Insert (Key      => Name,
                            New_Item => Parse_File (Name => Name));
            exception
               when Ada.IO_Exceptions.Name_Error =>
                  Ada.Text_IO.Put_Line
                    (File => Ada.Text_IO.Standard_Error,
                     Item =>
                        "Error reading """ & To_String (Source => Name) &
                        """!");
            end;
         else
            --  Skip file, we already got that one.
            null;
         end if;
      end loop;
   end Read;

end SPAT.Spark_Files;
