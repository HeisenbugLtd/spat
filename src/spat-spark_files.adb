with Ada.IO_Exceptions;
with Ada.Strings.Unbounded;
with Ada.Text_IO;
with GNATCOLL.JSON;

package body SPAT.Spark_Files is

   procedure Read_Files (This  : in out SPARK_Data;
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
            declare
               File_Content : Ada.Strings.Unbounded.Unbounded_String;
            begin
               Read_File :
               declare
                  JSON_File : Ada.Text_IO.File_Type;
               begin
                  Ada.Text_IO.Open (File => JSON_File,
                                    Mode => Ada.Text_IO.In_File,
                                    Name => Name);

                  while not Ada.Text_IO.End_Of_File (JSON_File) loop
                     Ada.Strings.Unbounded.Append
                       (Source   => File_Content,
                        New_Item => Ada.Text_IO.Get_Line (File => JSON_File));
                  end loop;

                  Ada.Text_IO.Close (File => JSON_File);
               exception
                  when Ada.IO_Exceptions.Name_Error =>
                     Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                                           "Error reading """ & Name & """!");
                     --  TODO: Error management.
               end Read_File;

               This.Insert (Key      => Name,
                            New_Item => GNATColl.JSON.Read (Strm => File_Content));
            end;
         else
            -- Skip file, we already got that one.
            null;
         end if;
      end loop;
   end Read_Files;

end SPAT.Spark_Files;
