------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Directories;
with Ada.Text_IO;

with SPAT.Command_Line;

package body SPAT.File_Ops is

   Filter : constant Ada.Directories.Filter_Type
     := (Ada.Directories.Directory     => True,   --  Recursive, so include directories.
         Ada.Directories.Ordinary_File => True,   --  These we want.
         Ada.Directories.Special_File  => False); --  Nope.

   not overriding procedure Add_Files (This      : in out File_List;
                                       Directory : in     String;
                                       Extension : in     String := "spark")
   is
      procedure Handle_Entry (Item : in Ada.Directories.Directory_Entry_Type);

      procedure Handle_Entry (Item : in Ada.Directories.Directory_Entry_Type)
      is
         Full_Name : constant String :=
                       Ada.Directories.Full_Name (Directory_Entry => Item);
         Simple_Name : constant String :=
                         Ada.Directories.Simple_Name (Name => Full_Name);
      begin
         case Ada.Directories.Kind (Directory_Entry => Item) is

            when Ada.Directories.Directory =>
               --  Don't search again in current or parent directory.
               if Simple_Name not in "" | "." | ".." then
                  --  Current entry is a directory, start a new search in there.
                  This.Add_Files (Directory => Full_Name,
                                  Extension => Extension);
                  --  FIXME: Recursive call.  'Extension' never changes,
                  --         yet it will be provided on the stack for each
                  --         recursion step.
               end if;

            when Ada.Directories.Ordinary_File =>
               --  Current entry is a file, check if it matches our extension.
               --  If so, add it to our file list.
               if Ada.Directories.Extension (Name => Full_Name) = Extension then
                  This.Append (New_Item => Full_Name);
               end if;

            when Ada.Directories.Special_File =>
               --  Current entry is a special file. This shouldn't happen, as
               --  the initial Filter argument is supposed to filter those out.
               raise Program_Error;

         end case;
      end Handle_Entry;
   begin
      if SPAT.Command_Line.Verbose.Get then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Output,
            Item => "Searching for "".spark"" files in """ & Directory & """...");
      end if;

      --  Search for our files...
      Ada.Directories.Search (Directory => Directory,
                              Pattern   => "",
                              Filter    => Filter,
                              Process   => Handle_Entry'Access);
   exception
      when Ada.Directories.Use_Error =>
         Ada.Text_IO.Put_Line
           ("warning: Unable to read directory """ & Directory & """.");
   end Add_Files;

end SPAT.File_Ops;
