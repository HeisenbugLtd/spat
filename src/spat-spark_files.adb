------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;
with Ada.Exceptions;
with Ada.IO_Exceptions;
with Ada.Text_IO;

with SPAT.Log;
with SPAT.Strings;

with System.Multiprocessors;

package body SPAT.Spark_Files is

   ---------------------------------------------------------------------------
   --  Parallelization support.
   --
   --  Reading and parsing the JSON formatted .spark files can easily be
   --  parallelized, there is no dependency between them.
   ---------------------------------------------------------------------------
   package Worker_Tasks is

      --  Just use the number of CPUs on the system.
      Num_Workers : constant System.Multiprocessors.CPU :=
        System.Multiprocessors.Number_Of_CPUs;

      --  The tasks get an input file name and return the read JSON result.
      --  To be able to map the file name and the corresponding result, we
      --  merge them into a single result record type.
      type Parse_Result is
         record
            Key    : SPARK_File_Name;
            Result : GNATCOLL.JSON.Read_Result;
         end record;

      package Implementation is

         --  Needed queue (interface) instantiations for input and output types.

         package File_Queue_Interface is new
           Ada.Containers.Synchronized_Queue_Interfaces
             (Element_Type => SPARK_File_Name);

         package File_Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues
             (Queue_Interfaces => File_Queue_Interface);

         package Result_Queue_Interface is new
           Ada.Containers.Synchronized_Queue_Interfaces
             (Element_Type => Parse_Result);

         package Result_Queues is new
           Ada.Containers.Unbounded_Synchronized_Queues
             (Queue_Interfaces => Result_Queue_Interface);

      end Implementation;

      --  Establish the input queue type.  This one expects a file name to
      --  parse and any instantiated worker tasks will pick it up from there.
      Input_File_Queue : Implementation.File_Queues.Queue;

      --  Establish the output queue type. Here you can pick up the parsing
      --  results.
      Result_Queue     : Implementation.Result_Queues.Queue;

      ------------------------------------------------------------------------
      --  Stop
      --
      --  Tell all worker tasks to terminate.
      ------------------------------------------------------------------------
      procedure Stop;

   end Worker_Tasks;

   package body Worker_Tasks is

      task type Parse_Task;

      type Task_List is
        array (System.Multiprocessors.CPU range <>) of Parse_Task;

      Workers : Task_List (1 .. Num_Workers);

      ------------------------------------------------------------------------
      --  Parse_File
      ------------------------------------------------------------------------
      function Parse_File
        (Name : in SPARK_File_Name) return GNATCOLL.JSON.Read_Result;

      ------------------------------------------------------------------------
      --  Parse_File
      ------------------------------------------------------------------------
      function Parse_File (Name : in SPARK_File_Name)
                           return GNATCOLL.JSON.Read_Result
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

      task body Parse_Task is
         Element : SPARK_File_Name;
         Result  : Worker_Tasks.Parse_Result;
      begin
         Main_Loop :
         loop
            Input_File_Queue.Dequeue (Element => Element);

            Result.Key := Element;

            begin
               Result.Result := Parse_File (Name => Element);
            exception
               when E : Ada.IO_Exceptions.Name_Error =>
                  Result.Result :=
                    GNATCOLL.JSON.Read_Result'
                      (Success => False,
                       Error   =>
                          GNATCOLL.JSON.Parsing_Error'
                            (Line    => 1,
                             Column  => 1,
                             Message =>
                                To_Name (Ada.Exceptions.Exception_Message (E))));
            end;

            Result_Queue.Enqueue (New_Item => Result);
         end loop Main_Loop;
      end Parse_Task;

      ------------------------------------------------------------------------
      --  Stop
      ------------------------------------------------------------------------
      procedure Stop is
      begin
         for Worker of Workers loop
            abort Worker;
         end loop;
      end Stop;

   end Worker_Tasks;

   ---------------------------------------------------------------------------
   --  Num_Workers
   --
   --  Report the number of tasks used for parallel file reads.
   ---------------------------------------------------------------------------
   function Num_Workers return Positive is
     (Positive (Worker_Tasks.Num_Workers));

   Pending : constant GNATCOLL.JSON.Read_Result :=
     GNATCOLL.JSON.Read_Result'
       (Success => False,
        Error   =>
          GNATCOLL.JSON.Parsing_Error'(Line    => Positive'Last,
                                       Column  => Positive'Last,
                                       Message => To_Name ("Queued...")));

   ---------------------------------------------------------------------------
   --  Read
   ---------------------------------------------------------------------------
   not overriding
   procedure Read (This  : in out T;
                   Names : in     Strings.SPARK_File_Names)
   is
      use type File_Maps.Cursor;
   begin
      --  Clear any old remnants and reserve capacity for the number of
      --  files we will be trying to add, so we avoid rehashing during
      --  the loop.
      This.Clear;
      This.Reserve_Capacity (Capacity => Names.Length);

      Run_Parsers :
      declare
         Num_Expected : Natural := 0; --  How many responses to expect.
      begin
         --  Queue input files to the worker tasks.
         for Name of Names loop
            if This.Find (Key => Name) = File_Maps.No_Element then
               This.Insert (Key      => Name,
                            New_Item => Pending);
               --  Establish key/value pair. The actual value will be
               --  established later, but we need the key in the table,
               --  otherwise Find above will never find anything.

               Log.Debug
                 (Message =>
                    "Queuing """ & To_String (Source => Name) & """...");

               Num_Expected := Num_Expected + 1;
               Worker_Tasks.Input_File_Queue.Enqueue (New_Item => Name);
            else
               --  Skip file, we already got that one.
               --
               --  TODO: Normally we shouldn't be too concerned about
               --        duplicates. This check is a leftover from the times
               --        when we still got these values from the command-line
               --        which could have specified the same file more than
               --        once.
               Log.Warning
                 (Message =>
                    "Duplicate file """ & To_String (Source => Name) &
                    """ ignored.");
            end if;
         end loop;

         --  Collect results.
         Collect_Results :
         declare
            Result : Worker_Tasks.Parse_Result;
         begin
            Log.Debug (Message => "Picking up results...");

            for i in 1 .. Num_Expected loop
               Wait_Loop :
               loop
                  select
                     Worker_Tasks.Result_Queue.Dequeue (Element => Result);
                     This.Replace (Key      => Result.Key,
                                   New_Item => Result.Result);
                     Log.Debug
                       (Message =>
                           """" & To_String (Result.Key) & """..." &
                        (if Result.Result.Success
                           then "ok."
                           else "error."));
                     --  Actual error message will be done by caller.
                     exit Wait_Loop;
                  or
                     delay 10.0;

                     --  Inform the user if this is taking way too long...
                     Log.Warning (Message => "Ten seconds have passed.");
                     Log.Warning (Message => "Still waiting for results.");
                     Log.Warning (Message => "This is taking awfully long!?");
                  end select;
               end loop Wait_Loop;
            end loop;
         end Collect_Results;
      end Run_Parsers;
   end Read;

   ---------------------------------------------------------------------------
   --  Shutdown
   ---------------------------------------------------------------------------
   procedure Shutdown is
   begin
      Worker_Tasks.Stop;
   end Shutdown;

end SPAT.Spark_Files;
