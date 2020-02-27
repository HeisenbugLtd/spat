with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;

with SPAT.File_Ops;

procedure Run_SPAT is
   File_List : SPAT.File_Ops.File_List;
begin
   --  TODO: Proper command line argument handling.
   if Ada.Command_Line.Argument_Count /= 1 then
      Ada.Text_IO.Put_Line
        ("Syntax: "
         & Ada.Directories.Simple_Name (Name => Ada.Command_Line.Command_Name)
         & " <directory>");
      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
   else
      File_List.Add_Files (Directory => Ada.Command_Line.Argument (1),
                           Extension => "spark");

      for File of File_List loop
         Ada.Text_IO.Put_Line (File);
      end loop;

      Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Success);
   end if;
end Run_SPAT;
