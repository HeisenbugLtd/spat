------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.String_Tables is

   type Lengths is array (1 .. Columns) of Ada.Text_IO.Count;
   type Column_Positions is array (1 .. Columns) of Ada.Text_IO.Positive_Count;

   ---------------------------------------------------------------------------
   --  Starts_With_Ignorable_Character
   --
   --  Very special handling of continuation lines.  If a string starts with
   --  one of the ignorable characters it is not considered to be counted in
   --  the column width.
   ---------------------------------------------------------------------------
   function Starts_With_Ignorable_Character
     (Source : in Ada.Strings.Unbounded.Unbounded_String) return Boolean;

   ---------------------------------------------------------------------------
   --  Put
   ---------------------------------------------------------------------------
   procedure Put
     (File : in Ada.Text_IO.File_Type := Ada.Text_IO.Standard_Output;
      Item : in Row_Vectors.Vector)
   is
      Col_Length : Lengths          := (others => 0);
      Start_Col  : Column_Positions := (others => 1);
      use type Ada.Text_IO.Count;
   begin
      --  Retrieve maximum length of each column.
      for Row of Item loop
         for Col in 1 .. Columns loop
            Col_Length (Col) :=
              Ada.Text_IO.Count'Max
                (Col_Length (Col),
                 (if Starts_With_Ignorable_Character (Source => Row (Col))
                  then 0 -- Ignore lines starting with special characters.
                  else Ada.Text_IO.Count (Ada.Strings.Unbounded.Length (Row (Col)))));
         end loop;
      end loop;

      --  Now each Length entry contains the maximum length of each item.
      --  Let's turn these length into absolute positions. We assume one space
      --  character between each column.
      for Col in Col_Length'Range loop
         Start_Col (Col) :=
           (if Col = Col_Length'First
            then 1
            else 1 + Start_Col (Col - 1) + Col_Length (Col - 1));
      end loop;

      --  Finally print each entry.
      for Row of Item loop
         for Col in Row'Range loop
            if Ada.Strings.Unbounded.Length (Row (Col)) > 0 then
               Ada.Text_IO.Set_Col (File => File,
                                    To   => Start_Col (Col));
               Ada.Text_IO.Put
                 (File => File,
                  Item => Ada.Strings.Unbounded.To_String (Row (Col)));
            end if;
         end loop;

         Ada.Text_IO.New_Line (File => File);
      end loop;
   end Put;

   ---------------------------------------------------------------------------
   --  Starts_With_Ignorable_Character
   ---------------------------------------------------------------------------
   function Starts_With_Ignorable_Character
     (Source : in Ada.Strings.Unbounded.Unbounded_String) return Boolean
   is
   begin
      if Ada.Strings.Unbounded.Length (Source => Source) = 0 then
         return True;
         --  Minor optimization to avoid an additional call to Length.
      end if;

      return
        Ada.Strings.Maps.Is_In
          (Element => Ada.Strings.Unbounded.Element (Source => Source,
                                                     Index  => 1),
           Set     => Ignorable_Characters);
   end Starts_With_Ignorable_Character;

end SPAT.String_Tables;
