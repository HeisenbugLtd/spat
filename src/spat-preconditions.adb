with Ada.Text_IO;

package body SPAT.Preconditions is

   --  Make JSON type enumeration literals directly visible.
   use all type JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Ensure_Field
   ---------------------------------------------------------------------------
   function Ensure_Field (Object : in JSON_Value;
                          Field  : in UTF8_String;
                          Kind   : in JSON_Value_Type) return Boolean is
   begin
      if not Object.Has_Field (Field => Field) then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item => "Warning: Expected field """ & Field & """ not present!");

         return False;
      end if;

      if Object.Get (Field => Field).Kind /= Kind then
         Ada.Text_IO.Put_Line
           (File => Ada.Text_IO.Standard_Error,
            Item =>
              "Warning: Field """ & Field & """ not of expected type """ &
              Kind'Image & """!");

         return False;
      end if;

      return True;
   end Ensure_Field;

   ---------------------------------------------------------------------------
   --  Ensure_File_Line
   ---------------------------------------------------------------------------
   function Ensure_File_Line (Object : in JSON_Value) return Boolean is
     (Ensure_Field (Object => Object,
                    Field  => Field_Names.File,
                    Kind   => JSON_String_Type) and then
      Ensure_Field (Object => Object,
                    Field  => Field_Names.Line,
                    Kind   => JSON_Int_Type));

   ---------------------------------------------------------------------------
   --  Ensure_File_Line_Column
   ---------------------------------------------------------------------------
   function Ensure_File_Line_Column (Object : in JSON_Value) return Boolean is
     (Preconditions.Ensure_File_Line (Object => Object) and then
      Preconditions.Ensure_Field (Object   => Object,
                                  Field    => Field_Names.Column,
                                  Kind     => JSON_Int_Type));

   ---------------------------------------------------------------------------
   --  Ensure_Rule_Severity
   ---------------------------------------------------------------------------
   function Ensure_Rule_Severity (Object : in JSON_Value) return Boolean is
     (Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Rule,
                                  Kind   => JSON_String_Type) and then
      Preconditions.Ensure_Field (Object => Object,
                                  Field  => Field_Names.Severity,
                                  Kind   => JSON_String_Type));

end SPAT.Preconditions;
