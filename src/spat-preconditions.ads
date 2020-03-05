with GNATCOLL.JSON;

private package SPAT.Preconditions is

   ---------------------------------------------------------------------------
   --  Ensure_Field
   --
   --  Check that the given JSON object contains an object named Field with
   --  type of Kind.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Field (Object : in JSON_Value;
                          Field  : in UTF8_String;
                          Kind   : in JSON_Value_Type) return Boolean;

   ---------------------------------------------------------------------------
   --  Ensure_File_Line
   --
   --  Checks that the given JSON object contains name and line indicating a
   --  source code position (i.e. line of declaration of an entity).
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_File_Line (Object : in JSON_Value) return Boolean;

   ---------------------------------------------------------------------------
   --  Ensure_File_Line_Column
   --
   --  Checks that the given JSON object contains name, line and column values
   --  indicating a source code position.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_File_Line_Column (Object : in JSON_Value) return Boolean;

   ---------------------------------------------------------------------------
   --  Ensure_Rule_Severity
   --
   --  Checks that the given JSON object contains a rule and a severity object.
   --  Returns True if so, False otherwise.
   ---------------------------------------------------------------------------
   function Ensure_Rule_Severity (Object : in JSON_Value) return Boolean;

end SPAT.Preconditions;
