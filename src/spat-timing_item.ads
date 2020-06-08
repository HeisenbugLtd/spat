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
--  S.P.A.T. - Object representing the "timing" JSON object.
--
------------------------------------------------------------------------------

with SPAT.Entity;
with SPAT.Field_Names;
with SPAT.Preconditions;

package SPAT.Timing_Item is

   use all type GNATCOLL.JSON.JSON_Value_Type;

   ---------------------------------------------------------------------------
   --  Has_Required_Fields
   ---------------------------------------------------------------------------
   function Has_Required_Fields (Object  : in JSON_Value;
                                 Version : in File_Version) return Boolean
   is
     ((case Version is
          when GNAT_CE_2019 =>
             Preconditions.Ensure_Field (Object => Object,
                                         Field  => Field_Names.Proof,
                                         Kind   => JSON_Float_Type),
       --  Here, we don't really check for required fields anymore, there
       --  is a whole bunch of items starting with "gnatwhy3.". If they are not
       --  present that means the "corresponding phase wasn't run at all" (@kanigsson)
       --  and we just assume 0.0 s.
          when GNAT_CE_2020 =>
             True) and
       Preconditions.Ensure_Field (Object => Object,
                                   Field  => Field_Names.Flow_Analysis,
                                   Kind   => JSON_Float_Type));

   --  Information obtained from the timing section of a .spark file.
   type T is new Entity.T with private;

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create (Object  : in JSON_Value;
                    Version : in File_Version) return T with
     Pre => Has_Required_Fields (Object  => Object,
                                 Version => Version);

   ---------------------------------------------------------------------------
   --  Flow
   ---------------------------------------------------------------------------
   function Flow (This : in T) return Duration;

   ---------------------------------------------------------------------------
   --  Proof
   ---------------------------------------------------------------------------
   function Proof (This : in T) return Duration;

   ---------------------------------------------------------------------------
   --  Version
   ---------------------------------------------------------------------------
   function Version (This : in T) return File_Version;

   None : constant T;

private

   type T is new Entity.T with
      record
         Version : File_Version; --  version of file encountered.
         Proof   : Duration;     --  Total time the prover spent.
         Flow    : Duration;     --  Total time of flow analysis.
      end record;

   None : constant T := T'(Entity.T with
                             Version => File_Version'First,
                             Proof   => 0.0,
                             Flow    => 0.0);

   ---------------------------------------------------------------------------
   --  Flow
   ---------------------------------------------------------------------------
   function Flow (This : in T) return Duration is
     (This.Flow);

   ---------------------------------------------------------------------------
   --  Proof
   ---------------------------------------------------------------------------
   function Proof (This : in T) return Duration is
     (This.Proof);

   ---------------------------------------------------------------------------
   --  Version
   ---------------------------------------------------------------------------
   function Version (This : in T) return File_Version is
     (This.Version);

end SPAT.Timing_Item;
