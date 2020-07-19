------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with Ada.Strings.Fixed;
with SI_Units.Metric;
with SI_Units.Names;

with SPAT.Command_Line;

package body SPAT is

   ---------------------------------------------------------------------------
   --  Nice_Image
   ---------------------------------------------------------------------------
   function Nice_Image is new
     SI_Units.Metric.Fixed_Image (Item        => Duration,
                                  Default_Aft => 0,
                                  Unit        => SI_Units.Names.Second);

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   function Image (Value : in Duration) return String is
     (if SPAT.Command_Line.Raw_Mode.Get
      then Value'Image
      else Nice_Image (Value => Value));

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   function Image (Value : in Duration;
                   Steps : in Prover_Steps) return String is
     (Image (Value => Value) & " (" &
      Ada.Strings.Fixed.Trim (Source => Steps'Image,
                              Side   => Ada.Strings.Both) & " step" &
      (if Steps /= 1 then "s)" else ")"));

   ---------------------------------------------------------------------------
   --  Scaled
   --
   --  See https://github.com/AdaCore/why3/blob/master/src/gnat/gnat_config.ml#L538
   ---------------------------------------------------------------------------
   function Scaled (Prover    : in Prover_Name;
                    Raw_Steps : in Prover_Steps) return Prover_Steps is
   begin
      --  Especially with Z3, negative steps indicate a proof failure (i.e. out
      --  of memory situation etc.), so if the input number is negative leave it
      --  as is.  At least I find it counterintuitive, that -1 would be
      --  converted to +1 instead.
      if Raw_Steps < 0 then
         return Raw_Steps;
      end if;

      if
        Ada.Strings.Unbounded.Index (Source  => Subject_Name (Prover),
                                     Pattern => "CVC4") = 1
      then
         --  add = 15_000, mult = 35
         return Prover_Steps'Max (Raw_Steps - 15_000, 0) / 35 + 1;
      end if;

      if
        Ada.Strings.Unbounded.Index (Source => Subject_Name (Prover),
                                     Pattern => "Z3") = 1
      then
         --  add = 450_000, mult = 800
         return Prover_Steps'Max (Raw_Steps - 450_000, 0) / 800 + 1;
      end if;

      --  alt-ergo, and others => no scaling
      return Raw_Steps + 1;
   end Scaled;

end SPAT;
