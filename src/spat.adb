------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

with SI_Units.Metric;
with SI_Units.Names;

with SPAT.Command_Line;

package body SPAT is

   function Nice_Image is new
     SI_Units.Metric.Fixed_Image (Item        => Duration,
                                  Default_Aft => 0,
                                  Unit        => SI_Units.Names.Second);

   function Image (Value : in Duration) return String is
     (if SPAT.Command_Line.Raw_Mode.Get
      then Value'Image
      else Nice_Image (Value => Value));

end SPAT;
