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
--  S.P.A.T. - Stopwatch for performance measurements.
--
------------------------------------------------------------------------------

limited private with Ada.Real_Time;

package SPAT.Stop_Watch is

   type T (<>) is tagged limited private;

   ---------------------------------------------------------------------------
   --  Create
   --
   --  Initializes a stop watch instance and returns it.
   ---------------------------------------------------------------------------
   function Create return T;

   ---------------------------------------------------------------------------
   --  Reset
   --
   --  Resets the stop watch.
   ---------------------------------------------------------------------------
   procedure Reset (This : in out T);

   ---------------------------------------------------------------------------
   --  Start
   --
   --  Starts a new lap measurement.
   ---------------------------------------------------------------------------
   procedure Start (This : in out T);

   ---------------------------------------------------------------------------
   --  Elapsed
   --
   --  Returns the time since the last start of measurement.
   ---------------------------------------------------------------------------
   function Elapsed (This : in T) return String;

   ---------------------------------------------------------------------------
   --  Elapsed_Total
   --
   --  Returns the time since the clock was reset.
   ---------------------------------------------------------------------------
   function Elapsed_Total (This : in T) return String;

private

   type T is tagged limited
      record
         Start_Time : Ada.Real_Time.Time;
         Lap_Time   : Ada.Real_Time.Time;
      end record;

end SPAT.Stop_Watch;
