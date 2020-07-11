------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Stop_Watch is

   use type Ada.Real_Time.Time;

   ---------------------------------------------------------------------------
   --  Image
   ---------------------------------------------------------------------------
   function Image (TS : in Ada.Real_Time.Time_Span) return String is
     (Image (Value => Ada.Real_Time.To_Duration (TS => TS)));

   ---------------------------------------------------------------------------
   --  Create
   ---------------------------------------------------------------------------
   function Create return T is
   begin
      return Result : T do
         Result.Reset;
      end return;
   end Create;

   ---------------------------------------------------------------------------
   --  Elapsed
   ---------------------------------------------------------------------------
   function Elapsed (This : in T) return String is
     (Image (TS => Ada.Real_Time.Clock - This.Lap_Time));

   ---------------------------------------------------------------------------
   --  Elapsed_Total
   ---------------------------------------------------------------------------
   function Elapsed_Total (This : in T) return String is
     (Image (TS => Ada.Real_Time.Clock - This.Start_Time));

   ---------------------------------------------------------------------------
   --  Reset
   ---------------------------------------------------------------------------
   procedure Reset (This : in out T) is
      Now : constant Ada.Real_Time.Time := Ada.Real_Time.Clock;
   begin
      This.Start_Time := Now;
      This.Lap_Time   := Now;
   end Reset;

   ---------------------------------------------------------------------------
   --  Start
   --
   --  Starts a new lap measurement.
   ---------------------------------------------------------------------------
   procedure Start (This : in out T) is
   begin
      This.Lap_Time := Ada.Real_Time.Clock;
   end Start;

end SPAT.Stop_Watch;
