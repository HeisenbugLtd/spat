------------------------------------------------------------------------------
--  Copyright (C) 2020 by Heisenbug Ltd. (gh+spat@heisenbug.eu)
--
--  This work is free. You can redistribute it and/or modify it under the
--  terms of the Do What The Fuck You Want To Public License, Version 2,
--  as published by Sam Hocevar. See the LICENSE file for more details.
------------------------------------------------------------------------------
pragma License (Unrestricted);

package body SPAT.Proof_Attempt.List is

   ---------------------------------------------------------------------------
   --  "<"
   ---------------------------------------------------------------------------
   not overriding
   function "<" (Left  : in T;
                 Right : in T) return Boolean
   is
      Left_Time  : Duration := 0.0;
      Right_Time : Duration := 0.0;
      --  FIXME: Proof_Attempts should have a field storing the max/accumulated
      --         time directly, so we don't need to recalculate it each time.
   begin
      for A of Left loop
         Left_Time := Left_Time + A.Time;
      end loop;

      for A of Right loop
         Right_Time := Right_Time + A.Time;
      end loop;

      return Left_Time > Right_Time;
   end "<";

   ---------------------------------------------------------------------------
   --  Has_Failed_Attempts
   ---------------------------------------------------------------------------
   not overriding
   function Has_Failed_Attempts (This : in T) return Boolean is
     (for some A of This => A.Result /= "Valid");

   ---------------------------------------------------------------------------
   --  Is_Unproved
   ---------------------------------------------------------------------------
   not overriding
   function Is_Unproved (This : in T) return Boolean is
     (for all A of This => A.Result /= "Valid");

   package By_Duration is new
     Implementation.Vectors.Generic_Sorting ("<" => "<");

   ---------------------------------------------------------------------------
   --  Sort_By_Duration
   ---------------------------------------------------------------------------
   not overriding
   procedure Sort_By_Duration (Container : in out T) is
   begin
      By_Duration.Sort (Implementation.Vectors.Vector (Container));
   end Sort_By_Duration;

end SPAT.Proof_Attempt.List;
