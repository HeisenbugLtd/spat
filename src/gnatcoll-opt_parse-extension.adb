------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2009-2019, AdaCore                     --
--                     Copyright (C) 2020, Heisenbug Ltd.                   --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Characters.Handling;

package body GNATCOLL.Opt_Parse.Extension is

   function "+" (Self : in String) return XString renames To_XString;

   function "+" (Self : in XString) return String renames To_String;

   function Parse_One_Option
     (Short, Long : in     String;
      Args        : in     XString_Array;
      Pos         : in     Positive;
      New_Pos     :    out Parser_Return) return XString;

   ---------------------------------------------------------------------------
   --  Parse_One_Option
   ---------------------------------------------------------------------------
   function Parse_One_Option (Short   : in     String;
                              Long    : in     String;
                              Args    : in     XString_Array;
                              Pos     : in     Positive;
                              New_Pos :    out Parser_Return) return XString is
   begin
      if
        Args (Pos) = Long
        or else (Short /= "" and then Args (Pos) = Short)
      then
         if Pos + 1 > Args'Last or else Args (Pos + 1).Starts_With ("-") then
            --  No more arguments or already next option.
            New_Pos := Pos + 1;
            return Null_XString;
         end if;

         New_Pos := Pos + 2;
         return Args (Pos + 1);

      elsif Args (Pos).Starts_With (Long & "=") then
         New_Pos := Pos + 1;
         return Args (Pos).Slice (Long'Last + 2, Args (Pos).Length);

      elsif Short /= "" and then Args (Pos).Starts_With (Short) then
         New_Pos := Pos + 1;
         return Args (Pos).Slice (Short'Last + 1, Args (Pos).Length);

      else
         New_Pos := Error_Return;
         return +"";
      end if;
   end Parse_One_Option;

   package body Parse_Option_With_Default is

      type Option_Parser is new GNATCOLL.Opt_Parse.Parser_Type with
        null record;

      overriding
      function Usage (Self : Option_Parser) return String is
        ("[" & Long & (if Short = "" then "" else "|" & Short) & " "
         & Ada.Characters.Handling.To_Upper (Long (3 .. Long'Last)) & "]");

      overriding
      function Help_Name (Dummy : Option_Parser) return String is
        (Long & ", " & Short);

      overriding
      function Parse_Args
        (Self   : in out Option_Parser;
         Args   : in     XString_Array;
         Pos    : in     Positive;
         Result : in out Parsed_Arguments) return Parser_Return;

      type Internal_Result is new Parser_Result with
         record
            Result : Arg_Type;
         end record;

      type Internal_Result_Access is access all Internal_Result;

      overriding
      procedure Release (Self : in out Internal_Result) is null;

      Self_Val : aliased Option_Parser :=
        Option_Parser'(Name     => +Long (3 .. Long'Last),
                       Help     => +Help,
                       Parser   => Parser.Data,
                       Opt      => True,
                       Position => <>);

      Self : constant Parser_Access := Self_Val'Unchecked_Access;

      ------------------------------------------------------------------------
      --  Get
      ------------------------------------------------------------------------
      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type is
      begin
         if not Enabled then
            return Default_Val;
         end if;

         declare
            R : constant Parser_Result_Access := Self.Get_Result (Args);
         begin
            if R /= null then
               return Internal_Result (R.all).Result;
            else
               return Default_Val;
            end if;
         end;
      end Get;

      ------------------------------------------------------------------------
      --  Parse_Args
      ------------------------------------------------------------------------
      overriding
      function Parse_Args
        (Self   : in out Option_Parser;
         Args   : in     XString_Array;
         Pos    : in     Positive;
         Result : in out Parsed_Arguments) return Parser_Return
      is
         New_Pos : Parser_Return;
         Raw     : constant XString :=
           Parse_One_Option (Short, Long, Args, Pos, New_Pos);
      begin
         if New_Pos /= Error_Return then
            declare
               Res : constant Internal_Result_Access :=
                 new Internal_Result'(Start_Pos => Pos,
                                      End_Pos   => Pos,
                                      Result    => Convert (+Raw));
            begin
               Result.Ref.Get.Results (Self.Position) :=
                  Res.all'Unchecked_Access;
            end;
         end if;

         return New_Pos;
      end Parse_Args;

   begin
      if Enabled then
         Parser.Data.Opts_Parsers.Append (Self);
         Parser.Data.All_Parsers.Append (Self);
         Self.Position := Parser.Data.All_Parsers.Last_Index;
      end if;
   end Parse_Option_With_Default;

end GNATCOLL.Opt_Parse.Extension;
