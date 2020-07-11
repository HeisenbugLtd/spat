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

package GNATCOLL.Opt_Parse.Extension is

   --  Option parser which accepts empty arguments.
   generic
      Parser : in out Argument_Parser;
      --  Argument_Parser owning this argument.

      Short : String := "";
      --  Short form for this flag. Should start with one dash and be followed
      --  by one or two alphanumeric characters.

      Long : String;
      --  Long form for this flag. Should start with two dashes.

      Help : String := "";
      --  Help string for the argument.

      type Arg_Type is private;
      --  Type of the option.

      with function Convert (Arg : String) return Arg_Type is <>;
      --  Conversion function to convert from a raw string argument to the
      --  argument type.

      Default_Val : Arg_Type;
      --  Default value if the option is not passed.

      Enabled : Boolean := True;
      --  Whether to add this argument parser

   package Parse_Option_With_Default is
      function Get
        (Args : Parsed_Arguments := No_Parsed_Arguments) return Arg_Type;
   end Parse_Option_With_Default;
   --  Parse a regular option. A regular option is of the form "--option val",
   --  or "--option=val", or "-O val", or "-Oval". If option is not passed,
   --  takes the default value. If no "val" is given for the option, takes the
   --  Default_If_Empty value.

end GNATCOLL.Opt_Parse.Extension;
