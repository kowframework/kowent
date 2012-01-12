------------------------------------------------------------------------------
--                                                                          --
--                       KOW Framework :: Entities                          --
--                                                                          --
--                              KOW Framework                               --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--               Copyright (C) 2007-2011, KOW Framework Project             --
--                                                                          --
--                                                                          --
-- KOWLib is free software; you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. KOWLib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License distributed with KOWLib; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- Main property types for KOW Ent                                          --
------------------------------------------------------------------------------



-----------
-- Ahven --
-----------
with Ahven.Framework;


-------------
-- KOW Ent --
-------------
with KOW_Ent;
with KOW_Ent.Properties;


package KOW_Ent_Tests is

	overriding
	procedure Initialize( T : in out Test ) is
	begin
		Set_Name( T, "KOW_Ent" );
		Ahven.Framework.Add_Test_Routine( T, Properties_Test'Access, "Properties" );
	end Initialize;

	procedure Properties_Test is
		-- test the core funcionality of properties -- how they hold value and the sorts
		type My_Container is new KOW_Ent.Entity_Container_Type with record
			My_Int : KOW_Ent.Properties.Integer_Property( "my_int", My_Container'Unrestricted_Access );
			My_Real : KOW_Ent.Properties.Real_Property( "my_real", My_Container'Unrestricted_Access );
		end record;


		procedure Iterator( P : KOW_Ent.Property_Type_Ptr ) is
			use KOW_Ent;
		begin
			if P.Value_Of = APQ_Integer then
				Ahven.Assert( P.Value.Integer_Value = 1, "integer not holding values" );
			elsif P.Value_Of = APQ_Real then
				Ahven.Assert( P.Value.Real_Value = 20.0, "real not holding values" );
			else
				Ahven.Assert( false, "Not the right value_of" );
			end if;
		end Iterator;
	begin
		My_Container.My_Int.Value.Integer_Value := 1;
		My_Container.My_Real.Value.Real_Value := 20.0;

		Iterate( My_Container, Iterator'Access );

	end Properties_Test;

end KOW_Ent_Tests;
