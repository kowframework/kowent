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
with APQ;
with KOW_Ent;
with KOW_Ent.Properties;


package body KOW_Ent_Tests is



	overriding
	procedure Initialize( T : in out Test ) is
	begin
		Set_Name( T, "KOW_Ent" );
		Ahven.Framework.Add_Test_Routine( T, Properties_Test'Access, "Properties" );
		Ahven.Framework.Add_Test_Routine( T, Properties_Stress_Test'Access, "Properties Stress" );
		Ahven.Framework.Add_Test_Routine( T, Stress_Control'Access, "Stress Control" );
	end Initialize;



	type Column_Name is access String;
	function CN( Name : in String ) return Column_Name is
	begin
		return new String'( Name );
	end CN;

	My_Int_Name	: constant Column_Name := CN( "my_int" );
	My_Real_Name	: constant Column_Name := CN( "my_real" );
	My_String_Name	: constant Column_Name := CN( "my_string" );



	-- test the core funcionality of properties -- how they hold value and the sorts
	type My_Container is new KOW_Ent.Property_Container_Type with record
		My_Int		: KOW_Ent.Properties.Integer_Property( My_Int_Name, My_Container'Unrestricted_Access );
		My_Real		: KOW_Ent.Properties.Real_Property( My_Real_Name, My_Container'Unrestricted_Access );
		My_String	: KOW_Ent.Properties.String_Property( My_String_Name, My_Container'Unrestricted_Access, 2 );
	end record;




	procedure Properties_Test is

		Registered : Boolean := False;

		procedure Iterator( P : KOW_Ent.Property_Ptr ) is
			use APQ;
			use KOW_Ent;
		begin
			Registered := True;
			if P.Value_Of = KOW_Ent.APQ_Integer then
				Ahven.Assert( P.Value.Integer_Value = 1, "integer not holding values" );
				Ahven.Assert( P.Name.all = My_Int_Name.all, "integer not holding property name" );

			elsif P.Value_Of = KOW_Ent.APQ_Real then
				Ahven.Assert( P.Value.Real_Value = 20.0, "real not holding values" );
				Ahven.Assert( P.Name.all = My_Real_Name.all, "real not holding property name" );

			elsif P.Value_Of = KOW_Ent.APQ_String then
				Ahven.Assert( P.Value.String_Value = "A ", "string not holding values" );
				Ahven.Assert( P.Name.all = My_String_Name.all, "string not holding property name" );
			else
				Ahven.Assert( false, "not the right value_of" );
			end if;
		end Iterator;

		My : My_Container;
	begin
		My.My_Int.Value.Integer_Value := 1;
		My.My_Real.Value.Real_Value := 20.0;
		KOW_Ent.From_String( My.My_String.Value, "A" );

		Iterate( My, Iterator'Access );


		if not Registered then
			Ahven.Assert( false, "types not registering" );
		end if;

	end Properties_Test;



	procedure Properties_Stress_Test is
		task type runner;

		task body runner is
		begin
			for i in 1 .. 10_000 loop
				Properties_Test;
			end loop;
		end runner;


		type runner_ptr is access runner;
		type runner_arr is array(integer range <> ) of runner_ptr;

		r : runner_arr( 1 .. 50 );
	begin
		for i in r'range loop
			r(i) := new runner;
			delay 0.2;
		end loop;
	end Properties_Stress_Test;



	procedure Stress_Control is
		task type runner;

		task body runner is
			a : integer;
		begin
			for i in 1 .. 10_000 loop
				a := i;
			end loop;
		end runner;


		type runner_ptr is access runner;
		type runner_arr is array(integer range <> ) of runner_ptr;

		r : runner_arr( 1 .. 50 );
	begin
		for i in r'range loop
			r(i) := new runner;
			delay 0.2;
		end loop;
	end Stress_Control;

end KOW_Ent_Tests;