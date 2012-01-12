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

with ada.text_io;

package body KOW_Ent_Tests is



	overriding
	procedure Initialize( T : in out Test ) is
	begin
		Set_Name( T, "KOW_Ent" );
		Ahven.Framework.Add_Test_Routine( T, Properties_Test'Access, "Properties" );
		Ahven.Framework.Add_Test_Routine( T, Properties_Stress_Test'Access, "Properties Stress" );
		Ahven.Framework.Add_Test_Routine( T, Stress_Control'Access, "Stress Control" );
	end Initialize;



	My_Smallint_Name: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_smallint" );
	My_Int_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_int" );
	My_Bigint_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_bigint" );
	My_Real_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_real" );
	My_Double_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_double" );
	My_Serial_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_serial" );
	My_Bigserial_Name:constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_bigserial" );

	-- TODO :: date, time and timestamp

	My_Hour_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_hour" );
	My_Minute_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_minute" );
	My_Second_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_second" );

	My_String_Name	: constant KOW_Ent.Property_Name_Type := KOW_Ent.PN( "my_string" );



	-- test the core funcionality of properties -- how they hold value and the sorts
	type My_Container is new KOW_Ent.Property_Container_Type with record
		My_Smallint	: KOW_Ent.Properties.Smallint_Property( My_Smallint_Name, My_Container'Unrestricted_Access );
		My_Int		: KOW_Ent.Properties.Integer_Property( My_Int_Name, My_Container'Unrestricted_Access );
		My_Bigint	: KOW_Ent.Properties.Bigint_Property( My_Bigint_Name, My_Container'Unrestricted_Access );
		My_Real		: KOW_Ent.Properties.Real_Property( My_Real_Name, My_Container'Unrestricted_Access );
		My_Double	: KOW_Ent.Properties.Double_Property( My_Double_Name, My_Container'Unrestricted_Access );
		My_Serial	: KOW_Ent.Properties.Serial_Property( My_Serial_Name, My_Container'Unrestricted_Access );
		My_Bigserial	: KOW_Ent.Properties.Bigserial_Property( My_Bigserial_Name, My_Container'Unrestricted_Access );

		-- TODO :: date, time and timestamp
		

		My_Hour		: KOW_Ent.Properties.Hour_Property( My_Hour_Name, My_Container'Unrestricted_Access );
		My_Minute	: KOW_Ent.Properties.Minute_Property( My_Minute_Name, My_Container'Unrestricted_Access );
		My_Second	: KOW_Ent.Properties.Second_Property( My_Second_Name, My_Container'Unrestricted_Access );

		My_String	: KOW_Ent.Properties.String_Property( My_String_Name, My_Container'Unrestricted_Access, 2 );
	end record;




	procedure Properties_Test is

		Registered : Boolean := False;

		procedure Iterator( P : KOW_Ent.Property_Ptr ) is
			use APQ;
			use KOW_Ent;
		begin
			Registered := True;

			case Get_Type( P.all ) is
				when KOW_Ent.APQ_Smallint =>
					Ahven.Assert( Get_Value( P.all ).Smallint_Value = 1, "smallint not holding values" );
					Ahven.Assert( P.Name.all = My_Smallint_Name.all, "smallint not holding property name" );

				when KOW_Ent.APQ_Integer =>
					Ahven.Assert( Get_Value( P.all ).Integer_Value = 1, "integer not holding values" );
					Ahven.Assert( P.Name.all = My_Int_Name.all, "integer not holding property name" );

				when KOW_Ent.APQ_Bigint =>
					Ahven.Assert( Get_Value( P.all ).Bigint_Value = 1, "bigint not holding values" );
					Ahven.Assert( P.Name.all = My_Bigint_Name.all, "bigint not holding property name" );

				when KOW_Ent.APQ_Real =>
					Ahven.Assert( Get_Value( P.all ).Real_Value = 20.0, "real not holding values" );
					Ahven.Assert( P.Name.all = My_Real_Name.all, "real not holding property name" );

				when KOW_Ent.APQ_Double =>
					Ahven.Assert( Get_Value( P.all ).Double_Value = 20.0, "double not holding values" );
					Ahven.Assert( P.Name.all = My_Double_Name.all, "double not holding property name" );

				when KOW_Ent.APQ_Serial =>
					Ahven.Assert( Get_Value( P.all ).Serial_Value = 1, "serial not holding values" );
					Ahven.Assert( P.Name.all = My_Serial_Name.all, "serial not holding property name" );

				when KOW_Ent.APQ_Bigserial =>
					Ahven.Assert( Get_Value( P.all ).Bigserial_Value = 1, "bigserial not holding values" );
					Ahven.Assert( P.Name.all = My_Bigserial_Name.all, "bigserial not holding property name" );



				-- TODO :: implement those:
				when KOW_Ent.APQ_Date =>
					raise program_error with "not implemented";
				when KOW_Ent.APQ_Time =>
					raise program_error with "not implemented";
				when KOW_Ent.APQ_Timestamp =>
					raise program_error with "not implemented";


				
				when KOW_Ent.Hour_Number =>
					Ahven.Assert( Get_Value( P.all ).Hour_Value = 1, "hour not holding values" );
					Ahven.Assert( P.Name.all = My_Hour_Name.all, "hour not holding property name" );

				when KOW_Ent.Minute_Number =>
					Ahven.Assert( Get_Value( P.all ).Minute_Value = 1, "minute not holding values" );
					Ahven.Assert( P.Name.all = My_Minute_Name.all, "minute not holding property name" );

				when KOW_Ent.Second_Number =>
					Ahven.Assert( Get_Value( P.all ).Second_Value = 1, "second not holding values" );
					Ahven.Assert( P.Name.all = My_Second_Name.all, "second not holding property name" );




				when KOW_Ent.APQ_String =>
					Ahven.Assert( Get_Value( P.all ).String_Value = "A ", "string not holding values" );
					Ahven.Assert( P.Name.all = My_String_Name.all, "string not holding property name" );
			end case;
		end Iterator;

		My : My_Container;
	begin
		My.My_Smallint.Value.Smallint_Value := 1;
		My.My_Int.Value.Integer_Value := 1;
		My.My_Bigint.Value.Bigint_Value := 1;
		My.My_Real.Value.Real_Value := 20.0;
		My.My_Double.Value.Double_Value := 20.0;
		My.My_Serial.Value.Serial_Value := 1;
		My.My_Bigserial.Value.Bigserial_Value := 1;


		-- TODO: date, time and timestamp

		My.My_Hour.Value.Hour_Value := 1;
		My.My_Minute.Value.Minute_Value := 1;
		My.My_Second.Value.Second_Value := 1;


		KOW_Ent.From_String( My.My_String.Value, "A" );

		Iterate( My, Iterator'Access );


		if not Registered then
			Ahven.Assert( false, "types not registering" );
		end if;

	end Properties_Test;



	procedure Properties_Stress_Test is
	begin
		for i in 1 .. 1_000_000 loop
			Properties_Test;
		end loop;
	end Properties_Stress_Test;



	procedure Stress_Control is
		a : integer;
	begin
		for i in 1 .. 1_000_000 loop
			a := i;
		end loop;
	end Stress_Control;

end KOW_Ent_Tests;
