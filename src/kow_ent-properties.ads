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



package KOW_Ent.Properties is



	type Valued_Property_Type(
				Name		: Property_Name_Type;
				Container	: Property_Container_Ptr;
				Type_Of		: Type_Of_Data_Type;
				String_Length	: Natural;
				Allow_Null	: Boolean
			) is abstract new KOW_Ent.Property_Type( Name, Container, Allow_Null ) with record
		Value : Value_Type( Type_of, String_Length );
	end record;


	overriding
	function Get_Type ( Property : in Valued_Property_Type ) return Type_Of_Data_Type;
	-- return the type of the value; used for calling set_valued in the right way

	overriding
	function Get_Value(
				Property	: in Valued_Property_Type;
				For_Store	: in Boolean := False
			) return Value_Type;
	-- get the value
	
	overriding
	procedure Set_Value(
				Property	: in out Valued_Property_Type;
				Value		: in     Value_Type
			);


	function To_String( Property : in Valued_Property_Type ) return String;

	procedure From_String( Property : in out Valued_Property_Type; String_Value : in String );


	-------------
	-- Numeric --
	-------------

	type Smallint_Property(
				Name		: Property_Name_Type;
				Container	: KOW_Ent.Property_Container_Ptr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Smallint, 0, Allow_Null ) with null record;
				

	type Integer_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Integer, 0, Allow_Null ) with null record;

	type Bigint_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, COntainer,  APQ_Bigint, 0, Allow_Null ) with null record;


	type Real_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Real, 0, Allow_Null ) with null record;
	
	type Double_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Double, 0, Allow_Null ) with null record;


	type Serial_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Serial, 0, Allow_Null ) with null record;

	type Bigserial_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Bigserial, 0, Allow_Null ) with null record;



	---------------
	-- Date/time --
	---------------

	type Date_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Date, 0, Allow_Null ) with null record;
	
	
	type Time_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Time, 0, Allow_Null ) with null record;

	type Timestamp_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, APQ_Timestamp, 0, Allow_Null ) with null record;


	
	type Hour_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, Hour_Number, 0, Allow_Null ) with null record;
	
	type Minute_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, Minute_Number, 0, Allow_Null ) with null record;

	
	type Second_Property(
				Name		: Property_Name_Type;
				Container	: Property_Container_PTr;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, Second_Number, 0, Allow_Null ) with null record;


	------------
	-- String --
	------------

	type String_Property(
				Name		: Property_Name_Type;
				Container	: KOW_Ent.Property_Container_Ptr;
				String_Length	: Positive;
				Allow_Null	: Boolean
			) is new Valued_Property_Type( Name, Container, KOW_Ent.APQ_String, String_Length, Allow_Null ) with null record;


	

	---------------------
	-- The ID Property --
	---------------------

	type Id_Property(
				Name		: Property_Name_Type;
				Container	: KOW_Ent.Property_Container_Ptr
			) is new Valued_Property_Type( Name, Container, KOW_Ent.APQ_Bigserial, 0, True ) with null record;
	-- allow null is true here because the ID is generated by the data storage
	
	overriding
	function Ignore_For_Insert( Property : in Id_Property ) return Boolean;
	-- don't insert it's value

	overriding
	function Ignore_For_Update( Property : in Id_Property ) return Boolean;
	-- don't update it's value

	overriding
	function Is_Id( Property : in Id_Property ) return Boolean;
	-- return true :)

end KOW_Ent.Properties;
