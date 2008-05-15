------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Library                            --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--         Copyright (C) 2007-2008, Ydea Desenv. de Softwares Ltda          --
--                                                                          --
--                                                                          --
-- Aw_Ent is free library;  you can redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. Aw_Lib is distributed in the hope that it will be useful, but WITH---
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with Aw_Lib; see file COPYING. If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- This is the Aw_Ent package                                               --
--                                                                          --
-- Main package for Aw_Ent.                                                 --
--                                                                          --
-- IT'S STILL ONLY FOR PLANNING PURPOSE! DON'T TRUST IT! IT LIES!           --
-- TODO:                                                                    --
-- 	. better filter for data                                            --
-- 	. entity relations                                                  --
-- 	. auto-generation of keys                                           --
------------------------------------------------------------------------------


with APQ;	use APQ;


package Aw_Ent is



	type Supported_Types is (
		-- scalar types
		Type_APQ_Smallint,
		Type_APQ_Integer,
		Type_APQ_Bigint,
		Type_APQ_Real,
		Type_APQ_Double,
		Type_APQ_Serial,
		Type_APQ_Bigserial,

		-- time types
		Type_APQ_Date,
		Type_APQ_Time,
		Type_APQ_Timestamp,
		Type_APQ_Timezone,

		-- other types..
		Type_APQ_Boolean,
		Type_APQ_Bitstring,
		Type_String		-- APQ doesn't got his own type for strings.
					-- hence the name isn't preffixed with APQ
		);


	type Value_Type( Contains: Supported_Types ) is private;
	-- The type Value_Type is used to stored all information in a HashMap.

	type Value_Type_Access is access all Value_Type;
	-- this access type is used inside the map.

	------------------------
	-- CONVERSION METHODS --
	------------------------
	
	-- The following methods should be used to convert from your values
	-- to Aw_Ent's values. These values are then mapped into a
	--
	-- Hash_Map which is used by Aw_Ent to iterate with the database.

	-- scalar types
	function Get_Value( Value: APQ_Smallint ) return Value_Type_Access;
	function Get_Value( Value: APQ_Integer ) return Value_Type_Access;
	function Get_Value( Value: APQ_Bigint ) return Value_Type_Access;
	function Get_Value( Value: APQ_Real ) return Value_Type_Access;
	function Get_Value( Value: APQ_Double ) return Value_Type_Access;
	function Get_Value( Value: APQ_Serial ) return Value_Type_Access;
	function Get_Value( Value: APQ_Bigserial ) return Value_Type_Access;

	-- time types 
	function Get_Value( Value: APQ_Date ) return Value_Type_Access;
	function Get_Value( Value: APQ_Time ) return Value_Type_Access;
	function Get_Value( Value: APQ_Timestamp ) return Value_Type_Access;
	function Get_Value( Value: APQ_Timezone ) return Value_Type_Access;


	-- other types
	function Get_Value( Value: APQ_Boolean ) return Value_Type_Access;
	function Get_Value( Value: APQ_Bitstring ) return Value_Type_Access;
	function Get_Value( Value: String ) return Value_Type_Access;
	function Get_Value( Value: Unbounded_String ) return Value_Type_Access;



	package Valued_Maps is new Ada.Containers.Hash_Maps(
			Key_Type => Unbounded_String,
			Element_Type => Value_Type_Access );

	procedure Append(
		Map: in out Valued_Maps.Map;
		Key: in String;
		Element: Value_Type_Access );
	-- this method is provided to easy the development, as it's not expected
	-- to have the field names parammetrized.


	---------------------------------
	-- The entity interface itself --
	---------------------------------

	type Entity is abstract tagged private;
	-- Every entity should implement the methods declared here.
	--
	-- Then the user will have authomatically some methods for
	-- data iteration and retrieval.
	--


	function Get_Keys( E: in Entity )
		return Valued_Maps.Map is abstract;
	procedure Set_Keys( E: in out Entity; Values: Valued_Maps.Map );


	function Get_Values( E: in Entity_Inteface )
		return Valueed_Maps.Map is abstract;
	procedure Set_Values( E: in out Entity; Values: Valued_Maps.Map );


	
	function Get_Name( E: in Entity_Interface ) return Name_Type;
	-- Aw_Ent will use an active APQ connection and will expect the tables
	--are named using the same name as the implementing type in UPERCASE.
	-- Follow this rules and you'll be fine.
	--
	-- Optionally, if you _NEED_ to implement something at your own, you can
	-- do it by overriding this method.
	--
	-- That's where the table name is defined.


	-------------------------------------------
	-- Entity Management Dispatching Methods --
	-------------------------------------------
	
	procedure Load(	Entity_Object	: in out Entity'Class;
			Keys		: in Valued_Maps.Map );
	-- load an entity using it's own allocated object
	
	procedure Store( Entity_Object : in Entity'Class );
	-- save an already created entity in the backend
	
	procedure Insert( Entity_Object: in out Entity'CLass );
	-- insert a new value into the database backend.
	-- Notice the entity is an "in out" parameter.
	-- It's so it can be possible to Aw_Ent to determine auto-generated
	-- keys in the entity.
	


private


	type APQ_Bitstring_Access is access APQ_Bitstring;
	-- this type is created so, internally, we can treat bitstring as it
	-- was a constrained type.

	type Value_Type( Contains: Supported_Types ) is record
		case Contains is
			-- scalar types
			when Type_APQ_Smallint	=>
				APQ_Smallint_Value : APQ_Smallint;

			when Type_APQ_Integer	=>
				APQ_Integer_Value : APQ_Integer;

			when Type_APQ_Bigint	=>
				APQ_Bigint_Value : APQ_Bigint;
			
			when Type_APQ_Real	=>
				APQ_Real_Value : APQ_Real;

			when Type_APQ_Double	=>
				APQ_Double_Value : APQ_Double;
				
			when Type_APQ_Serial	=>
				APQ_Serial_Value : APQ_Serial;
				
			when Type_APQ_Bigserial	=>
				APQ_Bigserial_Value : APQ_Bigserial;

			-- time types
			when Type_APQ_Date	=>
				APQ_Date_Value : APQ_Date;

			when Type_APQ_Time	=>
				APQ_Time_Value : APQ_Time;
			
			when Type_APQ_Timestamp	=>
				APQ_Timestamp_Value : APQ_Timestamp;

			when Type_APQ_Timezone	=>
				APQ_Timezone_Value : APQ_Timezone;
			
			-- other types.
			when Type_APQ_Boolean	=>
				APQ_Boolean_Value : APQ_Boolean;

			when Type_APQ_Bitstring	=>
				APQ_Bitstring_Value : APQ_Bitstring_Access;

			when Type_String	=>
				String_Value : Unbounded_String;
		end case;
	end record;



	type Entity is abstract tagged private with null record;
	-- the entity has no record here (for now. ;)).
	

end Aw_Ent;
