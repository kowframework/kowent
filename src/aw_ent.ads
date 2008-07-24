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

with Ada.Strings.Unbounded;	use Ada.Strings.Unbounded;

with Ada.Containers.Ordered_Maps;

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


	type Value is abstract tagged null record;

	type Value_Ptr is access all Value;
	-- this access type is used inside the map.
	-- NOTE: There is no proper memory management implemented so far.
	--       This should be provided by the Value_Ptr type, which
	--      is to be changed to a controlled type soon.




	package Valued_Maps is new Ada.Containers.Ordered_Maps(
			Key_Type => Unbounded_String,
			Element_Type => Value_Ptr );

	procedure Append(
		Map: in out Valued_Maps.Map;
		Key: in String;
		Element: Value_Ptr );
	-- this method is provided to easy the development, as it's not expected
	-- to have the field names parammetrized.


	---------------------------------
	-- The entity interface itself --
	---------------------------------

	type Entity is abstract tagged null record;
	-- Every entity should implement the methods declared here.
	--
	-- Then the user will have authomatically some methods for
	-- data iteration and retrieval.
	--


	function Get_Keys( E: in Entity )
		return Valued_Maps.Map is abstract;
	procedure Set_Keys( E: in out Entity; Values: Valued_Maps.Map );


	function Get_Values( E: in Entity )
		return Valued_Maps.Map is abstract;
	procedure Set_Values( E: in out Entity; Values: Valued_Maps.Map );


	
	function Get_Name( E: in Entity ) return String;
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
	


	

end Aw_Ent;
