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
-- Main KOW_Ent package.                                                    --
--                                                                          --
-- KOW_Ent is reponsible for handling persistent data in your application   --
-- stored in Database backends using the native DB types.                   --
------------------------------------------------------------------------------



--------------
-- Ada 2005 --
--------------
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Finalization;

-------------------
-- KOW Framework --
-------------------
with APQ;

package KOW_Ent is


	----------------
	-- Data Model --
	----------------

	-- KOW_Ent uses the APQ data model
	-- Each property must have one or more of these


	type Type_Of_Data_Type is (
				-- these values are mapped back to the APQ type with the same name
				APQ_Smallint,
				APQ_Integer,
				APQ_Bigint,
				APQ_Real,
				APQ_Double,
				APQ_Serial,
				APQ_Bigserial,

				APQ_Date,
				APQ_Time,
				APQ_Timestamp,

				Hour_Number,
				Minute_Number,
				Second_Number,

				-- and this one is mapped back to String
				APQ_String
			);


	--------------------
	-- The Value Type --
	--------------------
	type Value_Type(
				Type_Of		: Type_Of_Data_Type;
				String_Length	: Natural 		-- used only when Type_Of = APQ_String
			) is record
		case Type_Of is
			when APQ_Smallint =>
				Smallint_Value	: APQ.APQ_Smallint := APQ.APQ_Smallint'First;

			when APQ_Integer =>
				Integer_Value	: APQ.APQ_Integer;

			when APQ_Bigint =>
				Bigint_Value	: APQ.APQ_Bigint;

			when APQ_Real	=>
				Real_Value	: APQ.APQ_Real;

			when APQ_Double =>
				Double_Value	: APQ.APQ_Double;

			when APQ_Serial	=>
				Serial_Value	: APQ.APQ_Serial;

			when APQ_Bigserial =>
				Bigserial_Value	: APQ.APQ_Bigserial;

			when APQ_Date =>
				Date_Value	: APQ.APQ_Date;

			when APQ_Time =>
				Time_Value	: APQ.APQ_Time;

			when APQ_Timestamp =>
				Timestamp_Value	: APQ.APQ_Timestamp;



			when Hour_Number =>
				Hour_Value	: APQ.Hour_Number;
			
			when Minute_Number =>
				Minute_Value	: APQ.Minute_Number;

			when Second_Number =>
				Second_Value	: APQ.Second_Number;

			when APQ_String =>
				String_Value	: String( 1 .. String_Length );

		end case;
	end record;


	function To_String( Value : in Value_Type ) return String;
	-- convert the value into a not trimmed string
	
	procedure From_String( Value : in out Value_Type; String_Value : in String );
	-- set the value form a string
	-- if the value.type_of=apq_string, raises constraint_error only if string_value'length > value.string_value'length




	----------------------------------
	-- Property Container Interface --
	----------------------------------

	type Property_Container_Type is abstract tagged;
	-- this interface is so the Property_Type has 
	type Property_Container_Ptr is access all Property_Container_Type'Class;


	-----------------------
	-- The Property Type --
	-----------------------


	type Property_Type(
				Name		: access String;	-- the idea here is column name in a table
				Container	: Property_Container_Ptr;
				Value_Of	: Type_Of_Data_Type;
				String_Length	: Natural
			) is abstract new Ada.Finalization.Controlled with record
		-- the property type is abstract so you will have to extend it.
		Value : Value_Type( Value_Of, String_Length );
	end record;

	type Property_Ptr is access all Property_Type'Class;

	overriding
	procedure Initialize( Property : in out Property_Type );
	-- register the given property in the given container

	

	-----------------------
	-- The Property List --
	-----------------------
	package Property_Lists is new Ada.Containers.Doubly_Linked_Lists(
							Element_Type	=> Property_Ptr
						);


	---------------------------------
	-- The Property Container Type --
	---------------------------------

	type Property_Container_Type is abstract tagged record
		-- the property container type is a stucture with
		-- properties.
		--
		-- it's basically an Entity that doesn't know where 
		-- to be stored


		Properties : Property_Lists.List;
	end record;

	procedure Register(
			Container	: in out Property_Container_Type;
			Property	: in     Property_Ptr
		);
	-- register a property into this property container
	-- this is to be called when the property is allocated
	
	procedure Iterate(
			Container	: in out Property_Container_Type;
			Iterator	: access procedure( Property : Property_Ptr )
		);
	-- iterate over all registered properties in this container




--	---------------------------
--	-- The Data Storage Type --
--	---------------------------

--	type Data_Storage_Type is interface;
	-- this is the type that actually handles storing and retrieving data

--	type Data_Storage_Ptr is access all Data_Storage_Type'Class;

--	procedure Read(
--			Data_Storage	: in out Data_Storage_Type;
--			Container	: in out Property_Container_Type'Class;
--			Query		: in     Query_Type
--		) is abstract;
	
--	procedure Write(
--			Data_Storage	: in out Data_Storage_Type;
--			Container	: in     Property_Container_Type'Class
--		) is abstract;


	---------------------
	-- The Entity Type --
	---------------------
	
--	type Entity_Type is new Property_Container_Type with record
--		Data_Storage	: Data_Storage_Ptr;
--	end record;
--
--	procedure Load()

end KOW_Ent;
