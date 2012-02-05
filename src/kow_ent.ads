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
with Ada.Tags;
with Ada.Unchecked_Deallocation;

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
				APQ_Boolean,

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
				-- TODO :: adicionar APQ_Boolean
			);


	--------------------
	-- The Value Type --
	--------------------
	type Value_Type(
				Type_Of		: Type_Of_Data_Type;
				String_Length	: Natural 		-- used only when Type_Of = APQ_String
			) is record

		Is_Null : Boolean := False;
		-- the null state of the variable can be set in two ways:
		-- 	1. manually (ie, the user sets using Value.Is_Null := true)
		-- 	2. from the database backend.
		--
		-- remember your property type must take this into account.

		case Type_Of is
			when APQ_Boolean =>
				Boolean_Value	: APQ.APQ_Boolean := False;

			when APQ_Smallint =>
				Smallint_Value	: APQ.APQ_Smallint := APQ.APQ_Smallint'First;

			when APQ_Integer =>
				Integer_Value	: APQ.APQ_Integer := APQ.APQ_Integer'First;

			when APQ_Bigint =>
				Bigint_Value	: APQ.APQ_Bigint := APQ.APQ_Bigint'First;

			when APQ_Real	=>
				Real_Value	: APQ.APQ_Real := APQ.APQ_Real'First;

			when APQ_Double =>
				Double_Value	: APQ.APQ_Double := APQ.APQ_Double'First;

			when APQ_Serial	=>
				Serial_Value	: APQ.APQ_Serial := APQ.APQ_Serial'First;

			when APQ_Bigserial =>
				Bigserial_Value	: APQ.APQ_Bigserial := APQ.APQ_Bigserial'First;


			when APQ_Date => -- TODO :: initialize the date, time and timestamp values
				Date_Value	: APQ.APQ_Date;

			when APQ_Time =>
				Time_Value	: APQ.APQ_Time;

			when APQ_Timestamp =>
				Timestamp_Value	: APQ.APQ_Timestamp;



			when Hour_Number =>
				Hour_Value	: APQ.Hour_Number := APQ.Hour_Number'First;
			
			when Minute_Number =>
				Minute_Value	: APQ.Minute_Number := APQ.Minute_Number'First;

			when Second_Number =>
				Second_Value	: APQ.Second_Number := APQ.Second_Number'First;

			when APQ_String =>
				String_Value	: String( 1 .. String_Length ) := ( others => ' ' );
		end case;
	end record;


	type Value_Ptr is access all Value_Type;


	function To_String( Value : in Value_Type ) return String;
	-- convert the value into a trimmed string
	
	procedure From_String( Value : in out Value_Type; String_Value : in String );
	-- set the value form a string
	-- if the value.type_of=apq_string, raises constraint_error only if string_value'length > value.string_value'length


	procedure Free is new Ada.Unchecked_Deallocation(
					Object	=> Value_Type,
					Name	=> Value_Ptr
				);


	----------------------------------
	-- Property Container Interface --
	----------------------------------

	type Property_Container_Type is abstract tagged limited private;
	-- the property container type is limited so the relation that the property initialization
	-- is sane.
	--
	-- but a clone method is available for entities of the same type

	type Property_Container_Ptr is access all Property_Container_Type'Class;

	procedure Clone(
			From	: in     Property_Container_Type;
			To	: in out Property_Container_Type'Class
		);
	-- clone the data from [From] to [To]
	-- the cloning procedure copy only values from registered properties

	-----------------------
	-- The Property Type --
	-----------------------


	type Property_Name_Type is access String;
	type Property_Name_Array is Array( Positive range <> ) of Property_Name_Type;
	function PN( Name : in String ) return Property_Name_Type;
	-- alias to return new String'(Name);


	type Property_Type(
				Name		: Property_Name_Type;	-- the idea here is column name in a table
				Container	: Property_Container_Ptr;
				Allow_Null	: Boolean
			) is abstract new Ada.Finalization.Controlled with null record;

	type Property_Ptr is access all Property_Type'Class;

	overriding
	procedure Initialize( Property : in out Property_Type );
	-- register the given property in the given container

	

	function Get_Type ( Property : in Property_Type ) return Type_Of_Data_Type is abstract;
	-- return the type of the value; used for calling set_valued in the right way

	function Get_Value(
				Property 	: in Property_Type;
				For_Store	: in Boolean := False
			) return Value_Type is abstract;
	-- get the value
	
	procedure Set_Value(
				Property	: in out Property_Type;
				Value		: in     Value_Type
			) is abstract;
	-- set the value, respecting results of get_type



	function Ignore_For_Insert( Property : in Property_Type ) return Boolean;
	-- should the framework ignore this property in insert queries?
	-- default: false

	function Ignore_For_Update( Porperty : in Property_Type ) return Boolean;
	-- should the framework ignore this property in update queries?
	-- default: false


	function Is_Id( Property : in Property_Type ) return Boolean;
	-- if true, when updating use this to index the value
	-- used only by the get_id(entity) function


	-----------------------
	-- The Property List --
	-----------------------
	package Property_Lists is new Ada.Containers.Doubly_Linked_Lists(
							Element_Type	=> Property_Ptr
						);


	---------------------------------
	-- The Property Container Type --
	---------------------------------



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





	

	---------------------------
	-- The Data Storage Type --
	---------------------------

	type Data_Storage_Interface is interface;
	-- this is the type that actually handles storing and retrieving data
	-- for the complete definition of this type please see also
	-- 	KOW_Ent.Data_Storages
	type Data_Storage_Ptr is access all Data_Storage_Interface'Class;


	procedure Install(
				Data_Storage	: in out Data_Storage_Interface
			) is abstract;
	-- create all the required files for the data storage to work
	-- for DB storages, it means create the table structure for each one of the entities
	--
	-- also, calls the Post_Install method for each template entity used

	procedure Create_Index(
				Data_Storage	: in out Data_Storage_Interface;
				Entity_Tag	: in     Ada.Tags.Tag;
				Property_Names	: in     Property_Name_Array;
				Is_Unique	: in     Boolean
			) is abstract;
	-- create a index with the given properties for the given entity_tag
	-- is_unique define if each index entry should be unique or not


	function Type_Of(
				Data_Storage	: in     Data_Storage_Interface
			) return String is abstract;
	-- return a string of up to 50 characters describing this data source


	function Version_Of(
				Data_Storage	: in     Data_Storage_Interface
			) return String is abstract;
	-- return the version number of the current data storage

	---------------------------
	-- The Entity Alias Type --
	---------------------------

	type Entity_Alias_Type is new String( 1 .. 2**8 );
	-- this type is used all over the framework to identify the name used when storing an entity
	
	function To_Alias( Alias_String : in String ) return Entity_Alias_Type;
	-- convenient way for converting an alias string (any length) into an entity alias string
	
	function Trim( Alias : in Entity_Alias_Type ) return String;
	-- convenient way for returning the trimmed version of the alias


	---------------------
	-- The Entity Type --
	---------------------


	type Entity_Type is abstract new Property_Container_Type with private;
	type Entity_Ptr is access all Entity_Type'Class;


	function Version_Of( Entity : in Entity_Type ) return String;
	-- this is used to identify the current installed version.
	-- when -1 (default behavior), it means the entity is not versioned.
	-- NOTE :: the complete versioning framework isn't really implemented.
	-- it's just a place holder

	procedure Store( Entity : in out Entity_Type );
	-- procedure used to store the entity;
	-- for retrieving please read the documentation of the used data storage


	function Get_Alias( Entity : in Entity_Type ) return Entity_Alias_Type;
	-- get the alias from the data_storage



	function Get_id( Entity : in Entity_Type ) return Property_Type'Class;
	-- get the ID property
	-- the default implementation look for elements where Is_Id( Property ) = true
	-- if none found, raises constraint error with an informative message


	procedure Post_Install(
				Entity		: in out Entity_Type;
				Data_Storage	: in out Data_Storage_Interface'Class
			) is null;
	-- called by the install procedure after it has created the storage schema
	-- for this entity
	

	
private

	type Property_Container_Type is abstract tagged limited record
		-- the property container type is a stucture with
		-- properties.
		--
		-- it's basically an Entity that doesn't know where 
		-- to be stored


		Properties : Property_Lists.List;
	end record;

	type Entity_Type is abstract new Property_Container_Type with record
		-- conceptually the entity is a property container which
		-- is linked to a data storage backend, thus being able to
		-- be stored and retrieved later on
		--
		--
		-- the data storage is an abstract concept described later
		Data_Storage : Data_Storage_Ptr;
	end record;


end KOW_Ent;
