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
with Ada.Containers;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Ordered_Maps;
with Ada.Exceptions;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Tags;					use Ada.Tags;


-------------------
-- KOW Framework --
-------------------
with KOW_Config;
with KOW_Config.Generic_Registry;
with KOW_Lib.Json;
with KOW_Lib.Locales;
with KOW_Lib.UString_Vectors;

---------
-- APQ --
---------
with APQ;
with APQ_Provider;


package KOW_Ent is


	procedure Setup;
	-- looks for the kow_ent configuration file and initializes the provider


	-------------------------
	-- Database Management --
	-------------------------
	

	No_Factory : Exception;
	-- used when trying to produce an entity object with no factory in it's registry


	procedure Set_Connection_Provider( Provider : in APQ_Provider.Connection_Provider_Ptr );
	-- set the current database connection provider

	function Get_Connection_Provider return APQ_Provider.Connection_Provider_Ptr;
	-- get the current database connection provider

	-------------------------
	-- Password Management --
	-------------------------
	function Calculate_Hash( Pwd : in String ) return String;
	-- return a hashed version of Pwd.
	-- Used in both KOW_Ent.Properties and KOW_Ent.Query_Builders for handling password fields

	-------------------
	-- ID Management --
	-------------------


	type ID_Type is record
		Value : APQ.APQ_Bigserial := 1;
		-- It's a APQ_Bigserial value so it can store
		-- arbitrary big indexes.
		--
		-- In memory it's not a big issue, as it's only a really big integer and
		-- not all data is loaded into the RAM at the same time.
		--
		-- The shortcomings come when storing it. You can, yet, use even a smallint
		-- column in the database backend to store this id.
		--
		-- If we've chosen other type we'd forbit the user to have a Bigserial
		-- column for indexing their elements.

		My_Tag : Ada.Tags.Tag := No_Tag;
		-- This is used to track which entity has been used to generate this ID.
		-- This is used by the Store() procedure to determine if it should be saved or inserted
		-- It's set in Load() and Insert() procedures
	end record;

	function "<"( L, R : in ID_Type ) return Boolean;

	function To_String( ID: in Id_Type ) return String;
	-- get the ID value as a String
	

	procedure ID_Append is new APQ.Append_Integer( Val_Type => APQ.APQ_Bigserial );
	-- append an ID value into a query
	
	function ID_Value( Query : APQ.Root_Query_Type'Class; CX : APQ.Column_Index_Type) return APQ.APQ_Bigserial;
	-- get the ID value from a query


	------------------------
	-- Query All Elements --
	------------------------
	
	type ID_Array_Type is Array( Positive range <> ) of ID_Type;
	-- used to list IDs for any entity type
	
	function Get_All_IDs( Entity_Tag : Ada.Tags.Tag ) return ID_Array_Type;
	-- get all IDs from a given entity

	function Get_All_IDs( Entity_Tag : Unbounded_String ) return ID_Array_Type;
	-- get all IDs from a given entity





	-----------------------
	-- Entity Management --
	-----------------------

	type Entity_Type is tagged record
		ID		: ID_Type;
		-- The only thing that comes with the basic entity is an ID.
		-- For consistence sake, every entity has a numeric ID which is used
		-- internally to locate and iterate with entities.


		Filter_Tags	: Unbounded_String;
		-- keywords for searching this entity


		Original_Tag	: Unbounded_String;
		-- the expanded name of the tag of the entity originally created
		-- this is to be queried internally by Narrow() and Store()
	end record;


	function To_String( Entity : in Entity_Type ) return String;
	-- return the string representation for this entity
	-- as default, return the ID as String.
	-- Should be overriden to something that makes more sence

	function Describe( Entity : in Entity_Type ) return String;
	-- get a detailed description of this entity...
	-- as default return "Entity represented by """ & To_String( Entity ) & """"
	
	function Image_URL( Entity : in Entity_Type ) return String;
	-- get a URI (http://, https://, file://) with a image file representing the entity
	-- as default return "", representing there is no graphic representation of the given entity
	--
	-- this is quite usefull in gravatar interaction provided by kow_sec-entities package




	function To_Json_Object( Entity : in Entity_Type ) return KOW_Lib.Json.Object_Type;
	-- convert the given entity into json object
	
	function To_Json_Data (Entity : in Entity_Type ) return KOW_Lib.Json.Json_Data_Type;
	-- convert the given entity into a generic json data (using to_json_object)
	
	function To_Json( Entity : in Entity_Type ) return String;
	-- convert the given entity into json string (using to_json_object);

	-- The following procedures are triggers you can override.

	procedure Will_Insert ( Entity : in out Entity_Type ) is null;
	procedure Was_Inserted( Entity : in out Entity_Type ) is null;
	procedure Will_Update ( Entity : in out Entity_Type ) is null;
	procedure Was_Updated ( Entity : in out Entity_Type ) is null;
	procedure Will_Load   ( Entity : in out Entity_Type ) is null;
	-- this will be called _AFTER_ the entity ID was set
	procedure Was_Loaded  ( Entity : in out Entity_Type ) is null;


	function Is_New( Entity : in Entity_Type ) return Boolean;
	-- check if the entity has been stored or not..
	-- tipically using the entity's id's tag value but can be overriden


	procedure Load( Entity : in out Entity_Type'Class; ID : in ID_Type );
	-- load the entity from the default database Backend

	procedure Load( Entity : in out Entity_Type'Class; ID : in Natural );
	-- load the entity from the default database Backend
	-- it's the same as Load( Entity, To_ID( ID ) );
	
	procedure Store( Entity : in out Entity_Type'Class );
	-- save the entity into the database Backend
	-- if it's a new entity, create a new entry and generates an ID for it (recovering it if needed).
	-- after it has been saved.
	

	function Narrow( Entity : in Entity_Type'Class ) return Entity_Type'Class;
	-- return an entity in it's complete form.
	-- ie, supose the following:
	-- 	type translated_book is new book 
	--
	-- 	store( a_translated_book ) will create a record in the book entity table as well.
	-- 	
	-- 	narrow( a_book ) could return a translated_book entity

	procedure Set_Foreign_Key(
				Entity		: in out Entity_Type'Class;
				Related_Entity	: in     Entity_Type'Class
			);
	-- using the Foreign_Key_property_Type declared in KOW_Ent.Properties,
	-- set the foreign key for the given entity
	
	function Get_Related_IDs(
				Related_To	: in Entity_Type'Class;
				Entity_Tag	: in String
			) return ID_Array_Type;
	-- get the IDs for all related entities...

	----------------------
	-- Entity Extension --
	----------------------

	-- As of 2010-02-22, there is no need to extend the Entity Extension Interface.
	-- Simply extend any KOW_Ent entity and register all levels then you'll be fine.



	-- NOTE :: How the Entity Labels should work ::
	--
	-- They should be stored in an instance of KOW_Config·Generic_Registry
	-- Each configuration file name as in name1.name2 is mapped to the tag name1.name2 (all lower case).
	-- The entity label is set by the key "entity_label". All other labels are the property name. All lower case.


	type Label_Getter is record
		Id	: Unbounded_String;
		Config	: KOW_Config.Config_File;
	end record;

	package Labels is new KOW_Config.Generic_Registry(
					Element_Type	=> Label_Getter,
					Relative_Path	=> "kowent/labels"
				);

	function Get_Label( Getter : in Label_Getter; Locale : in KOW_Lib.Locales.Locale ) return Unbounded_String;
	-- get a label for this getter
	
	function Get_Label( Getter : in Label_Getter; Property : in Unbounded_String; Locale : in KOW_Lib.Locales.Locale ) return Unbounded_String;
	-- get a label for a property in this getter


	


	function Get_Label(
				Entity : in Entity_Type'Class;
				Locale : in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return String;
	-- get the Label for this entity type as string
	
	function Get_Label(
				Entity : in Entity_Type'Class;
				Locale : in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String;
	-- get the Label for this entity type as unbounded_string

	function Get_Label(
				Entity_Tag	: in Ada.Tags.Tag;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String;
	-- get the Label for this entity type as unbounded_string


	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return String;
	-- get the Label for the given property of this entity type as string
		
	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String;
	-- get the Label for given property of this entity type as unbounded_string

	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in Unbounded_String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return String;
	-- get the Label for the given property of this entity type as string

	function Get_Label(
				Entity		: in Entity_Type'Class;
				Property	: in Unbounded_String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String;
	-- get the Label for given property of this entity type as unbounded_string


	function Get_Label(
				Entity_tag	: in Ada.Tags.Tag;
				Property	: in String;
				Locale		: in KOW_Lib.Locales.Locale := KOW_Lib.Locales.Default_Locale
			) return Unbounded_String;

	--------------------------
	-- Entity ID Management --
	--------------------------





	function To_ID( ID: in Natural ) return ID_Type;
	-- convert a positive into an ID.
	-- used for loading entities.
	
	function To_ID( ID: in Natural; Tag : in Ada.Tags.Tag ) return ID_Type;
	-- the same as the previos To_ID, but return an ID initialized
	-- for a specific tag.
	-- This is used by Load() procedure.


	----------------------------------
	-- Entity Properties Management --
	----------------------------------


	type Entity_Property_Metadata_Interface is interface;
	-- used to store custom metadata into the entity property type (such as renderers for web pages)
	type Entity_Property_Metadata_Access is access all Entity_Property_Metadata_Interface'Class;

	type Entity_Property_Metadata_Array is Array( Positive range 1 .. 10 ) of Entity_Property_Metadata_Access;
	-- we support up to 10 metadata in each entity property...

	-- The work done in queries is managed by Set_Property and Get_Property procedures.
	-- This is another part of KOW_Ent that's 100% object oriented, which means you can
	-- extend KOW_Ent to support your own types.

	protected Metadata_Registry is
		-- take care of allocating slots for given metadata types...

		procedure Allocate( Index : out Positive );
		-- allocate the metadata slot returning it's index..
	private
		Next : Positive := 1;
	end Metadata_Registry;

	type Entity_Property_Type is abstract tagged record
		Column_Name	: Unbounded_String;
		-- the property column name directly maps into some field or fields in a table.
		-- this is public for two reasons:
		-- 	1. every entity property has it's own name, which maps to something.
		-- 		* the user could query all entities and their properties for listing
		-- 	2. when extending this package it must be clear the developer MUST use this variable.
		-- 		* Set_property should use this variable
		--
		
		Immutable	: Boolean := False;
		-- when true, the database value won't ever be updated.


		Metadata	: Entity_Property_Metadata_Array := ( others => null );
		-- the metadata for this property
		-- avoid accessing directly. Instead use KOW_Ent.Generic_Property_Metadata package
	end record;


	type Entity_Property_Ptr is access all Entity_Property_Type'Class;



	procedure Set_Property(	
				Property	: in     Entity_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Q		: in out APQ.Root_Query_Type'Class;	-- the query from witch to fetch the result
				Connection	: in out APQ.Root_Connection_Type'Class -- the connection that belongs the query
			) is abstract;
	-- Set the property from the query into the Entity.

	procedure Get_Property(
				Property	: in     Entity_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class;		-- the entity
				Query		: in out APQ.Root_Query_Type'Class;	-- the query to witch append the value to insert
				Connection	: in out APQ.Root_Connection_Type'Class	-- the connection that belongs the query
			) is abstract;
	-- Append into a query being created by the main KOW_ent engine.


	procedure Set_Property(
				Property	: in     Entity_Property_Type;		-- the property worker
				Entity		: in out Entity_Type'Class;		-- the entity
				Value		: in     String				-- the String representation of this value
			) is abstract;
	-- Set the property from a String representation of the value
	
	function Get_Property(
				Property	: in     Entity_Property_Type;		-- the property worker
				Entity		: in     Entity_Type'Class		-- the entity
			) return String is abstract;
	
	function To_Json_Data(
				Property	: in     Entity_Property_Type;
				Entity		: in     Entity_Type'Class
			) return KOW_Lib.Json.Json_Data_Type;
	-- get the property as a json data type

	function Should_Read( Property : in Entity_Property_Type ) return Boolean;
	-- Asks if the value should be set from the database or not
	-- Default :: true
	-- This is here for the Password_Property_Type (that doesn't read from the database)

	function Should_Store( Property : in Entity_Property_Type; Entity : in Entity_Type'Class ) return Boolean;
	-- asks if the property for the given entity should be stored or no
	-- useful to track if the user password has been changed and need to be stored back or not
	-- Default :: true



	----------------------------
	-- SQL Creation Framework --
	----------------------------


	--
	-- Property Related Methods..
	--
	
	procedure  Append_Create_Table( Property : in Entity_Property_Type; Query : in out APQ.Root_Query_Type'Class ) is abstract;
	-- generate the SQL for creating this column in the database for MySQL
	-- TODO :: do it for multiple database backends...


	--
	-- Entity kind related methods...
	--

	procedure Prepare_Create_For_Entity(
				Query	: in out APQ.Root_Query_Type'Class;
				Tag	: in     Ada.Tags.Tag
			);
	-- prepare the query for the given entity

	procedure Prepare_Create_For_Entity(
				Query	: in out APQ.Root_Query_Type'Class;
				Tag	: in     Unbounded_String
			);
	-- prepare the query for the given entity

	

	function Get_Create_For_Entity( Tag : in Ada.Tags.Tag ) return String;
	-- get the SQL string for creating the given entity using APQ Provider

	function Get_Create_For_Entity( Tag : in Unbounded_String ) return String;
	-- get the SQL string for creating the given entity using APQ Provider

	procedure Run_Create_For_Entity( Tag : in Ada.Tags.Tag );
	-- create and run the query for the given entity using APQ Provider


	procedure Run_Create_For_Entity( Tag : in Unbounded_String );
	-- create and run the query for the given entity using APQ Provider

	--
	-- global methods
	--

	function Get_Create( Append_Dump_if_Exists : Boolean := False ) return String;
	-- get the table creation SQL for every entity in the entity registry using APQ Provider

	procedure Run_Create( Dump_If_Exists : Boolean := False );
	-- create the entire DB structure using APQ Provider


	--------------------------------
	-- Generic Packages Instances --
	--------------------------------


	package Property_Lists is new Ada.Containers.Doubly_Linked_Lists(
				Element_Type	=> Entity_Property_Ptr
			);
	-- this is used to map every property in a given entity.

	--
	-- Where the entity is described..
	--
	

	package Tag_Lists is new Ada.Containers.Doubly_Linked_Lists(
				Element_Type	=> Ada.Tags.Tag
			);

	type Factory_Type is access function return Entity_Type'Class;


	type ID_Generator_Type is access function( Entity: in Entity_Type'Class ) return ID_Type;
	type Entity_Information_Type is record
		Entity_Tag	: Ada.Tags.Tag;
		-- just for internal reference (maybe we'll need it at some point?).

		Id_Generator	: Id_Generator_Type;
		-- The ID generator is used to help KOW_Ent generate IDs.
		-- When it's NULL the id generation is task for the database backend

		-- how the id is generated.
		-- if it's null, let the database generate the ID;


		Table_Name	: Unbounded_String;
		-- Where this entity is to be stored

		Unique_Keys	: KOW_Lib.UString_Vectors.Vector;

		Properties	: Property_Lists.List;
		-- The properties of this entity
		-- They are stored in a doubly linked list because of better memory usage than vector
		-- and because we only query this list in a sequential (be it forward or backward) way

		Factory		: Factory_Type;


		-- TODO :: finish the entity extension implementation

		Extension_Of	: Ada.Tags.Tag;
		-- used to track what this entity has extended..

		Extensions	: Tag_Lists.List;
		-- list all the available extensions for this entity
	end record;

	function Hash(Key : Ada.Strings.Unbounded.Unbounded_String ) return Ada.Containers.Hash_Type;
--	function Ada.Strings.Hash (Key : String) return Containers.Hash_Type;


	
	package Entity_Information_Maps is new Ada.Containers.Ordered_Maps(
				Key_Type	=> Unbounded_String,
				Element_Type	=> Entity_Information_Type
			);
	-- this is used to map an entity tag to it's properties
	-- Hashed_Maps could give us better performance, but I've encountered
	-- two strings with the exact same hash already


	package Entity_Information_Lists is new Ada.Containers.Doubly_Linked_Lists(
				Element_Type	=> Entity_Information_Type
			);
	-- this is used internally by the KOW_Ent framework BUT is left here so
	-- it can be reused by your components...



	procedure Set_Values_From_Query(
				Entity		: in out Entity_Type'Class;
				Query		: in out APQ.Root_Query_Type'Class;
				Connection	: in out APQ.Root_Connection_Type'Class;
				Info		: in     Entity_Information_Type
			);
	-- set all the values from the resulting query



	-------------------------
	-- Entity Registration --
	-------------------------



	protected Entity_Registry is
		-- This is task save.
		-- The part that really register the entities.

		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Table_Name	: in String;
					Id_Generator	: Id_Generator_Type := Null;
					Factory		: Factory_Type );
		-- register an Entity into the KOW_Ent engine
		-- Table_Name is the table name to be used.
	
		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Id_Generator	: Id_Generator_Type := Null;
					Factory		: Factory_Type );
		-- register an Entity into the KOW_Ent engine
		-- Auto generate the table name (using the Tag)


		procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
					Property	: in Entity_Property_Ptr;
					Is_Unique	: in Boolean := False );
		-- add another property to this entity

		procedure Replace_Property(
					Entity_Tag	: in Ada.Tags.Tag;
					Property	: in Entity_Property_Ptr
				);
		-- replace an existing property by the column_name and maintaining the unicity parameter

		function Get_Information( Entity_Tag : in Ada.Tags.Tag ) return Entity_Information_Type;
		-- retrieve the entity information by it's tag

		function Get_Information( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Entity_Information_Type;
		-- retrieve the entity information by it's tag's expanded name

		function Get_Properties( Entity_Tag : in Ada.Tags.Tag; Force_All : Boolean := False ) return Property_Lists.List;
		-- retrieve the property list for the given entity;
		-- if Force_All = true then get the properties from all the parents of this entity

		function Get_Properties( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String; Force_All : Boolean := False ) return Property_Lists.List;
		-- retrieve the property list for the given entity;
		-- if Force_All = true then get the properties from all the parents of this entity

		function New_Entity( Entity_Tag : in Ada.Tags.Tag ) return Entity_Type'Class;
		-- produce a new entity
		
		function New_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Entity_Type'Class;
		-- produce a new entity

		
		function Get_Informations_Map return Entity_Information_Maps.Map;

	private
		My_Entities	: Entity_Information_Maps.Map;
	end Entity_Registry;


	--------------------------------
	-- Shortcuts for the Registry --
	--------------------------------
	procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
				Table_Name	: in String;
				Id_Generator	: Id_Generator_Type := Null;
				Factory		: Factory_Type ) renames Entity_Registry.Register;
	-- register an Entity into the KOW_Ent engine
	-- Table_Name is the table name to be used.

	procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
				Id_Generator	: Id_Generator_Type := Null;
				Factory		: Factory_Type ) renames Entity_Registry.Register;
	-- register an Entity into the KOW_Ent engine
	-- Auto generate the table name (using the Tag)


	procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
				Property	: in Entity_Property_Ptr;
				Is_Unique	: in Boolean := False ) renames Entity_Registry.Add_Property;
	-- add another property to this entity
	-- if is_unique is true, add a unique key to this property column_name

	procedure Replace_Property(
				Entity_Tag	: in Ada.Tags.Tag;
				Property	: in Entity_Property_Ptr
			) renames Entity_Registry.Replace_Property;
	-- replace an existing entity *by the column name* maintaining the unicity parameter

	function Get_Information( Entity_Tag : in Ada.Tags.Tag ) return Entity_Information_Type renames Entity_Registry.Get_Information;
	-- retrieve the entity information by it's tag

	function Get_Properties( Entity_Tag : in Ada.Tags.Tag; Force_All : Boolean := False ) return Property_Lists.List renames Entity_Registry.Get_Properties;
	-- retrieve the property list for the given entity;

	function Get_Properties( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String; Force_All : Boolean := False ) return Property_Lists.List renames Entity_Registry.Get_Properties;

	function New_Entity( Entity_Tag : in Ada.Tags.Tag ) return Entity_Type'Class renames Entity_Registry.New_Entity;
	-- creates a new entity object returning it
	-- raises No_Factory if the entity has been created without one

	function New_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Entity_Type'Class renames Entity_Registry.New_Entity;


	function Load_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String; ID : in Id_Type ) return Entity_Type'Class;
	function Load_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String; ID : in Natural ) return Entity_Type'Class;

	function Load_Entity( Entity_Tag : in Ada.Tags.Tag; ID : in Id_Type ) return Entity_Type'Class;
	function Load_Entity( Entity_Tag : in Ada.Tags.Tag; ID : in Natural ) return Entity_Type'Class;

	function Get_Property( Entity_Tag : in Ada.Tags.Tag; Column_Name : in String; Force_All : Boolean := False ) return Entity_Property_Ptr;
	
private



	----------------------------------------------
	-- Auxiliar Functions for Entity Management --
	----------------------------------------------

	procedure Save( Entity : in out Entity_Type'Class );
	-- save the existing entity into the database Backend
	

	procedure Insert( Entity : in out Entity_Type'Class  );
	-- save the entity into the database Backend
	-- if it's a new entity, create a new entry and generates an ID for it.
	-- after it has been saved.

	procedure Append_Column_Names_For_Read(
				Query	: in out APQ.Root_Query_Type'Class;
				Info	: in     Entity_Information_Type;
				Before	: in     String := ""
			); 
	-- this procedure is used internally to set a column of values in the fashion of:
	-- a,b,c,d
	-- where a, b, c and d are columns of this entity
	-- This respect the Should_Read method for the property
	--
	-- insert before before everything if there is something to add

	procedure Append_Column_Names_For_Store( Query : in out APQ.Root_Query_Type'Class; Properties: Property_Lists.List; Entity : in Entity_Type'Class; After: in String := ""  );
	-- this procedure is used internally to set a column of values in the fashion of:
	-- a,b,c,d
	-- where a, b, c and d are columns of this entity
	--
	-- append "after" only if has elements

	------------------------
	-- Type Specification --
	------------------------




	-----------------------
	-- Package Variables --
	-----------------------

	---------------------------
	-- Connection Management --
	---------------------------

	My_Provider : APQ_Provider.Connection_Provider_Ptr;
	


end KOW_Ent;