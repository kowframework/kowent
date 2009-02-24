-- Main Aw_Ent package.
--
-- Aw_Ent is reponsible for handling persistent data in your application
-- stored in Database backends using the native DB types.
--
-- author Marcelo C. de Freitas <marcelo.batera@gmail.com>
-- createdAt 2008-10-XX
--
-- Repository information:
-- $Date: $
-- $Revision: $
-- $Author: $





--------------
-- Ada 2005 --
--------------
with Ada.Containers.Doubly_Linked_Lists;
---------------
-- Ada Works --
---------------
with Aw_Lib.UString_Vectors;

---------
-- APQ --
---------
with APQ;


package Aw_Ent is

	type Entity_Type is tagged private;
	-- represents a data thats storedn into the database backend

	type ID_Type is private;
	-- represents an identifier of the entity

	No_ID: constant Id_Type;


	-------------------------
	-- Database Management --
	-------------------------
	
	type Connection_Ptr is access all APQ.Root_Connection_Type'Class;

	procedure Set_Connection( Connection: in Connection_Ptr );
	-- set the current database connection
	-- TODO: implement some sort of database pooling
	
	-----------------------
	-- Entity Management --
	-----------------------


	procedure Load( Entity : in out Entity_Type; ID : in ID_Type );
	-- load the entity from the default database Backend

	procedure Load( Entity : in out Entity_Type; ID : in Natural );
	-- load the entity from the default database Backend
	-- it's the same as Load( Entity, To_ID( ID ) );
	
	procedure Store( Entity : in out Entity_Type; Recover_ID: Boolean := True );
	-- save the entity into the database Backend
	-- if it's a new entity, create a new entry and generates an ID for it.
	-- If Recover_ID = TRUE then the ID is then loaded into the in-memory entity
	-- after it has been saved.
	
	-- TODO: implement Narrow
	-- procedure Narrow( From : in Entity_Type'Class; To: out Entity_Type'Class );
	-- narrow an entity to it's parent/child preserving/restoring properties
	-- this is usefull for working with entities that extend a parent entity



	--------------------------
	-- Entity ID Management --
	--------------------------



	type Id_Generator_Type is access function( Entity: in Entity_Type'Class )
		return ID_Type;
	-- The ID generator is used to help Aw_Ent generate IDs.
	-- If the ID returned is equal to the constant No_ID then the task
	-- of id creation is delegated to the database backend.

	function Standard_Id_Generator( Entity: in Entity_Type'Class ) return ID_Type;
	-- return No_ID, thus making Aw_Ent delegate the id creation task to the 
	-- database backend.

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


	-- The work done in queries is managed by Set_Property and Get_Property procedures.
	-- This is another part of Aw_Ent that's 100% object oriented, which means you can
	-- extend Aw_Ent to support your own types.


	type Entity_Property_Type is abstract tagged record
		Name	: Unbounded_String;
		-- the property name directly maps into some field or fields in a table.
		-- this is public for two reasons:
		-- 	1. every entity property has it's own name, which maps to something.
		-- 		* the user could query all entities and their properties for listing
		-- 	2. when extending this package it must be clear the developer MUST use this variable.
		-- 		* Set_property should use this variable
	end record;


	type Entity_Property_Ptr is access all Entity_Property_Type'Class;



	procedure Set_Property(	
				Property: in     Entity_Property_Type;		-- the property worker
				Entity	: in out Entity_Type'Class;		-- the entity
				Q	: in out Query_Type'Class		-- the query from witch to fetch the result
			) is abstract;
	-- Set the property into the Entity.

	procedure Get_Property(
				Property: in     Entity_Property_Type;		-- the property worker
				Entity	: in out Entity_Type'Class;		-- the entity
				Query	: in out Query_Type'Class		-- the query to witch append the value to insert
			) is abstract;
	-- Append into a query being created by the main Aw_ent engine.

	-------------------------
	-- Entity Registration --
	-------------------------


	procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
				Table_Name	: in String;
				Id_Generator	: in Id_Generator_Type := Standard_Id_Generator );
	-- register an Entity into the Aw_Ent engine
	-- Table_Name is the table name to be used.
	
	procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
				Id_Generator	: in Id_Generator_Type := Standard_Id_Generator );
	-- register an Entity into the Aw_Ent engine
	-- Auto generate the table name (using the Tag)


	procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
				Property_Name	: in String;
				Property	: in Property_Access );

private


	My_Connection : Connection_Ptr;


	type ID_Type is record
		Value : APQ.APQ_Bigserial;
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
		-- This is mainly used in the Store and Narrow procedures.
	end record;

	package Property_Lists is new Ada.Containers.Doubly_Linked_Lists(
				Element_Type	=> Entity_Property_Ptr
			);


	type Entity_Type is tagged record
		ID		: ID_Type;
		-- The only thing that comes with the basic entity is an ID.
		-- For consistence sake, every entity has a numeric ID which is used
		-- internally to locate and iterate with entities.


		Table		: Unbounded_String;
		-- Where this entity is to be stored


		Properties	: Property_Lists.List;
		-- The properties of this entity
		-- They are stored in a doubly linked list because of better memory usage than vector
		-- and because we only query this list in a sequential (be it forward or backward) way


		-- Original_Tag	: Ada.Tags.Tag;
		-- this is to be queried internally by Narrow() and Store()
	end record;


	type Property_Entry is tagged record
		Column : Unbounded_String;
		-- the column is usually the name of the property.
		-- it's where the valued is stored in the database backend.
		--
		-- NOTE: the property's name is not set in this record.
		-- it's defined by the Property_Map above.
	end record;

	type Property_Entry_Ptr is access all Property_Entry'Class;

	package Property_Maps is new Ada.Containers.Ordered_Maps(
			Key_Tyope	=> Unbounded_String,
			Element_Type	=> Entity_Property_Ptr
			);



	package UString_Properties is new Generic_Properties(
			Property_Type	=> Unbounded_String
			);
end Aw_Ent;
