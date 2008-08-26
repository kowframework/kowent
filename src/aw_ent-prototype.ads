



-- TODO: implement support for multiple database backends at the same time

package Aw_Ent.Prototype is

	type Entity_Type is tagged private;
	-- represents a data thats storedn into the database backend

	type ID_Type is private;
	-- represents a identifier of the entity

	No_ID: constant Id_Type;

	-----------------------
	-- Entity Management --
	-----------------------

	function To_ID( ID: in Natural ) return ID_Type;
	-- convert a positive into a ID.
	-- used for loading entities.

	procedure Load( Entity : in out Entity_Type; ID : in ID_Type );
	-- load the entity from the default database Backend

	procedure Load( Entity : in out Entity_Type; ID : in Natural );
	-- load the entity from the default database Backend
	-- it's the same as Load( Entity, To_ID( ID ) );
	
	procedure Store( Entity : in out Entity_Type; Recover_ID: Boolean := True );
	-- save the entity into the database Backend
	-- if it's a new entity, create a new entry and generates a ID for it.
	-- If Recover_ID = TRUE then the ID is then loaded into the in-memory entity
	-- after it has been saved.
	
	procedure Narrow( From : in Entity_Type'Class; To: out Entity_Type'Class );
	-- narrow an entity to it's parent/child preserving/restoring properties
	-- this is usefull for working with entities that extend a parent entity




	type Id_Generator_Type is access function( Entity: in Entity_Type'Class )
		return ID_Type;
	-- The ID generator is used to help Aw_Ent generate IDs.
	-- If the ID returned is equal to the constant No_ID then the task
	-- of id creation is delegated to the database backend.

	function Standard_Id_Generator( Entity: in Entity_Type'Class ) return ID_Type;
	-- return No_ID, thus making Aw_Ent delegate the id creation task to the 
	-- database backend.

	-------------------------
	-- Entity Registration --
	-------------------------


	procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
				Id_Generator	: in Id_Generator_Type := Standard_Id_Generator );
	-- register an Entity into the Aw_Ent engine



	-- TODO:
	-- move this to a generic subpackage
	type String_Property_Setter_Type is access procedure(
			Entity	: in Entity_Type'Class,
			Value	: in String );
	type String_Property_Getter_Type is access function(
			Entity	: in Entity_Type'Class ) return String;

	procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
				Property_Name	: in String;
				Property_Getter	: in String_Property_Getter_Type;
				Property_Setter	: in String_Property_Setter_Type );

private

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

	type Entity_Type is tagged record
		ID: ID_Type;
		-- The only thing that comes with the basic entity is an ID.
		-- For consistence sake, every entity has a numeric ID which is used
		-- internally to locate and iterate with entities.
	end record;




