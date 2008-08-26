



-- TODO: implement support for multiple database backends at the same time

package Aw_Ent.Prototype is

	type Entity_Type is tagged private;
	-- represents a data thats storedn into the database backend

	type Key_Type is tagged private;
	-- represents a identifier of the entity

	procedure Load( Entity : in out Entity_Type; ID : in Id_Type ); 
	-- load the entity from the default database Backend
	

	procedure Store( Entity : in out Entity_Type; Recover_ID: Boolean := True );
	-- save the entity into the database Backend
	-- if it's a new entity, create a new entry and generates an ID for it.
	-- If Recover_ID = TRUE then the id is then loaded into the in-memory entity
	-- after it has been saved.
	

	procedure Narrow( From : in Entity'Class; To: out Entity'Class );
	-- narrow an entity to it's parent/child preserving/restoring properties
	-- this is usefull for working with entities that extend a parent entity
	

private

