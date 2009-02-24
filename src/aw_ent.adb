




with Aw_Lib.UString_Vectors;

package body Aw_Ent is


	-------------------------
	-- Database Management --
	-------------------------
	procedure Set_Connection( Connection : in Connection_Ptr ) is
	begin
		My_Connection := Connection;
	end Set_Connection;


	-----------------------
	-- Entity Management --
	-----------------------

	function To_ID( ID: in Natural ) return ID_Type is
		-- convert a positive into an ID.
		-- used for loading entities.
		My_Id : Id_Type;
	begin
		My_ID.Value := APQ.APQ_Bigserial( ID );
		return My_ID;
	end To_ID;
	
	function To_ID( ID: in Natural; Tag : in Ada.Tags.Tag ) return ID_Type is
		-- convert a positive into an ID.
		-- used for loading entities.
		My_Id : Id_Type;
	begin
		My_ID.Value  := APQ.APQ_Bigserial( ID );
		My_ID.My_Tag := Tag;
		return My_ID;
	end To_ID;



	procedure Append_Column_Names( Query : in out APQ.Root_Query_Type'Class; Entity: Entity_Type ) is
		-- this procedure is used internally to set a column of values in the fashion of:
		-- a,b,c,d
		-- where a, b, c and d are columns of this entity
		First_Property: Boolean := True;

		procedure Set_Column_Names( C: Property_Lists.Cursor ) is
			use Property_Lists;
			Column: String := To_String( Element( C ).all.Column );
		begin
			if not First_Property then
				APQ.Append( Query, "," );
			else
				First_Property := False;
			end if;

			APQ.Append( Column );
		end Set_Column_Names;

	begin
		Property_Elements.Iterate( Entity.Properties, Set_Column_Names'Class );
	end Append_Column_Names;


	procedure Load( Entity : in out Entity_Type; ID : in ID_Type ) is
		-- load the entity from the database Backend
		
		Query : APQ.Root_Query_Type'Class := APQ.New_Query( My_Connection.all );

		procedure Set_Value( C : in Property_Lists.Cursor ) is
		begin
			Set_Property( Element( C ).all, Entity, Query );
		end Set_Value;

	begin

		----------------------
		-- SQL Construction --
		----------------------
		
		APQ.Prepare( Query, "SELECT id," );
		Append_Column_Names( Query, Entity );
		APQ.Append( Query, " WHERE id=" );
		APQ.Append( Query, ID.Value );

		-------------------
		-- SQL Execution --
		-------------------
		
		APQ.Execute( Query, My_Connection.all );
		APQ.Fetch( Query );
		-- we only reach for the first result as there should be only one
		-- if none is found, No_Tuple is raised. :D
		

		---------------------
		-- Data Processing --
		---------------------

		Property_Lists.Iterate( Entity.Properties, Set_Value'Access );

	end Load;

	procedure Load( Entity : in out Entity_Type; ID : in Natural ) is
		-- load the entity from the database Backend
		-- it's the same as Load( Entity, To_ID( ID ) );
	begin
		Entity.ID		:= To_ID( ID );
		Entity.ID.My_Tag	:= Entity'Tag;
	
		Load( Entity, Entity.ID );
	end Load;


	procedure Store( Entity : in out Entity_Type; Recover_ID: Boolean := True ) is
		-- save the entity into the database Backend
		-- if it's a new entity, create a new entry and generates an ID for it.
		-- If Recover_ID = TRUE then the ID is then loaded into the in-memory entity
		-- after it has been saved.

	begin
		if E.ID.My_Tag = No_Tag then
			Insert( E'Tag, Keys, Values, E.ID, Recover_ID );
		else
			Save( E'Tag, Keys, Values, E.ID );
		end if;
		
		-- Construct_SQL( Table_Name( Entity'Tag ), Keys, Values );
	end Store;


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


	----------------------------------
	-- Entity Properties Management --
	----------------------------------

	type Entity_Property_Type is abstract tagged null record;

	type Entity_Property_Ptr is access all Entity_Property_Type'Class;


	function New_String_Property(
				Getter : String_Getter_Type;
				Setter : String_Setter_Type
			) return Entity_Property_Ptr;

	procedure Set_Property(	
				Entity	: in out Entity_Type'Class;		-- the entity
				Property: in     Entity_Property_Type;		-- the property worker
				Field	: in     String;			-- the database field name
				Q	: in     Query_Type'Class		-- the query from witch to fetch the result
			) is abstract;

	procedure Get_Property(
				Entity	: in out Entity_Type'Class;		-- the entity
				Property: in     Entity_Property_Type;		-- the property worker
				Query	: in out Query_Type'Class		-- the query to witch append the value to insert
			) is abstract;


	type String_Property_Type is new Entity_Property_Type with record
		Getter : String_Getter_Type;
		Setter : String_Setter_Type;
	end record;


	



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



	-- TODO:
	-- move this to a generic subpackage
	type String_Property_Setter_Type is access procedure(
			Entity	: in Entity_Type'Class;
			Value	: in String );
	type String_Property_Getter_Type is access function(
			Entity	: in Entity_Type'Class ) return String;

	procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
				Property_Name	: in String;
				Property_Getter	: in String_Property_Getter_Type;
				Property_Setter	: in String_Property_Setter_Type );



end Aw_Ent;
