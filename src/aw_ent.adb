


with Ada.Strings.Hash;

with Aw_Lib.UString_Vectors;

package body Aw_Ent is

	
	procedure ID_Append is new APQ.Append_Integer( Val_Type => APQ.APQ_Bigserial );


	procedure Append_Column_Names( Query : in out APQ.Root_Query_Type'Class; Properties: Property_Lists.List); 
		-- this procedure is used internally to set a column of values in the fashion of:
		-- a,b,c,d
		-- where a, b, c and d are columns of this entity
		-- implementation is at the end of the file


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





	procedure Load( Entity : in out Entity_Type'Class; ID : in ID_Type ) is
		-- load the entity from the database Backend
		
		Query	: APQ.Root_Query_Type'Class := APQ.New_Query( My_Connection.all );
		Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity'Tag );

		procedure Set_Value( C : in Property_Lists.Cursor ) is
		begin
			Set_Property( Property_Lists.Element( C ).all, Entity, Query, My_Connection );
		end Set_Value;

	begin

		Entity.ID := ID;
		Entity.ID.My_Tag := Entity'Tag;
		-- and now we set the tag of the entity into it's ID

		----------------------
		-- SQL Construction --
		----------------------
		
		APQ.Prepare( Query, "SELECT id," );
		Append_Column_Names( Query, Info.Properties );
		APQ.Append( Query, " FROM " & To_String( Info.Table_Name ) );
		APQ.Append( Query, " WHERE id=" );
		ID_Append( Query, Entity.ID.Value );

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

		Property_Lists.Iterate( Info.Properties, Set_Value'Access );

	end Load;

	procedure Load( Entity : in out Entity_Type'Class; ID : in Natural ) is
		-- load the entity from the database Backend
		-- it's the same as Load( Entity, To_ID( ID ) );
	begin
		Entity.ID		:= To_ID( ID );
		Entity.ID.My_Tag	:= Entity'Tag;
	
		Load( Entity, Entity.ID );
	end Load;


	procedure Store( Entity : in out Entity_Type'Class; Recover_ID: Boolean := True ) is
		-- save the entity into the database Backend
		-- if it's a new entity, create a new entry and generates an ID for it.
		-- If Recover_ID = TRUE then the ID is then loaded into the in-memory entity
		-- after it has been saved.

	begin
		if Entity.ID.My_Tag = No_Tag then
			Insert( Entity, Recover_ID );
		else
			Save( Entity );
		end if;
		
		-- Construct_SQL( Table_Name( Entity'Tag ), Keys, Values );
	end Store;





	function Hash(Key : Ada.Tags.Tag) return Ada.Containers.Hash_Type is
	begin
		return Ada.Strings.Hash (Expanded_Name( Key ) );
	end Hash;

	-------------------------
	-- Entity Registration --
	-------------------------


	protected body Entity_Registry is
		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Table_Name	: in String;
					Id_Generator	: in Id_Generator_Type := Null ) is
			-- register an Entity into the Aw_Ent engine
			-- Table_Name is the table name to be used.

			Info	: Entity_Information_Type;
		begin
			Info.Entity_Tag   := Entity_Tag;
			Info.Table_Name	  := To_Unbounded_String( Table_Name );
			Info.Id_Generator := Id_Generator;

			Entity_Information_Maps.Insert(
				Container	=> My_Entities,
				Key		=> Entity_Tag,
				New_Item	=> Info
			);
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Dupĺicated entity :: """ & Expanded_Name( Entity_Tag ) & """";

		end Register;
	
		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Id_Generator	: in Id_Generator_Type := Null ) is
			-- register an Entity into the Aw_Ent engine
			-- Auto generate the table name (using the Tag)
		begin
			Register(
				Entity_Tag	=> Entity_Tag,
				Table_Name	=> Expanded_Name( Entity_Tag ),
				Id_Generator	=> Id_Generator
			);
		end Register;
	
	
		procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
					Property	: in Entity_Property_Ptr ) is
			-- add another property to this entity
			Info : Entity_Information_Type;
		begin
			Info := Entity_Information_Maps.Element( My_Entities, Entity_Tag );
			Property_Lists.Append( Info.Properties, Property );
			-- we add the entity to the list we retrieved.

			Entity_Information_Maps.Include( My_Entities, Entity_Tag, Info );
			-- and now we replace the existing entity registry
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ & Expanded_Name( Entity_Tag ) & """";
		end Add_Property;


		function Get_Information( Entity_Tag : in Ada.Tags.Tag ) return Entity_Information_Type is
		begin
			return Entity_Information_Maps.Element( My_Entities, Entity_Tag );
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ & Expanded_Name( Entity_Tag ) & """";
		end Get_Information;


		function Get_Properties( Entity_Tag : in Ada.Tags.Tag ) return Property_Lists.List is
			-- retrieve the property list for the given entity;
		begin
			return Get_Information( Entity_Tag ).Properties;
		end Get_Properties;

	end Entity_Registry;



-- private ::

	----------------------------------------------
	-- Auxiliar Functions for Entity Management --
	----------------------------------------------

	procedure Save( Entity : in out Entity_Type'Class ) is
		-- save the existing entity into the database Backend
		Query	: APQ.Root_Query_Type'Class := APQ.New_Query( My_Connection.all );
		Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity'Tag );


		First_Element	: Boolean := True;
		procedure Update_Appender( C : Property_Lists.Cursor ) is
			Property: Entity_Property_Ptr := Property_Lists.Element( C );
		begin
			if not First_Element then
				APQ.Append( Query, "," );
			else
				First_Element := False;
			end if;


			if Property /= null then
				APQ.Append(
					Query,
					To_String( Property.all.Column_Name ) & "="
				);
				

				Get_Property( Property.all, Entity, Query, My_Connection );
			else
				raise Constraint_Error with "Some null property exists in """& Expanded_Name( Entity'Tag ) & """";
			end if;
		end Update_Appender;
				
			

	begin
		APQ.Prepare(
			Query,
			"UPDATE " &
				To_String( Info.Table_Name ) &
				" SET "
		);

		Property_Lists.Iterate( Info.Properties, Update_Appender'Access );


		APQ.Append(
			Query,
			" WHERE id="
		);

		ID_Append(
			Query,
			Entity.ID.Value
		);

		APQ.Execute(
			Query,
			My_Connection.all
		);
	end Save;
	

	procedure Insert( Entity : in out Entity_Type'Class; Recover_ID: Boolean := True ) is
		-- save the entity into the database Backend
		-- if it's a new entity, create a new entry and generates an ID for it.
		-- If Recover_ID = TRUE then the ID is then loaded into the in-memory entity
		-- after it has been saved
		ID	: Id_Type;
		Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity'Tag );
	
		Query	: APQ.Root_Query_Type'Class := APQ.New_Query( My_Connection.all );

		First_Element : Boolean := True;

		procedure Insert_Appender( C: in Property_Lists.Cursor ) is
			Property: Entity_Property_Ptr := Property_Lists.Element( C );
		begin
			if not First_Element then
				APQ.Append( Query, "," );
			else
				First_Element := False;
			end if;
			if Property /= null then
				Get_Property( Property.all, Entity, Query, My_Connection );
			else
				raise Constraint_Error with "Some property is null at """ & Expanded_Name( Entity'Tag ) & """";
			end if;
		end Insert_Appender;

	begin

		---------------
		-- SQL Setup --
		---------------
		
		APQ.Prepare(
			Query,
			"INSERT INTO " & To_String( Info.Table_Name ) & "("
		);

		Append_Column_Names( Query, Info.Properties );

		if Info.Id_Generator /= NULL then
			APQ.Append(
				Query,
				",id"
			);
		end if;

		APQ.Append( Query, ") VALUES(" );

		Property_Lists.Iterate( Info.Properties, Insert_Appender'Access );

		if Info.Id_Generator /= NULL then
			ID := Info.Id_Generator.all( Entity );
			APQ.Append(
				Query,
				","
			);
			ID_Append(
				Query,
				ID.Value
			);
		end if;

		APQ.Append( Query, ")" );

		-------------------
		-- SQL Execution --
		-------------------

		APQ.Execute( Query, My_Connection.all );

		if Recover_ID then
			if Info.Id_Generator /= NULL then
				Entity.ID := ID;
			else
				declare
					OID : APQ.Row_Id_Type;
				begin
					OID := APQ.Command_OID( Query );
					Entity.ID := To_Id( Natural( OID ) );
				end;
			end if;
		end if;
	end Insert;


	------------------------------
	-- Other Auxiliar Functions --
	------------------------------

	procedure Append_Column_Names( Query : in out APQ.Root_Query_Type'Class; Properties: Property_Lists.List ) is
		-- this procedure is used internally to set a column of values in the fashion of:
		-- a,b,c,d
		-- where a, b, c and d are columns of this entity
		-- implementation is at the end of the file

		First_Property: Boolean := True;

		procedure Set_Column_Names( C: Property_Lists.Cursor ) is
			use Property_Lists;
			Column: String := To_String( Element( C ).all.Column_Name );
		begin
			if not First_Property then
				APQ.Append( Query, "," );
			else
				First_Property := False;
			end if;

			APQ.Append( Query, Column );
		end Set_Column_Names;

	begin
		Property_Lists.Iterate( Properties, Set_Column_Names'Access );
	end Append_Column_Names;


end Aw_Ent;