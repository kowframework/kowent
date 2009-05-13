------------------------------------------------------------------------------
--                                                                          --
--                          Ada Works :: Entity                             --
--                                                                          --
--                                Ada Works                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--               Copyright (C) 2007-2009, Ada Works Project                 --
--                                                                          --
--                                                                          --
-- Aw_Lib is free library;  you can redistribute it  and/or modify it under --
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



--------------
-- Ada 2005 --
--------------
with Ada.Characters.Handling;
with Ada.Containers;
with Ada.Strings;
with Ada.Strings.Fixed;
with Ada.Strings.Hash;
with Ada.Strings.Unbounded;


---------------
-- Ada Works --
---------------
with Aw_Lib.Log;
with Aw_Lib.UString_Vectors;

package body Aw_Ent is

	
	Logger : Aw_Lib.Log.Logger_Type := 
			Aw_Lib.Log.Get_Logger( "Aw_Ent" );
	

	procedure Log(
			Message : in String;
			Level : Aw_Lib.Log.Log_Level := Aw_lib.Log.Level_Info
		) is
	begin
		Aw_lib.Log.Log(
				Logger	=> Logger,
				Level	=> Level,
				Message	=> Message
			);
	end Log;





	-------------------------
	-- Database Management --
	-------------------------
	procedure Set_Connection( Connection : in Connection_Ptr ) is
	begin
		My_Connection := Connection;
	end Set_Connection;



	function To_String( ID: in Id_Type ) return String is
		-- get the ID value as a String
	begin
		-- TODO: I know there is a better way of doing it but I just can't remember how
		return Ada.Strings.Fixed.Trim( APQ.APQ_Bigserial'Image( ID.Value ), Ada.Strings.Both );
	end To_String;



	-------------------------
	-- Password Management --
	-------------------------
	function Calculate_Hash( Pwd : in String ) return String is
		-- return a hashed version of Pwd.
		-- Used in both Aw_Ent.Properties and Aw_Ent.Query_Builders for handling password fields
	begin
		return Ada.Containers.Hash_Type'Image(
				Ada.Strings.Hash( Pwd )
			);
	end Calculate_Hash;



	-------------------
	-- ID Management --
	-------------------
	function ID_Value( Query : APQ.Root_Query_Type'Class; CX : APQ.Column_Index_Type) return APQ.APQ_Bigserial is
		-- get the ID value from a query
		i : Integer := APQ.Value( Query, CX );
	begin
		return APQ.APQ_Bigserial( i );
	end ID_Value;

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

	procedure Set_Values_From_Query( Entity : in out Entity_Type'Class; Query: in out APQ.Root_Query_Type'Class ) is
		-- set all the values from the resulting query

		Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity'Tag );

		procedure Set_Value( C : in Property_Lists.Cursor ) is
		begin
			Set_Property( Property_Lists.Element( C ).all, Entity, Query, My_Connection );
		end Set_Value;
		TMP_Id : Integer := APQ.Value( Query, APQ.Column_Index( Query, "id" ) );
	begin
		Entity.ID		:= To_ID( TMP_Id );
		Entity.ID.My_Tag	:= Entity'Tag;

		Property_Lists.Iterate( Info.Properties, Set_Value'Access );
	end Set_Values_From_Query;




	procedure Load( Entity : in out Entity_Type'Class; ID : in ID_Type ) is
		-- load the entity from the database Backend
		Query	: APQ.Root_Query_Type'Class := APQ.New_Query( My_Connection.all );
		Info	: Entity_Information_Type := Entity_Registry.Get_Information( Entity'Tag );
	begin

		Entity.ID := ID;
		Entity.ID.My_Tag := Entity'Tag;
		-- and now we set the tag of the entity into it's ID

		----------------------
		-- SQL Construction --
		----------------------
		
		APQ.Prepare( Query, "SELECT id," );
		Append_Column_Names_For_Read( Query, Info.Properties );
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

		Set_Values_From_Query( Entity, Query );

		begin
			loop
				-- fetch any remaining data just in case..
				APQ.Fetch( Query );
			end loop;
		exception
			when others => null;
		end;

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
			Log( "inserting entity" );
			Insert( Entity, Recover_ID );
		else
			Log( "updating entity" );
			Save( Entity );
		end if;
		
		-- Construct_SQL( Table_Name( Entity'Tag ), Keys, Values );
	end Store;





	function Hash(Key : Ada.Strings.Unbounded.Unbounded_String) return Ada.Containers.Hash_Type is
	begin
		return Ada.Strings.Hash (Ada.Strings.Unbounded.To_String( Key ) );
	end Hash;



	function Should_Read( Property : in Entity_Property_Type ) return Boolean is
		-- Asks if the value should be set from the database or not
		-- Default :: true
		-- This is here for the Password_Property_Type (that doesn't read from the database)
	begin
		return True;
	end Should_Read;

	function Should_Store( Property : in Entity_Property_Type; Entity : in Entity_Type'Class ) return Boolean is
		-- asks if the property for the given entity should be stored or no
		-- useful to track if the user password has been changed and need to be stored back or not
		-- Default :: true
	begin
		return True;
	end Should_Store;


	-------------------------
	-- Entity Registration --
	-------------------------


	function To_UString( Str : in String ) return Unbounded_String is
	begin
		return Ada.Strings.Unbounded.To_Unbounded_String(
				Ada.Characters.Handling.To_Lower(
					Ada.Strings.Fixed.Trim(
							Str,
							Ada.Strings.Both
						)
					)
				);
	end To_UString;


	function To_UString( T : in Ada.Tags.Tag ) return Unbounded_String is
		Str : String := Ada.Tags.Expanded_Name( T );
	begin
		return To_UString( Str );
	end To_UString;

	function To_UString( U : in Ada.Strings.Unbounded.Unbounded_String ) return Unbounded_String is
		Str : String := Ada.Strings.Unbounded.To_String( U );
	begin

		return To_UString( Str );
	end To_UString;


	protected body Entity_Registry is
		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Table_Name	: in String;
					Id_Generator	: in Id_Generator_Type := Null;
					Factory		: in Entity_Factory_Type := Null ) is
			-- register an Entity into the Aw_Ent engine
			-- Table_Name is the table name to be used.

			Info	: Entity_Information_Type;
		begin
			Info.Entity_Tag   := Entity_Tag;
			Info.Table_Name	  := To_Unbounded_String( Table_Name );
			Info.Id_Generator := Id_Generator;
			Info.Factory      := Factory;

			Entity_Information_Maps.Insert(
				Container	=> My_Entities,
				Key		=> To_UString( Entity_Tag ),
				New_Item	=> Info
			);
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Dupĺicated entity :: """ & Expanded_Name( Entity_Tag ) & """";

		end Register;
	
		procedure Register(	Entity_Tag	: in Ada.Tags.Tag;
					Id_Generator	: in Id_Generator_Type := Null;
					Factory		: in Entity_Factory_Type := Null ) is
			-- register an Entity into the Aw_Ent engine
			-- Auto generate the table name (using the Tag)
		begin
			Register(
				Entity_Tag	=> Entity_Tag,
				Table_Name	=> Expanded_Name( Entity_Tag ),
				Id_Generator	=> Id_Generator,
				Factory		=> Factory
			);
		end Register;
	
	
		procedure Add_Property( Entity_Tag	: in Ada.Tags.Tag;
					Property	: in Entity_Property_Ptr ) is
			-- add another property to this entity
			Info : Entity_Information_Type;
		begin
			Info := Entity_Information_Maps.Element( My_Entities, To_UString( Entity_Tag ) );
			Property_Lists.Append( Info.Properties, Property );
			-- we add the entity to the list we retrieved.

			Entity_Information_Maps.Include( My_Entities, To_UString( Entity_Tag ), Info );
			-- and now we replace the existing entity registry
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ & Expanded_Name( Entity_Tag ) & """";
		end Add_Property;


		function Get_Information( Entity_Tag : in Ada.Tags.Tag ) return Entity_Information_Type is
		begin
			return Entity_Information_Maps.Element( My_Entities, To_UString( Entity_Tag ) );
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ & Expanded_Name( Entity_Tag ) & """";
		end Get_Information;

		function Get_Information( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Entity_Information_Type is
			-- retrieve the entity information by it's tag's expanded name
		begin
			return Entity_Information_Maps.Element( My_Entities, To_UString( Entity_Tag ) );
		exception
			when Constraint_Error =>
				raise Constraint_Error with "Unknown entity :: """ &  To_String ( Entity_Tag ) & """";
		end Get_Information;

		function Get_Properties( Entity_Tag : in Ada.Tags.Tag ) return Property_Lists.List is
			-- retrieve the property list for the given entity;
		begin
			return Get_Information( Entity_Tag ).Properties;
		end Get_Properties;
		
		function Get_Properties( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Property_Lists.List is
			-- retrieve the property list for the given entity;
		begin
			return Get_Information( Entity_Tag ).Properties;
		end Get_Properties;


		function New_Entity( Entity_Tag : in Ada.Tags.Tag ) return Entity_Type'Class is
			-- produce a new entity
			Info : Entity_information_Type := Get_Information( Entity_Tag );
		begin
			if Info.Factory = Null then
				raise No_Factory with "No factory registered for entity :: """ & Expanded_Name( Entity_Tag ) & """";
			end if;

			return Info.Factory.all;
		end New_Entity;


		function New_Entity( Entity_Tag : in Ada.Strings.Unbounded.Unbounded_String ) return Entity_Type'Class is
			-- produce a new entity
			Info : Entity_information_Type := Get_Information( Entity_Tag );
		begin
			if Info.Factory = Null then
				raise No_Factory with "No factory registered for entity :: """ & Ada.Strings.Unbounded.To_String( Entity_Tag ) & """";
			end if;

			return Info.Factory.all;
		end New_Entity;

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
			if Should_Store( Property.all, Entity ) then
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
			if not Should_Store( Property.all, Entity ) then
				-- if there is nothing to store here, simply ignores this one property
				return;
			end if;

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

		Append_Column_Names_For_Store( Query, Info.Properties, Entity );

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

		Log( "Will run :: " & APQ.To_String( Query ) , Aw_Lib.Log.Level_Debug );
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

	procedure Append_Column_Names_For_Read( Query : in out APQ.Root_Query_Type'Class; Properties: Property_Lists.List ) is
		-- this procedure is used internally to set a column of values in the fashion of:
		-- a,b,c,d
		-- where a, b, c and d are columns of this entity
		-- implementation is at the end of the file

		First_Property: Boolean := True;

		procedure Set_Column_Names( C: Property_Lists.Cursor ) is
			use Property_Lists;
			Column: String := To_String( Element( C ).all.Column_Name );
		begin
			if Should_Read( Element( C ).all ) then
				if not First_Property then
					APQ.Append( Query, "," );
				else
					First_Property := False;
				end if;

				APQ.Append( Query, Column );
			end if;
		end Set_Column_Names;

	begin
		Property_Lists.Iterate( Properties, Set_Column_Names'Access );
	end Append_Column_Names_For_Read;


	procedure Append_Column_Names_For_Store( Query : in out APQ.Root_Query_Type'Class; Properties: Property_Lists.List; Entity : in Entity_Type'Class ) is
		-- this procedure is used internally to set a column of values in the fashion of:
		-- a,b,c,d
		-- where a, b, c and d are columns of this entity
		-- implementation is at the end of the file

		First_Property: Boolean := True;

		procedure Set_Column_Names( C: Property_Lists.Cursor ) is
			use Property_Lists;
			Column: String := To_String( Element( C ).all.Column_Name );
		begin
			if Should_Store( Element( C ).all, Entity ) then
				if not First_Property then
					APQ.Append( Query, "," );
				else
					First_Property := False;
				end if;

				APQ.Append( Query, Column );
			end if;
		end Set_Column_Names;

	begin
		Property_Lists.Iterate( Properties, Set_Column_Names'Access );
	end Append_Column_Names_For_Store;


end Aw_Ent;
