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
with Ada.Strings.Hash;
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with APQ;
with APQ_Provider;
with KOW_Ent.Data_Storages;
with KOW_Ent.Properties;
with KOW_Ent.Queries;
with KOW_Ent.Queries.Logic_Relations;
with KOW_Ent.SQL;






package body KOW_Ent.DB.Data_Storages is



	type Entity_Access is access all Entity_Type;
	procedure Free is new Ada.Unchecked_Deallocation( Name => Entity_Access, Object => Entity_Type );

	------------------------
	-- private procedures --
	------------------------
	procedure Check_Provider( For_Query : in String ) is
		use APQ_Provider;
	begin
		if KOW_Ent.DB.Provider = null then
			raise PROGRAM_ERROR with "provider not set when trying to " & For_Query;
		end if;
	end Check_Provider;




	overriding
	function Get_Alias(
				Data_Entity	: in    DB_Storage_Type;
				Entity_Tag	: in    Ada.Tags.Tag
			) return Entity_Alias_Type is
		-- get the alias for the given entity
		-- for database backend, it's the table name

	begin
		return THE_Entity_Alias;
	end Get_Alias;


	overriding
	function Create(
				Data_Storage	: in     DB_Storage_Type;
				Entity_Tag	: in     Ada.Tags.Tag
			) return KOW_Ent.Entity_Ptr is
		-- this factory creates data with no default value changed or whatsoever;

		Ent : Entity_Access := new Entity_Type;
	begin
		return Entity_Ptr( Ent );
	end Create;


	overriding
	procedure Free(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Ptr
			) is
		use Ada.Tags;
	begin
		if Entity /= null then
			pragma Assert( Entity.all'Tag = Entity_Type'Tag, "I can't free object from a type I don't manage.. sorry" );
			Free( Entity_Access( Entity ) );
		end if;
	end Free;

	--------------------
	-- Load Functions --
	--------------------

	overriding
	procedure Load(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     Queries.Query_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Unique		: in     Boolean := True
			) is 
		-- build the query and then return the first result
		-- if unique=true and there are more results, raise UNICITY_ERROR

		Loader : DB_Loader_Type := DB_Loader_Type( New_Loader( Data_Storage, Query ) );
	begin

		if Unique = True then
			Loader.Query.Limit := 2;
			-- we limit in two so we don't end up fetching lots of unecessary data
			-- offset is not changed though
		else
			Loader.Query.Limit := 1;
			-- make sure we only get the one needed result
		end if;

		Loader.Query.Entity_Tag := Entity_Type'Tag;
		-- we make sure the entity tag is set correctly

		
		Execute( Loader );
		Fetch( Loader );
		Load( Loader, Entity );

		if Unique then
			Fetch( Loader );
			if Has_Element( Loader ) then
				raise KOW_Ent.Data_Storages.UNICITY_ERROR with "At least two results for entity " & Entity_Alias;
			end if;
		end if;
	end Load;




	overriding
	function New_Loader(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     KOW_Ent.Queries.Query_Type'Class
			) return KOW_Ent.Data_Storages.Entity_Loader_Interface'Class is
		Loader : DB_Loader_Type;
	begin
		Loader.Query := KOW_Ent.Queries.Clone( Query );
		Loader.Query.Entity_Tag := Entity_Type'Tag;

		return Loader;
	end New_Loader;

	

	-------------------
	-- Entity Loader --
	-------------------


	overriding
	procedure Adjust( Loader : in out DB_Loader_Type ) is
		use KOW_Ent.Queries;
	begin
		if Loader.Query /= null then
			Loader.Query := Clone( Loader.Query.all );
		end if;
	end Adjust;

	overriding
	procedure Finalize( Loader : in out DB_Loader_Type ) is
		-- make sure we don't leave garbage in the memory
		use KOW_Ent.Queries;
	begin
		if Loader.Query /= null then
			Free( Loader.Query.all, Loader.Query );
		end if;
	end Finalize;

	overriding
	procedure Execute( Loader : in out DB_Loader_Type ) is
		use KOW_Ent.Queries;
		-- execute the query
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query		: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );

			Generator	: SQL.SELECT_Generator_Type;
			Template_Entity	: Entity_Type;
		begin
			SQL.Generate_Select(
					Generator	=> Generator,
					Query		=> Loader.Query.all,
					Connection	=> Connection,
					Q		=> Query
				);

			APQ.Execute_Checked( Query, Connection, "ERROR RUNNING KOW_ENT SELECT QUERY" );

			loop

				APQ.Fetch( Query );
				declare
					Values		: Value_Lists.List;
					Table_Name	: constant String := SQL.Get_Table_Name( Generator );

					procedure Iterator( P : in Property_Ptr ) is
						V : Value_Container_Type;
					begin
						V.Value := new Value_Type'( Get_Value( P.all ) );

						SQL.Load_Value( 
								Value		=> V.Value.all,
								Connection	=> Connection,
								Q		=> Query,
								Column_Name	=> Table_Name & '_' & P.Name.all
							);

						Value_Lists.Append( Values, V );
					end Iterator;
				begin
					Iterate(
							Container	=> Template_Entity,
							Iterator	=> Iterator'Access
						);

					if Loader.Query.all in Queries.Join_Query_Type'Class then
						null;
					end if;
					Entity_Values_Lists.Append( Loader.Cache, Values );
				end;
			end loop;
		exception
			when APQ.No_Tuple =>
				null;
		end Runner;
	begin
		pragma Assert( Loader.Query /= null, "Seems like you are working with a non initialized loader!" );
		if not Entity_Values_Lists.Is_Empty( Loader.Cache ) then
			raise PROGRAM_ERROR with "Trying to execute a query in a not-flushed entity loader";
		end if;

		Check_Provider( "SELECT" );
		APQ_Provider.Run(
					Provider		=> KOW_Ent.DB.Provider.all,
					Connection_Runner	=> Runner'Access,
					Queue_On_OOI		=> True
				);
	end Execute;


	overriding
	procedure Fetch( Loader : in out DB_Loader_Type ) is
		-- fetch the next result
		use Entity_Values_Lists;
	begin
		if Loader.Current = No_Element then
			Loader.Current := First( Loader.Cache );
		else
			Next( Loader.Current );
		end if;
	end Fetch;

	overriding
	function Has_Element( Loader : in DB_Loader_Type ) return Boolean is
		-- check if there is a element fetched
	begin
		return Entity_Values_Lists.Has_Element( Loader.Current );
	end Has_Element;

	overriding
	procedure Load(
			Loader	: in out DB_Loader_Type;
			Entity	: in out KOW_Ent.Entity_Type'Class
		) is
		-- load the current query result into the entity
		-- if the entity type is unknown to the loader interface, raises constraint_error
		use Entity_Values_Lists;


		Values : Value_Lists.List;
		C : Value_Lists.Cursor;

		function Next_Value return Value_Type is
			Val : Value_Type := Value_Lists.Element( C ).Value.all;
		begin
			Value_Lists.Next( C );
			return Val;
		end Next_Value;

		procedure Iterator( P : in Property_Ptr ) is
			-- set the current value
		begin
			Set_Value( P.all, Next_Value );
		end Iterator;

	begin
		if Loader.Current = No_Element then
			raise KOW_Ent.Data_Storages.NO_RESULT with "while trying to load the result";
		end if;

		Values := Element( Loader.Current );


		if not Value_Lists.Is_Empty( Values ) then
			C := Value_Lists.First( Values );
		end if;

		Iterate( Entity, Iterator'Access );
	end Load;



	------------
	-- Insert --
	------------

	overriding
	procedure Insert(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class
			) is
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Generator	: SQL.Insert_Generator_Type;
			Query		: APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
			Id_Property	: Property_Ptr;
		begin
			SQL.Generate_Insert( Generator, Connection, Query, Entity, Id_Property );
			APQ.Execute_Checked( Query, Connection, "ERROR RUNNING KOW_ENT INSERT QUERY" );

			if Id_Property /= null then
				-- set the ID value from DB
				declare
					OID : APQ.Row_Id_Type;
				begin
					OID := APQ.Command_OID( Query );
					Properties.Id_Property( Id_Property.all ).Value.Bigserial_Value := APQ.APQ_Bigserial( OID );
					-- the Id_Property pointer is only set if it's in the id_property type
				end;
			end if;
		end Runner;
	begin
		Check_Provider( "INSERT" );
		APQ_Provider.Run(
					Provider		=> KOW_Ent.DB.Provider.all,
					Connection_Runner	=> Runner'Access,
					Queue_On_OOI		=> True
				);
		Entity.Data_Storage := Data_Storage'Unrestricted_Access;
	end Insert;
	

	------------
	-- Update --
	------------

	overriding
	procedure Update(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Criteria	: in     KOW_Ent.Queries.Logic_Criteria_Type
			) is

		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Generator : SQL.Update_Generator_Type;
			Query : APQ.Root_Query_Type'Class := APQ.New_Query( Connection );
		begin
			SQL.Generate_Update( Generator, Connection, Query, Entity, Criteria );
			APQ.Execute_Checked( Query, Connection, "ERROR RUNNING KOW_ENT UPDATE QUERY" );
		end Runner;
	begin
		Check_Provider( "UPDATE" );
		APQ_Provider.Run(
					Provider		=> KOW_Ent.DB.Provider.all,
					Connection_Runner	=> Runner'Access,
					Queue_On_OOI		=> True
				);
		Entity.Data_Storage := Data_Storage'Unrestricted_Access;
	end Update;



	overriding
	procedure Flush( Loader : in out DB_Loader_Type ) is
		use Entity_Values_Lists;
	begin
		Loader.Current := No_Element;
		Clear( Loader.Cache );
	end Flush;

-- private

	overriding
	procedure Adjust( V : in out Value_Container_Type ) is
	begin
		if V.Value /= null then
			V.Value := new Value_Type'( V.Value.all );
		end if;
	end Adjust;


	overriding
	procedure Finalize( V: in out Value_Container_Type ) is
	begin
		if V.Value /= null then
			KOW_Ent.Free( V.Value );
		end if;
	end Finalize;
	

	function Hash( Tag : in Ada.Tags.Tag ) return Ada.Containers.Hash_Type is
	begin
		return Ada.Strings.Hash( Ada.Tags.Expanded_Name( Tag ) );
	end Hash;

begin
	THE_Entity_Alias := To_Alias( Entity_Alias );
	KOW_Ent.Data_Storages.Register_Entity(
					Entity_Tag	=> Entity_Type'Tag,
					Data_Storage	=> Storage'Unrestricted_Access
				);
end KOW_Ent.DB.Data_Storages;
