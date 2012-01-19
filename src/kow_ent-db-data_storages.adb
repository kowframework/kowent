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
with Ada.Unchecked_Deallocation;

-------------------
-- KOW Framework --
-------------------
with APQ;
with APQ_Provider;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;
with KOW_Ent.SQL;






package body KOW_Ent.DB.Data_Storages is



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

	--------------------
	-- Load Functions --
	--------------------

	overriding
	function Load(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     Queries.Query_Type;
				Unique		: in     Boolean := True
			) return KOW_Ent.Entity_Type'Class is
		-- build the query and then return the first result
		-- if unique=true and there are more results, raise UNICITY_ERROR

		Entity : Entity_Type;
		Loader : DB_Loader_Type;
	begin
		Loader.Query := Query;

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

		return Entity;
	end Load;




	overriding
	function New_Loader(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     KOW_Ent.Queries.Query_Type
			) return KOW_Ent.Data_Storages.Entity_Loader_Interface'Class is
		Loader : DB_Loader_Type;
	begin
		Loader.Query := Query;
		Loader.Query.Entity_Tag := Entity_Type'Tag;

		return Loader;
	end New_Loader;

	

	-------------------
	-- Entity Loader --
	-------------------



	overriding
	procedure Execute( Loader : in out DB_Loader_Type ) is
		-- execute the query
		procedure Runner( Connection : in out APQ.Root_Connection_Type'Class ) is
			Query : APQ.Root_Query_Type'Class := APQ.New_Query( Connection );

			Generator : SQL.SELECT_Generator_Type;
		begin
			SQL.Generate_Select(
					Generator	=> Generator,
					Query		=> Loader.Query,
					Connection	=> Connection,
					Q		=> Query
				);

			APQ.Execute_Checked( Query, Connection, "ERROR RUNNING KOW_ENT SELECT QUERY" );

			loop
				APQ.Fetch( Query );
				declare
					E : Entity_Type;
				begin
					-- TODO :: set all the data in the entity type...
					Entity_Lists.Append( Loader.Cache, E );
				end;
			end loop;
		exception
			when APQ.No_Tuple =>
				null;
		end Runner;
	begin
		if not Entity_Lists.Is_Empty( Loader.Cache ) then
			raise PROGRAM_ERROR with "Trying to execute a query in a not-flushed entity loader";
		end if;

		APQ_Provider.Run(
					Provider		=> KOW_Ent.DB.Provider.all,
					Connection_Runner	=> Runner'Access,
					Queue_On_OOI		=> True
				);
	end Execute;


	overriding
	procedure Fetch( Loader : in out DB_Loader_Type ) is
		-- fetch the next result
		use Entity_Lists;
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
		return Entity_Lists.Has_Element( Loader.Current );
	end Has_Element;

	overriding
	procedure Load(
			Loader	: in out DB_Loader_Type;
			Entity	: in out KOW_Ent.Entity_Type'Class
		) is
		-- load the current query result into the entity
		-- if the entity type is unknown to the loader interface, raises constraint_error
		use Entity_Lists;
	begin
		if Loader.Current = No_Element then
			raise KOW_Ent.Data_Storages.NO_RESULT with "while trying to load the result";
		end if;

		Entity := KOW_Ent.Entity_Type'Class( Element( Loader.Current ) );
	end Load;


	overriding
	procedure Flush( Loader : in out DB_Loader_Type ) is
		use Entity_Lists;
	begin
		Loader.Current := No_Element;
		Clear( Loader.Cache );
	end Flush;


begin
	THE_Entity_Alias := To_Alias( Entity_Alias );
	KOW_Ent.Data_Storages.Register_Entity(
					Entity_Tag	=> Entity_Type'Tag,
					Data_Storage	=> Storage'Access
				);
end KOW_Ent.DB.Data_Storages;
