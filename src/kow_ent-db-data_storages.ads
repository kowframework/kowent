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
with APQ_Provider;
with KOW_Ent.Data_Storages;
with KOW_Ent.Queries;







generic
	type Entity_Type is new KOW_Ent.Entity_Type with private;
	Entity_Alias : String;
package KOW_Ent.DB.Data_Storages is



	type DB_Storage_Type is new KOW_Ent.Data_Storages.Data_Storage_Type with private;



	overriding
	function Get_Alias(
				Data_Entity	: in    DB_Storage_Type;
				Entity_Tag	: in    Ada.Tags.Tag
			) return Entity_Alias_Type;
	-- get the alias for the given entity
	-- for database backend, it's the table name


	overriding
	function Create(
				Data_Storage	: in     DB_Storage_Type;
				Entity_Tag	: in     Ada.Tags.Tag
			) return KOW_Ent.Entity_Type'Class;


	---------------------
	-- Store Procedure --
	---------------------

	overriding
	procedure Store(
				Data_Storage	: in out DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class
			) is null;
	-- store the entity
	-- TODO :: implement-me!



	--------------------
	-- Load Functions --
	--------------------
	
	overriding
	function Load(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     Queries.Query_Type;
				Unique		: in     Boolean := True
			) return KOW_Ent.Entity_Type'Class;
	-- build the query and then return the first result
	-- if unique=true and there are more results, raise UNICITY_ERROR


	------------
	-- Insert --
	------------

	overriding
	procedure Insert(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class
			);
	

	------------
	-- Update --
	------------

	overriding
	procedure Update(
				Data_Storage	: in     DB_Storage_Type;
				Entity		: in out KOW_Ent.Entity_Type'Class;
				Criteria	: in     KOW_Ent.Queries.Logic_Criteria_Type
			);



	overriding
	function New_Loader(
				Data_Storage	: in     DB_Storage_Type;
				Query		: in     KOW_Ent.Queries.Query_Type
			) return KOW_Ent.Data_Storages.Entity_Loader_Interface'Class;

	

	-------------------
	-- Entity Loader --
	-------------------

	type DB_Loader_Type is new KOW_Ent.Data_Storages.Entity_Loader_Interface with private;



	overriding
	procedure Execute( Loader : in out DB_Loader_Type );
	-- execute the query

	overriding
	procedure Fetch( Loader : in out DB_Loader_Type );
	-- fetch the next result


	overriding
	function Has_Element( Loader : in DB_Loader_Type ) return Boolean;
	-- check if there is a next result


	overriding
	procedure Load(
			Loader	: in out DB_Loader_Type;
			Entity	: in out KOW_Ent.Entity_Type'Class
		);
	-- load the current query result into the entity
	-- if the entity type is unknown to the loader interface, raises constraint_error



	overriding
	procedure Flush( Loader : in out DB_Loader_Type );


private


	type DB_Storage_Type is new KOW_Ent.Data_Storages.Data_Storage_Type with null record;

	package Entity_Lists is new Ada.Containers.Doubly_Linked_Lists( Entity_Type );

	type DB_Loader_Type is new KOW_Ent.Data_Storages.Entity_Loader_Interface with record
		Cache	: Entity_Lists.List;
		-- where the results are cached when execute is called as APQ_Provide require
		-- us to fetch all the results at once and then release the connection for good


		Current	: Entity_Lists.Cursor := Entity_Lists.No_Element;

		Query	: KOW_Ent.Queries.Query_Type;
	end record;


	THE_Entity_Alias : Entity_Alias_Type;
	-- where the entity alias is actually stored (it's copied from the Entity_Alias string on elaboration time)


	Storage : aliased DB_Storage_Type;
	-- the actual data storage instance is a singleton object for each entity type (at least for entities in the database backend)
end KOW_Ent.DB.Data_Storages;
